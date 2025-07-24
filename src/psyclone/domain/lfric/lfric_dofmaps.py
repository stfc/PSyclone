# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

'''
This module contains the LFRicDofmaps class which holds all the
information and methods for the dofmaps required by an invoke such as:
generating the calls to the LFRic infrastructure that look-up the necessary
dofmaps; declaring all unique function space dofmaps in the PSy layer as
pointers to integer arrays; and adding dofmap-related declarations to a
Kernel stub.

LFRicDofmaps is used in the LFRicInvoke module.
'''

from collections import OrderedDict

from psyclone import psyGen
from psyclone.domain.lfric import LFRicCollection, LFRicTypes, LFRicConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import Assignment, Reference, StructureReference
from psyclone.psyir.symbols import (
    UnsupportedFortranType, DataSymbol, ArgumentInterface, ArrayType)


class LFRicDofmaps(LFRicCollection):
    '''
    Holds all information on the dofmaps (including column-banded and
    indirection) required by an invoke.

    :param node: Kernel or Invoke for which to manage dofmaps.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.domain.lfric.LFRicInvoke`

    '''
    def __init__(self, node):
        # pylint: disable=too-many-branches
        super().__init__(node)

        # Look at every kernel call in this invoke and generate a list
        # of the unique function spaces involved.
        # We create a dictionary whose keys are the map names and entries
        # are the corresponding field objects.
        self._unique_fs_maps = OrderedDict()
        # We also create a dictionary of column-banded dofmaps. Entries
        # in this one are themselves dictionaries containing two entries:
        # "argument" - the object holding information on the CMA kernel
        #              argument
        # "direction" - whether the dofmap is required for the "to" or
        #               "from" function space of the operator.
        self._unique_cbanded_maps = OrderedDict()
        # A dictionary of required CMA indirection dofmaps. As with the
        # column-banded dofmaps, each entry is itself a dictionary with
        # "argument" and "direction" entries.
        self._unique_indirection_maps = OrderedDict()

        for call in self.kernel_calls:
            # We only need a dofmap if the kernel operates on cells
            # rather than dofs.
            if call.iterates_over != "dof":
                for unique_fs in call.arguments.unique_fss:
                    # We only need a dofmap if there is a *field* on this
                    # function space. If there is then we use it to look
                    # up the dofmap.
                    fld_arg = unique_fs.field_on_space(call.arguments)
                    if fld_arg:
                        map_name = unique_fs.map_name
                        if map_name not in self._unique_fs_maps:
                            self._unique_fs_maps[map_name] = fld_arg
                if call.cma_operation == "assembly":
                    # A kernel that assembles a CMA operator requires
                    # column-banded dofmaps for its 'to' and 'from'
                    # function spaces
                    cma_args = psyGen.args_filter(
                        call.arguments.args,
                        arg_types=["gh_columnwise_operator"])

                    # Sanity check - we expect only one CMA argument
                    if len(cma_args) != 1:
                        raise GenerationError(
                            f"Internal error: there should only be one CMA "
                            f"operator argument for a CMA assembly kernel but "
                            f"found {len(cma_args)}")

                    map_name = \
                        cma_args[0].function_space_to.cbanded_map_name
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "to"}
                    map_name = \
                        cma_args[0].function_space_from.cbanded_map_name
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "from"}
                elif call.cma_operation == "apply":
                    # A kernel that applies (or applies the inverse of) a
                    # CMA operator requires the indirection dofmaps for the
                    # to- and from-spaces of the operator.
                    cma_args = psyGen.args_filter(
                        call.arguments.args,
                        arg_types=["gh_columnwise_operator"])

                    # Sanity check - we expect only one CMA argument
                    if len(cma_args) != 1:
                        raise GenerationError(
                            f"Internal error: there should only be one CMA "
                            f"operator argument for a kernel that applies a "
                            f"CMA operator but found {len(cma_args)}")

                    map_name = cma_args[0].function_space_to\
                        .cma_indirection_map_name
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "to"}
                    map_name = cma_args[0].function_space_from\
                        .cma_indirection_map_name
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "from"}

    def initialise(self, cursor: int) -> int:
        '''
        Add code to initialise the entities being managed by this class.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        '''
        first = True
        for dmap, field in self._unique_fs_maps.items():
            stmt = Assignment.create(
                    lhs=Reference(self.symtab.lookup(dmap)),
                    rhs=field.generate_method_call("get_whole_dofmap"),
                    is_pointer=True)
            if first:
                stmt.preceding_comment = (
                    "Look-up dofmaps for each function space")
                first = False
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        first = True
        for dmap, cma in self._unique_cbanded_maps.items():
            stmt = Assignment.create(
                    lhs=Reference(self.symtab.lookup(dmap)),
                    rhs=StructureReference.create(
                         self._invoke.schedule.symbol_table.lookup(
                            cma["argument"].proxy_name),
                         [f"column_banded_dofmap_{cma['direction']}"]),
                    is_pointer=True)
            if first:
                stmt.preceding_comment = (
                    "Look-up required column-banded dofmaps"
                )
                first = False
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        first = True
        for dmap, cma in self._unique_indirection_maps.items():
            stmt = Assignment.create(
                    lhs=Reference(self.symtab.lookup(dmap)),
                    rhs=StructureReference.create(
                            self._invoke.schedule.symbol_table.lookup(
                                cma["argument"].proxy_name_indexed),
                            [f"indirection_dofmap_{cma['direction']}"]),
                    is_pointer=True)
            if first:
                stmt.preceding_comment = (
                    "Look-up required CMA indirection dofmaps"
                )
                first = False
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1
        return cursor

    def invoke_declarations(self):
        '''
        Declare all unique function space dofmaps in the PSy layer as pointers
        to integer arrays of rank 2.

        '''
        super().invoke_declarations()
        # Function space dofmaps
        for dmap in sorted(self._unique_fs_maps):
            if dmap not in self.symtab:
                dmap_sym = DataSymbol(
                    dmap, UnsupportedFortranType(
                        f"integer(kind=i_def), pointer :: {dmap}(:,:) "
                        f"=> null()"))
                self.symtab.add(dmap_sym, tag=dmap)

        # Column-banded dofmaps
        for dmap in sorted(self._unique_cbanded_maps):
            if dmap not in self.symtab:
                dmap_sym = DataSymbol(
                    dmap, UnsupportedFortranType(
                        f"integer(kind=i_def), pointer :: {dmap}(:,:) "
                        f"=> null()"))
                self.symtab.add(dmap_sym, tag=dmap)

        # CMA operator indirection dofmaps
        for dmap in sorted(self._unique_indirection_maps):
            if dmap not in self.symtab:
                dmap_sym = DataSymbol(
                    dmap, UnsupportedFortranType(
                        f"integer(kind=i_def), pointer :: {dmap}(:) "
                        "=> null()"))
                self.symtab.add(dmap_sym, tag=dmap)

    def stub_declarations(self):
        '''
        Add dofmap-related declarations to a Kernel stub.

        '''
        super().stub_declarations()
        # Function space dofmaps
        for dmap in sorted(self._unique_fs_maps):
            # We declare ndf first as some compilers require this
            ndf_name = \
                self._unique_fs_maps[dmap].function_space.ndf_name
            dim = self.symtab.find_or_create(
                ndf_name, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            dim.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(dim)
            dmap_symbol = self.symtab.find_or_create(
                dmap, symbol_type=DataSymbol,
                datatype=ArrayType(LFRicTypes("LFRicIntegerScalarDataType")(),
                                   [Reference(dim)]))
            dmap_symbol.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
            self.symtab.append_argument(dmap_symbol)

        # Column-banded dofmaps
        for dmap, cma in self._unique_cbanded_maps.items():
            if cma["direction"] == "to":
                ndf_name = cma["argument"].function_space_to.ndf_name
            elif cma["direction"] == "from":
                ndf_name = cma["argument"].function_space_from.ndf_name
            else:
                raise InternalError(
                    f"Invalid direction ('{cma['''direction''']}') found for "
                    f"CMA operator when collecting column-banded dofmaps. "
                    f"Should be either 'to' or 'from'.")
            symbol = self.symtab.find_or_create(
                ndf_name, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            symbol.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(symbol)

            nlayers = self.symtab.find_or_create_tag(
                "nlayers",
                symbol_type=LFRicTypes("MeshHeightDataSymbol")
            )
            nlayers.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
            self.symtab.append_argument(nlayers)

            dmap_symbol = self.symtab.find_or_create(
                dmap, symbol_type=DataSymbol,
                datatype=ArrayType(LFRicTypes("LFRicIntegerScalarDataType")(),
                                   [Reference(symbol), Reference(nlayers)]))
            dmap_symbol.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
            self.symtab.append_argument(dmap_symbol)

        # CMA operator indirection dofmaps
        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]
        for dmap, cma in self._unique_indirection_maps.items():
            if cma["direction"] == "to":
                param = "nrow"
            elif cma["direction"] == "from":
                param = "ncol"
            else:
                raise InternalError(
                    f"Invalid direction ('{cma['''direction''']}') found for "
                    f"CMA operator when collecting indirection dofmaps. "
                    f"Should be either 'to' or 'from'.")
            arg_name = cma["argument"].name
            dim = self.symtab.find_or_create_tag(
                f"{arg_name}:{param}:{suffix}",
                root_name=f"{arg_name}_{param}",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            dim.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(dim)

            dmap_symbol = self.symtab.find_or_create(
                dmap, symbol_type=DataSymbol,
                datatype=ArrayType(LFRicTypes("LFRicIntegerScalarDataType")(),
                                   [Reference(dim)]))
            dmap_symbol.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
            self.symtab.append_argument(dmap_symbol)


# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicDofmaps']
