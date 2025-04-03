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
from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicCollection
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import AssignGen, CommentGen, DeclGen


class LFRicDofmaps(LFRicCollection):
    '''
    Holds all information on the dofmaps (including column-banded and
    indirection) required by an invoke.

    :param node: Kernel or Invoke for which to manage dofmaps.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.dynamo0p3.LFRicInvoke`

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

        for call in self._calls:
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

    def initialise(self, parent):
        ''' Generates the calls to the LFRic infrastructure that
        look-up the necessary dofmaps. Adds these calls as children
        of the supplied parent node. This must be an appropriate
        f2pygen object. '''

        # If we've got no dofmaps then we do nothing
        if self._unique_fs_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up dofmaps for each function space"))
            parent.add(CommentGen(parent, ""))

            for dmap, field in self._unique_fs_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=field.proxy_name_indexed +
                                     "%" + field.ref_name() +
                                     "%get_whole_dofmap()"))
        if self._unique_cbanded_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up required column-banded dofmaps"))
            parent.add(CommentGen(parent, ""))

            for dmap, cma in self._unique_cbanded_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=cma["argument"].proxy_name_indexed +
                                     "%column_banded_dofmap_" +
                                     cma["direction"]))

        if self._unique_indirection_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up required CMA indirection dofmaps"))
            parent.add(CommentGen(parent, ""))

            for dmap, cma in self._unique_indirection_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=cma["argument"].proxy_name_indexed +
                                     "%indirection_dofmap_"+cma["direction"]))

    def _invoke_declarations(self, parent):
        '''
        Declare all unique function space dofmaps in the PSy layer as pointers
        to integer arrays of rank 2.

        :param parent: the f2pygen node to which to add the declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("lfric")

        # Function space dofmaps
        decl_map_names = \
            [dmap+"(:,:) => null()" for dmap in sorted(self._unique_fs_maps)]

        if decl_map_names:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True, entity_decls=decl_map_names))

        # Column-banded dofmaps
        decl_bmap_names = \
            [dmap+"(:,:) => null()" for dmap in self._unique_cbanded_maps]
        if decl_bmap_names:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True, entity_decls=decl_bmap_names))

        # CMA operator indirection dofmaps
        decl_ind_map_names = \
            [dmap+"(:) => null()" for dmap in self._unique_indirection_maps]
        if decl_ind_map_names:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True, entity_decls=decl_ind_map_names))

    def _stub_declarations(self, parent):
        '''
        Add dofmap-related declarations to a Kernel stub.

        :param parent: node in the f2pygen AST representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("lfric")

        # Function space dofmaps
        for dmap in sorted(self._unique_fs_maps):
            # We declare ndf first as some compilers require this
            ndf_name = \
                self._unique_fs_maps[dmap].function_space.ndf_name
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[ndf_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", dimension=ndf_name,
                               entity_decls=[dmap]))
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
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[ndf_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in",
                               dimension=",".join([ndf_name, "nlayers"]),
                               entity_decls=[dmap]))
        # CMA operator indirection dofmaps
        for dmap, cma in self._unique_indirection_maps.items():
            if cma["direction"] == "to":
                dim_name = cma["argument"].name + "_nrow"
            elif cma["direction"] == "from":
                dim_name = cma["argument"].name + "_ncol"
            else:
                raise InternalError(
                    f"Invalid direction ('{cma['''direction''']}') found for "
                    f"CMA operator when collecting indirection dofmaps. "
                    f"Should be either 'to' or 'from'.")
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[dim_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", dimension=dim_name,
                               entity_decls=[dmap]))


# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicDofmaps']
