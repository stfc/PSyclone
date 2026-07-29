# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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

''' This module implements the LFRicCellIterators collection which handles
    the requirements of kernels that operator on cells.'''

from typing import TYPE_CHECKING, Union

from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_invoke import LFRicInvoke
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Assignment, Reference
from psyclone.psyir.symbols import ArgumentInterface
if TYPE_CHECKING:
    from psyclone.domain.lfric import LFRicKernelArgument


class LFRicCellIterators(LFRicCollection):
    '''
    Handles all entities required by kernels that operate on cell-columns.

    :param kern_or_invoke: the Kernel or Invoke for which to manage cell
                           iterators.

    :raises GenerationError: if an Invoke has no field or operator arguments.

    '''
    def __init__(self,
                 kern_or_invoke: Union[LFRicKern, LFRicInvoke]):
        super().__init__(kern_or_invoke)

        # Dictionary to hold the names of the various nlayers/ndata variables
        # and (for invokes) the kernel argument to which each corresponds.
        self._nlayers_names: dict[str, LFRicKernelArgument] = {}
        self._ndata_names: dict[str, LFRicKernelArgument] = {}

        if not self._invoke:
            return

        # Each kernel that operates on either the domain or cell-columns needs
        # an 'nlayers' obtained from the first field/operator argument.
        for kern in self._invoke.schedule.walk(LFRicKern):
            if kern.iterates_over != "dof":
                first_arg: LFRicKernelArgument = (
                    kern.arguments.first_field_or_operator)
                sym = self.symtab.find_or_create_tag(
                    f"nlayers_{first_arg.name}",
                    symbol_type=LFRicTypes("MeshHeightDataSymbol"))
                self._nlayers_names[sym.name] = first_arg
                # We must also check for any subsequent arguments that have
                # a number of layers specified by a label in the metadata.
                for arg in kern.arguments.args:
                    if arg.nlayers and not arg.nlayers.isnumeric():
                        sym = self.symtab.find_or_create_tag(
                            f"nlayers_{arg.nlayers}",
                            symbol_type=LFRicTypes("MeshHeightDataSymbol"))
                        if sym.name not in self._nlayers_names:
                            self._nlayers_names[sym.name] = arg
                    if arg.ndata and not arg.ndata.isnumeric():
                        sym = self.symtab.find_or_create_tag(
                            f"ndata_{arg.ndata}",
                            # TODO - shouldn't be MeshHeightDataSymbol
                            symbol_type=LFRicTypes("MeshHeightDataSymbol"))
                        if sym.name not in self._ndata_names:
                            self._ndata_names[sym.name] = arg

        first_var = None
        for var in self._invoke.psy_unique_vars:
            if not var.is_scalar:
                first_var = var
                break
        if not first_var:
            raise GenerationError(
                "Cannot create an Invoke with no field/operator arguments.")
        self._first_var = first_var

    def stub_declarations(self) -> None:
        '''
        Declare entities required for a kernel stub that operates on
        cell-columns.

        '''
        super().stub_declarations()
        if self._kernel.cma_operation not in ["apply", "matrix-matrix"]:
            nlayers = self.symtab.find_or_create_tag(
                "nlayers",
                symbol_type=LFRicTypes("MeshHeightDataSymbol")
            )
            nlayers.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
            self.symtab.append_argument(nlayers)

    def initialise(self, cursor: int) -> int:
        '''
        Look-up the number(s) of vertical layers and number(s) of data values
        per dof in the PSy layer.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        cursor = self._initialise_var_list(cursor, self._nlayers_names,
                                           "get_nlayers",
                                           "number of layers")
        cursor = self._initialise_var_list(cursor, self._ndata_names,
                                           "get_ndata",
                                           "number of data values per dof")
        return cursor

    def _initialise_var_list(self,
                             cursor: int,
                             name_map: dict[str, "LFRicKernelArgument"],
                             fn_name: str,
                             comment: str
                             ) -> int:
        '''
        A utility to generate the initialisation statements for the
        kernel arguments named in the supplied map.

        :param cursor: the current position at which to add content in
            the PSy-layer schedule.
        :param name_map: dict holding descriptions of each kernel argument,
            indexed by name.
        :param fn_name: the name of the type-bound procedure that must be
            called to get the initial vaue of each argument.
        :param comment: text to use in the associated comment in the
            generated PSy-layer code.

        '''
        # Sort for test reproducibility
        sorted_names = sorted(list(name_map.keys()))
        init_cursor = cursor
        for name in sorted_names:
            symbol = self.symtab.lookup(name)
            var = name_map[name]
            stmt = Assignment.create(
                lhs=Reference(symbol),
                rhs=var.generate_method_call(fn_name))
            if cursor == init_cursor:
                stmt.preceding_comment = f"Initialise {comment}"
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1
        return cursor
