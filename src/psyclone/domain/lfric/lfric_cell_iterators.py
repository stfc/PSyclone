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

''' This module implements the LFRicCellIterators collection which handles
    the requirements of kernels that operator on cells.'''

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import GenerationError
from psyclone.f2pygen import AssignGen, CommentGen, DeclGen


class LFRicCellIterators(LFRicCollection):
    '''
    Handles all entities required by kernels that operate on cell-columns.

    :param kern_or_invoke: the Kernel or Invoke for which to manage cell
                           iterators.
    :type kern_or_invoke: :py:class:`psyclone.domain.lfric.LFRicKern` |
                          :py:class:`psyclone.dynamo0p3.LFRicInvoke`

    :raises GenerationError: if an Invoke has no field or operator arguments.

    '''
    def __init__(self, kern_or_invoke):
        super().__init__(kern_or_invoke)

        # Dictionary to hold the names of the various nlayers variables and
        # (for invokes) the kernel argument to which each corresponds.
        self._nlayers_names = {}

        if not self._invoke:
            # We are dealing with a single Kernel so there is only one
            # 'nlayers' variable and we don't need to store the associated
            # argument.
            self._nlayers_names[self._symbol_table.find_or_create_tag(
                "nlayers",
                symbol_type=LFRicTypes("MeshHeightDataSymbol")).name] = None
            # We're not generating a PSy layer so we're done here.
            return

        # Each kernel that operates on either the domain or cell-columns needs
        # an 'nlayers' obtained from the first field/operator argument.
        for kern in self._invoke.schedule.walk(LFRicKern):
            if kern.iterates_over != "dof":
                arg = kern.arguments.first_field_or_operator
                sym = self._symbol_table.find_or_create_tag(
                    f"nlayers_{arg.name}",
                    symbol_type=LFRicTypes("MeshHeightDataSymbol"))
                self._nlayers_names[sym.name] = arg

        first_var = None
        for var in self._invoke.psy_unique_vars:
            if not var.is_scalar:
                first_var = var
                break
        if not first_var:
            raise GenerationError(
                "Cannot create an Invoke with no field/operator arguments.")
        self._first_var = first_var

    def _invoke_declarations(self, parent):
        '''
        Declare entities required for iterating over cells in the Invoke.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("lfric")

        # Declare the number of layers in the mesh for each kernel that
        # operates on cell-columns or the domain.
        name_list = list(self._nlayers_names.keys())
        if name_list:
            name_list.sort()  # Purely for test reproducibility.
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=name_list))

    def _stub_declarations(self, parent):
        '''
        Declare entities required for a kernel stub that operates on
        cell-columns.

        :param parent: the f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("lfric")

        if self._kernel.cma_operation not in ["apply", "matrix-matrix"]:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in",
                               entity_decls=list(self._nlayers_names.keys())))

    def initialise(self, parent):
        '''
        Look-up the number of vertical layers in the mesh for each user-
        supplied kernel that operates on cell columns.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if not self._nlayers_names or not self._invoke:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Initialise number of layers"))
        parent.add(CommentGen(parent, ""))
        # Sort for test reproducibility
        sorted_names = list(self._nlayers_names.keys())
        sorted_names.sort()
        for name in sorted_names:
            var = self._nlayers_names[name]
            parent.add(AssignGen(
                parent, lhs=name,
                rhs=(f"{var.proxy_name_indexed}%{var.ref_name()}%"
                     f"get_nlayers()")))
