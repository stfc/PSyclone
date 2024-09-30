# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
This module contains the LFRicHaloDepths class which manages the
declarations for halo-depth arguments required by special kernels that iterate
into the halo cells.

'''

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.f2pygen import DeclGen
from psyclone.psyir.symbols import DataSymbol


class LFRicHaloDepths(LFRicCollection):
    '''
    Manages the declarations for all halo-depth arguments (as needed by
    kernels that have operates_on == HALO_CELL_COLUMNS or
    OWNED_AND_HALO_CELL_COLUMNS) required by an Invoke or Kernel stub.

    '''
    def __init__(self, node):
        super().__init__(node)
        self._halo_depth_vars = set()
        if not Config.get().distributed_memory:
            # No distributed memory so there are no halo regions.
            return
        for kern in self._calls:
            if not kern.halo_depth:
                continue
            name = kern.halo_depth

            symbol = self._symbol_table.find_or_create_tag(
                f"{kern.name}:halo_depth",
                root_name=name,
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

            self._halo_depth_vars.add(symbol)

    def _invoke_declarations(self, parent):
        '''

        :param parent: the node in the f2pygen AST representing the PSy-layer
                       routine to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for unsupported intrinsic types of field
                               argument data.

        '''
        # Add the Invoke subroutine argument declarations for the
        # different halo depths. They are declared as intent "in".
        if self._halo_depth_vars:
            var_names = [sym.name for sym in self._halo_depth_vars]
            var_names.sort()
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=var_names, intent="in"))

    def _stub_declarations(self, parent):
        '''
        Add field-related declarations to a Kernel stub.

        :param parent: the node in the f2pygen AST representing the Kernel
                       stub to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for an unsupported data type of field
                               argument data.

        '''
        raise NotImplementedError("huhb")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for (see [https://psyclone-ref.readthedocs.io]).
__all__ = ['LFRicHaloDepths']
