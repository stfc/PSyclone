# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''
This module provides the basic functionality to create a PSyIR from scratch,
i.e. without parsing existing code. It contains convenience functions tor 
often required functionality. The implementation should be language agnostic,
but language-specific implementation will be based on it. For example, a Fortran
creator might provide a function 'create_subroutine', which will then be
mapped to 'create_container'. 
'''

from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (SymbolTable, GlobalInterface,
                                    RoutineSymbol, SymbolTable)



class Creator:
    '''The base class for the PSyIR creator. It provides basic functionality
    directly related to PSyIR concepts (e.g. create_container). Language-specific
    implementation (create_subroutine) will then be based on this class.
    '''

    def __init__(self):
        pass

    # -------------------------------------------------------------------------
    def create_container(self, name):
        '''This subroutine creates a PSyIR container.

        :param str name: name of the contanier.

        :returns: the created PSyIR container.
        :rtype: :psy:class:`psyclone.psyir.nodes.Container`
        '''

        return Container(name)

    # -------------------------------------------------------------------------
    def create_kernel_schedule(self, name, arguments):
        from psyclone.psyGen import KernelSchedule

        return KernelSchedule.create(name, SymbolTable(), arguments)

    # -------------------------------------------------------------------------
    def create_import(self, name, externals=[]):
                # First declare the module from which the externals are imported
        # as a symbol:
        use_container = ContainerSymbol(module_name)
        self.subroutine.symbol_table.add(use_container)

        for external in externals:
            # Now add each external to the symbol table and the use container:
            # Then import the PSyDataType
            data_type = DataSymbol(external,
                                   DeferredType(),
                                   interface=GlobalInterface(use_container))
            # This will also add the new symbol to use_psydatamod_container
            self.subroutine.symbol_table.add(data_type)

    # -------------------------------------------------------------------------
    def add_routine_name(self, container, name):
        routine_symbol = RoutineSymbol(name)
        return routine_symbol