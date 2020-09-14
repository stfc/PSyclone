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
This module provides the basic functionality to create a PSyIR from scratch
based on Fortran. This means most methods will correspond to Fortran elements
(e.g. 'use', or data type names like 'double precision'). It is based on the 
Creator base class, so this class will mostly map names
'''

from psyclone.psyir.creator import Creator
from psyclone.psyir.nodes import Call, Literal
from psyclone.psyir.symbols import (ArrayType, ContainerSymbol,
                                    DataSymbol, DeferredType,
                                    GlobalInterface, REAL8_TYPE, ScalarType,
                                    SymbolTable)


class FortranCreator(Creator):
    '''This class provides methods to create a PSyIR based on Fortran
    language elements.
    '''

    def __init__(self):
        # The creator keeps track of the inner-most symbol table, to reduce
        # number of parameters in some of the calls.
        pass

    # -------------------------------------------------------------------------
    def create_module(self, name):
        '''Create a module with the given name. This is mapped to a PSyIR
        container structure.

        :param str name: name of the module.

        :returns: the module as a Container instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Container`

        '''
        self.module = self.create_container(name)
        return self.module

    # -------------------------------------------------------------------------
    def create_subroutine(self, container, name, arguments=[]):
        '''Creates a subroutine, which is mapped to a PSyIR KernelSChedule.
        If defined, the subroutine will automatically be added to the
        latest module defined.

        :param str name: name of the subroutine.
        :param arguments: arguments of the subroutine.
        :type arguments: list of SOMETHING OR OTHER 

        :returns: the subroutine as a KernelSchedule instance.
        :rtype: :py:class:`psyclone.pssyGen.KernelSchedule`
        '''

        subroutine = self.create_kernel_schedule(name, arguments)
        if container:
            container.addchild(subroutine)
        
        return subroutine

    # -------------------------------------------------------------------------
    def add_use(self, container, module_name, externals=[]):

        # First declare the module from which the externals are imported
        # as a symbol:
        use_container = ContainerSymbol(module_name)
        container.symbol_table.add(use_container)

        for external in externals:
            # Now add each external to the symbol table and the use container:
            # Then import the PSyDataType
            data_type = DataSymbol(external,
                                   DeferredType(),
                                   interface=GlobalInterface(use_container))
            # This will also add the new symbol to use_psydatamod_container
            container.symbol_table.add(data_type)

    # -------------------------------------------------------------------------
    def convert_type(self, type_name):
        type_name = type_name.lower()
        if type_name in ["real*8", "double precision"]:
            return REAL8_TYPE
        raise GenerationError("Type {0} is not supported".format(type_name))

    # -------------------------------------------------------------------------
    def declare_scalar(self, container, var_name, type_name):
        unique_var_name = container.symbol_table.new_symbol_name(var_name)
        var_type = self.convert_type(type_name)
        var = DataSymbol(unique_var_name, var_type)
        container.symbol_table.add(var)
        return var

    # -------------------------------------------------------------------------
    def declare_array(self, container, var_name, type_name, dimensions):
        var_type = self.convert_type(type_name)
        unique_name = container.symbol_table.new_symbol_name(var_name)
        #ar = ArrayType(REAL8_TYPE, [ArrayType.Extent.DEFERRED, 
        #                            ArrayType.Extent.DEFERRED])
        l_dims = []
        for dim in dimensions:
            if dim==":":
                l_dims.append(ArrayType.Extent.DEFERRED)
            else:
                raise GenerationError("Unsupported dimension: '{0}'"
                                      .format(dim))
        ar = ArrayType(var_type, l_dims)
        var_symbol = DataSymbol(unique_name, ar)
        container.symbol_table.add(var_symbol)
        return var_symbol

    # -------------------------------------------------------------------------
    def add_call(self, container, subroutine_name, arguments):
        routine = self.add_routine_name(container, subroutine_name)

        l_args = []
        for arg in arguments:
            if isinstance(arg, str):
                lit = Literal(arg,
                              ScalarType(ScalarType.Intrinsic.CHARACTER,
                                         ScalarType.Precision.UNDEFINED))
                l_args.append(lit)

        call = Call.create(routine, l_args)
        container.addchild(call)



