# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''This module contains PSyclone Kernel-layer-specific PSyIR classes.

'''
from psyclone.psyir.symbols import DataTypeSymbol
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk


class KernelMetadataSymbol(DataTypeSymbol):
    '''Specialise DataTypeSymbol to capture Kernel Metadata information,
    verify that it conforms to the expected syntax and to provide the
    information to PSyclone in an easier to access form.

    '''
    class KernelMetadataArg():
        '''Internal class to capture Kernel metadata argument information.

        :param meta_arg: the native PSyIR representation of a kernel \
            metadata argument. This is currently part of a codeblock \
            in PSyIR so is actually an fparser2 tree.
        :type meta_arg: :py:class:`fparser.two.Fortran2003.Part_Ref`

        '''
        def __init__(self, meta_arg):
            # TODO? Check name == "go_arg" print(repr(meta_args))
            arg_list = meta_arg.children[1]
            self._access = arg_list.children[0].string
            self._description = arg_list.children[1].string
            if len(arg_list.children) == 2:
                # Grid properties have 2 arguments
                return
            if isinstance(arg_list.children[2], Fortran2003.Name):
                self._form = arg_list.children[2].string
                self._stencil = None
            else: # Stencil
                self._form = arg_list.children[2].children[0].string
                self._stencil = []
                for stencil_dim in arg_list.children[2].children[1].children:
                    self._stencil.append(stencil_dim.children[0])

        def __repr__(self):
            return(f"({self._access}, {self._description}, {self._errr}, {self._stencil})")


    def _setup(self):
        '''Validates the metadata and extracts the values to make them easier
        to access from an instance of this class.

        '''
        unknown_fortran_type = self.datatype
        # The type is stored as a string so parse it with fparser2
        reader = FortranStringReader(unknown_fortran_type.declaration)
        spec_part = Fortran2003.Derived_Type_Def(reader)
        component_part = spec_part.children[1]

        meta_args_def = component_part.children[0]
        self._meta_args = []
        for meta_arg in walk(meta_args_def, Fortran2003.Ac_Value_List)[0].children:
            self._meta_args.append(self.KernelMetadataArg(meta_arg))
            
        iterates_over_def = component_part.children[1]
        self._iterates_over = walk(
            iterates_over_def,
            Fortran2003.Component_Initialization)[0].children[1].string

        index_offset_def = component_part.children[2]
        self._index_offset = walk(
            index_offset_def,
            Fortran2003.Component_Initialization)[0].children[1].string

        type_bound_procedure = spec_part.children[2]
        self._routine_name = walk(
            type_bound_procedure,
            Fortran2003.Specific_Binding)[0].children[4].string
