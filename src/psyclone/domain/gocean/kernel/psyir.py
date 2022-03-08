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
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk
from psyclone.domain.gocean import GOceanConstants


class KernelMetadataSymbol(DataTypeSymbol):
    '''Specialise DataTypeSymbol to capture Kernel Metadata information,
    verify that it conforms to the expected syntax and to provide the
    information to PSyclone in an easier to access form.

    '''
    class GridArg():
        '''Internal class to capture Kernel metadata argument information for
        a field argument.

        :param meta_arg: the native PSyIR representation of a kernel \
            metadata argument. This is currently part of a codeblock \
            in PSyIR so is actually an fparser2 tree.
        :type meta_arg: :py:class:`fparser.two.Fortran2003.Part_Ref`

        '''
        def __init__(self, meta_arg):
            arg_list = meta_arg.children[1]
            if len(arg_list.children) != 2:
                raise Exception("xxx")
            self._access = arg_list.children[0].string
            self._grid_property = arg_list.children[1].string

        @property
        def access(self):
            '''
            :returns: the value of access.
            :rtype: str
            '''
            return self._access

        @property
        def name(self):
            '''
            :returns: the grid property name.
            :rtype: str
            '''
            return self._grid_property


    class FieldArg():
        '''Internal class to capture Kernel metadata argument information for
        a field argument.

        :param meta_arg: the native PSyIR representation of a kernel \
            metadata argument. This is currently part of a codeblock \
            in PSyIR so is actually an fparser2 tree.
        :type meta_arg: :py:class:`fparser.two.Fortran2003.Part_Ref`

        '''
        def __init__(self, meta_arg):
            arg_list = meta_arg.children[1]
            if len(arg_list.children) != 3:
                raise Exception("xxx")
            self._access = arg_list.children[0].string
            self._stagger = arg_list.children[1].string
            if isinstance(arg_list.children[2], Fortran2003.Name):
                self._form = arg_list.children[2].string
                self._stencil = None
            else: # Stencil
                self._form = arg_list.children[2].children[0].string
                self._stencil = []
                for stencil_dim in arg_list.children[2].children[1].children:
                    self._stencil.append(stencil_dim.children[0])

        @property
        def access(self):
            '''
            :returns: the value of access.
            :rtype: str
            '''
            return self._access

        @property
        def stagger(self):
            '''
            :returns: the value of stagger.
            :rtype: str
            '''
            return self._stagger

        @property
        def form(self):
            '''
            :returns: the form of access.
            :rtype: str
            '''
            return self._form

        @property
        def stencil(self):
            '''
            :returns: the stencil value, or None if there is no stencil.
            :rtype: str[3] or NoneType
            '''
            return self._stencil


    @property
    def iterates_over(self):
        '''
        :returns: the value of iterates_over.
        :rtype: str
        '''
        return self._iterates_over

    @property
    def index_offset(self):
        '''
        :returns: the value of index_offset.
        :rtype: str
        '''
        return self._index_offset

    @property
    def args(self):
        '''
        :returns: a list of arg objects capturing their metadata values.
        :rtype: list of :py:class:`psyclone.psyir.common.kernel. \
            KernelMetadataSymbol.KernelMetadataArg`
        '''
        return self._meta_args

    @property
    def code(self):
        '''
        :returns: the kernel code routine name.
        :rtype: str
        '''
        return self._routine_name

    def _setup(self):
        '''Validates the non-api-specific aspects of the metadata and extracts
        the values to make them easier to access from an instance of
        this class.

        '''
        const = GOceanConstants()
        unknown_fortran_type = self.datatype
        # The type is stored as a string so parse it with fparser2
        reader = FortranStringReader(unknown_fortran_type.declaration)
        spec_part = Fortran2003.Derived_Type_Def(reader)
        component_part = spec_part.children[1]

        found_meta_args = False
        found_iterates_over = False
        found_index_offset = False
        for entry in component_part.children:
            name = entry.children[2].children[0].children[0].string.lower()
            if name == "meta_args":
                if found_meta_args:
                    raise ParseError(
                        f"'meta_args' should only be defined once in the "
                        f"metadata, but found {str(component_part)}.")
                found_meta_args = True
                self._meta_args = []
                for meta_arg in walk(
                        entry, Fortran2003.Ac_Value_List)[0].children:
                    if len(meta_arg.children[1].children) == 2:
                        self._meta_args.append(self.GridArg(meta_arg))
                    else: # nargs should be 3
                        self._meta_args.append(self.FieldArg(meta_arg))
            elif name == "iterates_over":
                if found_iterates_over:
                    raise ParseError(
                        f"'iterates_over' should only be defined once in "
                        f"the metadata, but found {str(component_part)}.")
                found_iterates_over = True
                iterates_over_def = component_part.children[1]
                self._iterates_over = walk(
                    entry,
                    Fortran2003.Component_Initialization)[0].children[1].string
                if self._iterates_over.lower() not in \
                   const.VALID_ITERATES_OVER:
                    raise ParseError(
                        f"The value of 'iterates_over' should be one of "
                        f"{str(const.VALID_ITERATES_OVER)}, but found "
                        f"'{self._iterates_over}'.")
            elif name == "index_offset":
                if found_index_offset:
                    raise ParseError(
                        f"'index_offset' should only be defined once in the "
                        f"metadata, but found {str(component_part)}.")
                found_index_offset = True
                self._index_offset = walk(
                    entry,
                    Fortran2003.Component_Initialization)[0].children[1].string
                if self._index_offset.lower() not in const.VALID_OFFSET_NAMES:
                    raise ParseError(
                        f"The value of 'index_offset' should be one of "
                        f"{str(const.VALID_OFFSET_NAMES)}, but found "
                        f"'{self._index_offset}'.")
            else:
                raise ParseError(
                    f"Expecting metadata entries to be one of 'meta_args', "
                    f"'iterates_over', or 'index_offset', but found '{name}' "
                    f"in {str(component_part)}.")

        if not found_meta_args:
            raise ParseError(
                f"Expecting 'meta_args' to be an entry in the metadata but "
                f"it was not found in {str(component_part)}.")
        if not found_iterates_over:
            raise ParseError(
                f"Expecting 'iterates_over' to be an entry in the metadata "
                f"but it was not found in {str(component_part)}.")
        if not found_index_offset:
            raise ParseError(
                f"Expecting 'index_offset' to be an entry in the metadata but "
                f"it was not found in {str(component_part)}.")

        # TODO RAISE EXCEPTION IF INVALID OR DOES NOT EXIST
        type_bound_procedure = spec_part.children[2]
        self._routine_name = walk(
            type_bound_procedure,
            Fortran2003.Specific_Binding)[0].children[4].string
