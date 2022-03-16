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
import re

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.configuration import Config
from psyclone.domain.gocean import GOceanConstants
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, Symbol


class KernelMetadataSymbol(DataTypeSymbol):
    '''Specialise DataTypeSymbol to capture Kernel Metadata information,
    verify that it conforms to the expected syntax and to provide the
    information to PSyclone in an easier to access form.

    :param str name: the name of this symbol.
    :param datatype: the type represented by this symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param visibility: the visibility of this symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
    :param interface: the interface to this symbol.
    :type interface: :py:class:`psyclone.psyir.symbols.SymbolInterface`

    '''
    def __init__(self, name, datatype, visibility=Symbol.DEFAULT_VISIBILITY,
                 interface=None):
        super().__init__(name, datatype, visibility, interface)
        self.setup()

    def setup(self):
        '''Populate KernelMetadataSymbol properties and check that the symbol
        contains valid metadata.

        '''
        # The metadata is stored as a string, so create an fparser2
        # parse tree.
        reader = FortranStringReader(self.datatype.declaration)
        spec_part = Fortran2003.Derived_Type_Def(reader)

        # Extract and store the required 'iterates_over',
        # 'index_offset' and 'code' properties from the parse tree for
        # ease of access.
        value = self._get_property(spec_part, "iterates_over").string
        self._validate_iterates_over(value)
        self._iterates_over = value

        value = self._get_property(spec_part, "index_offset").string
        self._validate_index_offset(value)
        self._index_offset = value

        self._code = self._get_property(spec_part, "code").string

        # meta_args contains arguments which have
        # properties. Therefore create appropriate (GridArg or
        # FieldArg) instances to capture this information.
        meta_args = self._get_property(spec_part, "meta_args")
        self._meta_args = []
        for meta_arg in walk(
                    meta_args, Fortran2003.Ac_Value_List)[0].children:
            if len(meta_arg.children[1].children) == 2:
                self._meta_args.append(self.GridArg(meta_arg, self))
            elif len(meta_arg.children[1].children) == 3:
                self._meta_args.append(self.FieldArg(meta_arg))
            else:
                raise ParseError(
                    f"'meta_args' should have either 2 or 3 arguments, but "
                    f"found {len(meta_arg.children[1].children)} in "
                    f"{str(meta_arg)}.")

    def _write_fortran_string(self):
        '''
        :returns: the metadata represented by this instance as a Fortran \
            string.
        :rtype: str

        '''
        go_args = []
        for go_arg in self.args:
            go_args.append(go_arg._write_fortran_string())
        go_args_str = ", ".join(go_args)
        result = (
            f"TYPE, EXTENDS(kernel_type) :: {self.name}\n"
            f"TYPE(go_arg), DIMENSION({len(self.args)}) :: "
            f"meta_args = (/{go_args_str}/)\n"
            f"  INTEGER :: ITERATES_OVER = {self.iterates_over}\n"
            f"  INTEGER :: index_offset = {self.index_offset}\n"
            f"  CONTAINS\n"
            f"  PROCEDURE, NOPASS :: code => {self.code}\n"
            f"END TYPE {self.name}\n")
        return result

    @staticmethod
    def _get_property(spec_part, property_name):
        '''Internal utility that gets the property 'property_name' from the an
        fparser2 tree capturing gocean metadata. It is
        assumed that the "code property is part of a type bound
        procedure and that the other properties are part of the data
        declarations.

        :param spec_part: the fparser2 parse tree containing the metadata.
        :type spec_part: :py:class:`fparser.two.Fortran2003.Derived_Type_Def`
        :param str property_name: the name of the property whose value \
            is being extracted from the metadata.

        :raises ParseError: if the property name is not found in the \
            metadata.

        '''
        if property_name == "code":
            # This should be found in a type bound procedure after the
            # contains keyword
            type_bound_procedure = spec_part.children[2]
            return walk(
                type_bound_procedure,
                Fortran2003.Specific_Binding)[0].children[4]
        # The should be a variable declaration within the derived type.
        component_part = spec_part.children[1]
        for entry in component_part.children:
            name = entry.children[2].children[0].children[0].string.lower()
            if name.lower() == property_name:
                return walk(
                    entry,
                    Fortran2003.Component_Initialization)[0].children[1]
        raise InternalError(
            f"The property name should always be found in the metadata but "
            f"'{property_name}' was not found in {str(spec_part)}.")

    @staticmethod
    def _validate_iterates_over(value):
        '''Check that 'value' is a valid 'iterates_over' value.

        :param str value: the value to check.

        raises ValueError: if the supplied value is invalid.

        '''
        const = GOceanConstants()
        if value.lower() not in const.VALID_ITERATES_OVER:
            raise ValueError(
                f"Expected one of {str(const.VALID_ITERATES_OVER)}, but "
                f"found '{value}'.")

    @property
    def iterates_over(self):
        '''
        :returns: the value of iterates_over.
        :rtype: str
        '''
        return self._iterates_over

    @iterates_over.setter
    def iterates_over(self, value):
        '''
        :param str value: the new value for iterates_over.

        :raises ValueError: if an invalid value is supplied.

        '''
        self._validate_iterates_over(value)
        self._iterates_over = value
        # Update the underlying string representation of the datatype.
        self.datatype.declaration = self._write_fortran_string()

    @staticmethod
    def _validate_index_offset(value):
        '''Check that 'value' is a valid 'index_offset' value.

        :param str value: the value to check.

        raises ValueError: if the supplied value is invalid.

        '''
        const = GOceanConstants()
        if value.lower() not in const.VALID_OFFSET_NAMES:
            raise ValueError(
                f"Expected one of {str(const.VALID_OFFSET_NAMES)}, but "
                f"found '{value}'.")

    @property
    def index_offset(self):
        '''
        :returns: the value of index_offset.
        :rtype: str
        '''
        return self._index_offset

    @index_offset.setter
    def index_offset(self, value):
        '''
        :param str value: the new value for index_offset.
        '''
        self._validate_index_offset(value)
        self._index_offset = value
        # Update the underlying string representation of the datatype.
        self.datatype.declaration = self._write_fortran_string()

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
        return self._code

    @code.setter
    def code(self, value):
        '''
        :param str value: the new value for code.
        '''
        self._code = value
        # Update the underlying string representation of the datatype.
        self.datatype.declaration = self._write_fortran_string()

    class GridArg():
        '''Internal class to capture Kernel metadata argument information for
        a grid property.

        :param meta_arg: an fparser2 tree representation of the metadata.
        :type meta_arg: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param parent: a KernelMetadataSymbol instance that captures \
            other parts of the metadata and references this instance.
        :type parent: :py:class`psyclone.psyir.common.kernel. \
            KernelMetadataSymbol`

        :raises ParseError: if the metadata does not contain two \
            arguments.

        '''
        def __init__(self, meta_arg, parent):
            self._parent = parent

            arg_list = meta_arg.children[1]
            if len(arg_list.children) != 2:
                raise ParseError(
                    f"There should be 2 kernel metadata arguments for a grid "
                    f"property but found {len(arg_list.children)} in "
                    f"{str(meta_arg)}")
            value = arg_list.children[0].string
            self._validate_access(value)
            self._access = value
            value = arg_list.children[1].string
            self._validate_name(value)
            self._name = value

        def _write_fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            return f"go_arg({self.access}, {self.name})"

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if not self._access.lower() in const.VALID_INTRINSIC_TYPES:
                raise ValueError(
                    f"The first metadata entry for a grid property argument "
                    f"should be one of {const.VALID_INTRINSIC_TYPES}, but "
                    f"found '{self._access}'.")

        @property
        def access(self):
            '''
            :returns: the value of access.
            :rtype: str
            '''
            return self._access

        @access.setter
        def access(self, value):
            '''
            :param str value: the new value for access.
            '''
            self._validate_access(value)
            self._access = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent._write_fortran_string()

        @staticmethod
        def _validate_name(value):
            '''Check that 'value' is a valid 'name' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            config = Config.get()
            api_config = config.api_conf("gocean1.0")
            grid_property_names = list(api_config.grid_properties.keys())
            if not value.lower() in grid_property_names:
                raise ValueError(
                    f"The second meadata entry for a grid property argument "
                    f"should be one of {grid_property_names}, but found "
                    f"'{value}.")

        @property
        def name(self):
            '''
            :returns: the grid property name.
            :rtype: str
            '''
            return self._name

        @name.setter
        def name(self, value):
            '''
            :param str value: the new value for name.
            '''
            self._validate_name()
            self._name = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent._write_fortran_string()

    class FieldArg():
        '''Internal class to capture Kernel metadata argument information for
        a field.

        :param meta_arg: an fparser2 tree representation of the metadata.
        :type meta_arg: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :param parent: a KernelMetadataSymbol instance that captures \
            other parts of the metadata and references this instance.
        :type parent: :py:class`psyclone.psyir.common.kernel. \
            KernelMetadataSymbol`

        :raises ParseError: if the metadata does not contain three \
            arguments.

        '''
        def __init__(self, meta_arg, parent):
            self._parent = parent

            const = GOceanConstants()
            arg_list = meta_arg.children[1]
            if not len(arg_list.children) == 3:
                raise ParseError(
                    f"There sould be 3 kernel metadata entries for a field "
                    f"argument, but found {len(arg_list)} in {str(meta_arg)}.")

            value = arg_list.children[0].string
            self._validate_access(value)
            self._access = value

            value = arg_list.children[1].string
            self._validate_stagger(value)
            self._stagger = value

            if isinstance(arg_list.children[2], Fortran2003.Name):
                value = arg_list.children[2].string
                self._validate_form(value)
                self._form = value
                self._stencil = None
            else: # Stencil
                value = arg_list.children[2].children[0].string
                # TODO VALIDATE???
                self._form = value
                self._stencil = []
                for stencil_dim in arg_list.children[2].children[1].children:
                    self._stencil.append(stencil_dim.children[0])
                    self._validate_stencil_entry(stencil_dim)
                if not len(self._stencil) == 3:
                    raise ParseError(
                        f"If the third metadata entry is a stencil, it should "
                        f"contain 3 arguments, but found "
                        f"{len(self._stencil)}.")

        def _write_fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            if self.stencil:
                return (f"go_arg({self.access}, {self.stagger}, "
                        f"{self.form}({', '.join(self.stencil)}))")
            else:
                return f"go_arg({self.access}, {self.stagger}, {self.form})"

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if not self._access.lower() in const.VALID_INTRINSIC_TYPES:
                raise ParseError(
                    f"The first metadata entry for a field argument should "
                    f"be one of {const.VALID_INTRINSIC_TYPES}, but found "
                    f"'{value}'.")

        @property
        def access(self):
            '''
            :returns: the value of access.
            :rtype: str
            '''
            return self._access

        @access.setter
        def access(self, value):
            '''
            :param str value: the new value for access.
            '''
            self._validate_access(value)
            self._access = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent._write_fortran_string()

        @staticmethod
        def _validate_stagger(value):
            '''Check that 'value' is a valid 'stagger' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if not value.lower() in const.VALID_FIELD_GRID_TYPES:
                raise ValueError(
                    f"The second metadata entry for a field argument should "
                    f"be one of {const.VALID_OFFSET_NAMES}, but found "
                    f"'{value}'.")

        @property
        def stagger(self):
            '''
            :returns: the value of stagger.
            :rtype: str
            '''
            return self._stagger

        @stagger.setter
        def stagger(self, value):
            '''
            :param str value: the new value for stagger.
            '''
            self._validate_stagger(value)
            self._stagger = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent._write_fortran_string()

        @staticmethod
        def validate_form(value):
            '''Check that 'value' is a valid 'form' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if not value.lower() in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"The third metadata entry for a field should "
                    f"be one of {const.VALID_STENCIL_NAMES} or "
                    f"'stencil(...)', but found '{value}'.")

        @property
        def form(self):
            '''
            :returns: the form of access.
            :rtype: str
            '''
            return self._form

        @form.setter
        def form(self, value):
            '''
            :param str value: the new value for form.
            '''
            self._validate_form(value)
            self._form = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent._write_fortran_string()

        def validate_stencil(value_list):
            '''Check that 'value_list' is a valid list of values to define a
            'stencil'.

            :param value_list: the values to check.
            :type value_list: list of str

            raises ValueError: if the supplied value is invalid.

            '''
            p = re.compile("[01]{3,3}")
            if not isinstance(value_list, list):
                raise TypeError(
                    f"Stencil values should be provided as a list but found "
                    f"{type(value_list).__name__}.")
            if not len(value_list) == 3:
                raise ValueError(
                    "Stencil values should be a list with 3 entries, but "
                    f"found {len(value_list)}.")
            for value in value_list:
                if not isinstance(value, str):
                    raise TypeError(
                        f"Stencil entries should be strings but found "
                        f"{type(value).__name__}.")

                if not p.match(value.children[0]):
                    raise ValueError(
                        f"Stencil entries should follow the pattern "
                        f"[01]{3:3} but found {value.children[0]}.")

        def _validate_stencil_entry(value):
            '''Check that 'value' is a valid 'stencil' element

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''

        @property
        def stencil(self):
            '''
            :returns: the stencil value, or None if there is no stencil.
            :rtype: str[3] or NoneType
            '''
            return self._stencil

        @stencil.setter
        def stencil(self, value_list):
            '''
            :param value_list: the new value for form.
            :type value_list: list of str
            '''
            self._validate_stencil(value_list)
            self._stencil = value_list
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent._write_fortran_string()

    def validate(self):
        '''Validates the metadata.'''

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
                    elif len(meta_arg.children[1].children) == 3:
                        self._meta_args.append(self.FieldArg(meta_arg))
                    else:
                        raise ParseError(
                            f"'meta_args' should have either 2 or 3 "
                            f"arguments, but found "
                            f"{len(meta_arg.children[1].children)} in "
                            f"{str(meta_arg)}.")
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
        if not isinstance(spec_part.children[2], Fortran2003.Type_Bound_Procedure_Part):
            raise ParseError(
                "The metadata does not have a contains keyword (which is "
                "required to add the code metadata.")
        type_bound_procedure = spec_part.children[2]
        content = type_bound_procedure.children[1:]
        for line in content:
            pass # print(line)
        if len(content) != 1:
            raise ParseError(
                f"Expecting a single entry after the 'contains' keyword but "
                f"found {len(content)}.")
        self._routine_name = walk(
            type_bound_procedure,
            Fortran2003.Specific_Binding)[0].children[4].string
