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

'''This module contains PSyclone Kernel-layer-specific PSyIR classes
for the GOcean API.

'''
import re

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk, get_child

from psyclone.configuration import Config
from psyclone.domain.gocean import GOceanConstants
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, Symbol, UnknownFortranType


class KernelMetadataSymbol(DataTypeSymbol):
    '''Specialise DataTypeSymbol to capture Kernel Metadata information,
    verify that it conforms to the expected syntax and provide the
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

        :raises InternalError: if the kernel metadata is not stored as \
            a PSyIR UnknownFortranType.
        :raises InternalError: if the kernel metadata is not a derived type.
        :raises ParseError: if meta_args is not a list.
        :raises ParseError: if a meta_arg entry has invalid content.
        :raises ParseError: if a meta_arg entry has an invalid number \
            of arguments.

        '''
        if not isinstance(self.datatype, UnknownFortranType):
            raise InternalError(
                f"Expected kernel metadata to be stored in the PSyIR as "
                f"an UnknownFortranType, but found "
                f"{type(self.datatype).__name__}.")

        # In an UnknownFortranType, the declaration is stored as a
        # string, so create an fparser2 parse tree.
        reader = FortranStringReader(self.datatype.declaration)
        try:
            spec_part = Fortran2003.Derived_Type_Def(reader)
        except Fortran2003.NoMatchError:
            # pylint: disable=raise-missing-from
            raise InternalError(
                f"Expected kernel metadata to be a Fortran derived type, but "
                f"found '{self.datatype.declaration}'.")

        const = GOceanConstants()
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
        # properties. Therefore create appropriate (GridArg, ScalarArg
        # or FieldArg) instances to capture this information.
        meta_args = self._get_property(spec_part, "meta_args")
        args = walk(meta_args, Fortran2003.Ac_Value_List)
        if not args:
            raise ParseError(
                f"meta_args should be a list, but found '{str(meta_args)}' "
                f"in '{spec_part}'.")

        self._meta_args = []
        for meta_arg in args[0].children:
            if len(meta_arg.children[1].children) == 2:
                # Grid args have 2 arguments
                self._meta_args.append(self.GridArg(meta_arg, self))
            elif len(meta_arg.children[1].children) == 3:
                # scalar and field args have 3 arguments
                arg2 = meta_arg.children[1].children[1].string.lower()
                if arg2 in const.VALID_FIELD_GRID_TYPES:
                    self._meta_args.append(self.FieldArg(meta_arg, self))
                elif arg2 in const.VALID_SCALAR_TYPES:
                    self._meta_args.append(self.ScalarArg(meta_arg, self))
                else:
                    raise ParseError(
                        f"Expected a 'meta_arg' entry with 3 arguments to "
                        f"either be a field or a scalar, but found '{arg2}' "
                        f"as the second argument instead of "
                        f"'{const.VALID_FIELD_GRID_TYPES}' (fields) or "
                        f"'{const.VALID_SCALAR_TYPES}' (scalars).")
            else:
                raise ParseError(
                    f"'meta_args' should have either 2 or 3 arguments, but "
                    f"found {len(meta_arg.children[1].children)} in "
                    f"{str(meta_arg)}.")

    def write_fortran_string(self):
        '''
        :returns: the metadata represented by this instance as a Fortran \
            string.
        :rtype: str

        '''
        go_args = []
        for go_arg in self.args:
            go_args.append(go_arg.write_fortran_string())
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
        '''Internal utility that gets the property 'property_name' from an
        fparser2 tree capturing gocean metadata. It is assumed that
        the code property is part of a type bound procedure and that
        the other properties are part of the data declarations.

        :param spec_part: the fparser2 parse tree containing the metadata.
        :type spec_part: :py:class:`fparser.two.Fortran2003.Derived_Type_Def`
        :param str property_name: the name of the property whose value \
            is being extracted from the metadata.

        :returns: the value of the property.
        :rtype: :py:class:`fparser.two.Fortran2003.Name | \
            :py:class:`fparser.two.Fortran2003.Array_Constructor`

        :raises ParseError: if the property name is not found in the \
            metadata.

        '''
        if property_name.lower() == "code":
            # The value of 'code' should be found in a type bound
            # procedure (after the contains keyword)
            type_bound_procedure = get_child(
                spec_part, Fortran2003.Type_Bound_Procedure_Part)
            if not type_bound_procedure:
                raise ParseError(
                    f"No type-bound procedure 'contains' section was found in "
                    f"'{spec_part}'.")
            if len(type_bound_procedure.children) != 2:
                raise ParseError(
                    f"Expecting a single type-bound procedure, but found "
                    f"'{spec_part}'.")
            specific_binding = type_bound_procedure.children[1]
            if not isinstance(specific_binding, Fortran2003.Specific_Binding):
                raise ParseError(
                    f"Expecting a specific binding for the type-bound "
                    f"procedure, but found '{specific_binding}' in "
                    f"'{spec_part}'.")
            binding_name = specific_binding.children[3]
            procedure_name = specific_binding.children[4]
            if binding_name.string.lower() != "code" and procedure_name:
                raise ParseError(
                    f"Expecting the type-bound procedure binding-name to be "
                    f"'code' if there is a procedure name, but found "
                    f"'{str(binding_name)}' in '{spec_part}'.")
            if not procedure_name:
                # Support the alternative metadata format that does
                # not include 'code =>'
                procedure_name = binding_name
            return procedure_name

        # The 'property_name' will be declared within Component_Part.
        component_part = get_child(spec_part, Fortran2003.Component_Part)
        if not component_part:
            raise ParseError(
                f"No type-bound procedure component-part section was found in "
                f"'{spec_part}'.")
        # Each name/value pair will be contained within a Component_Decl
        for component_decl in walk(component_part, Fortran2003.Component_Decl):
            # Component_Decl(Name('name') ...)
            name = component_decl.children[0].string
            if name.lower() == property_name.lower():
                # The value will be contained in a Component_Initialization
                comp_init = get_child(
                    component_decl, Fortran2003.Component_Initialization)
                if not comp_init:
                    raise ParseError(
                        f"No value for property {property_name} was found "
                        f"in '{spec_part}'.")
                # Component_Initialization('=', Name('name'))
                return comp_init.children[1]
        raise ParseError(
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
                f"Expected one of {str(const.VALID_ITERATES_OVER)} for "
                f"'iterates_over' metadata, but found '{value}'.")

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
        self.datatype.declaration = self.write_fortran_string()

    @staticmethod
    def _validate_index_offset(value):
        '''Check that 'value' is a valid 'index_offset' value.

        :param str value: the value to check.

        raises ValueError: if the supplied value is invalid.

        '''
        const = GOceanConstants()
        if value.lower() not in const.SUPPORTED_OFFSETS:
            raise ValueError(
                f"Expected one of {str(const.SUPPORTED_OFFSETS)} for "
                f"'index_offset' metadata, but found '{value}'.")

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
        self.datatype.declaration = self.write_fortran_string()

    @property
    def args(self):
        '''
        :returns: a list of arg objects capturing their metadata values.
        :rtype: List[:py:class:`psyclone.psyir.common.kernel.\
            KernelMetadataSymbol.KernelMetadataArg`]
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
        self.datatype.declaration = self.write_fortran_string()

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
            access = arg_list.children[0].string
            self._validate_access(access)
            self._access = access
            name = arg_list.children[1].string
            self._validate_name(name)
            self._name = name

        def write_fortran_string(self):
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
            if value.lower() not in const.VALID_ACCESS_TYPES:
                raise ValueError(
                    f"The first metadata entry for a grid property argument "
                    f"should be one of {const.VALID_ACCESS_TYPES}, but "
                    f"found '{value}'.")

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
                self._parent.write_fortran_string()

        @staticmethod
        def _validate_name(value):
            '''Check that 'value' is a valid 'name' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            config = Config.get()
            api_config = config.api_conf("gocean1.0")
            grid_property_names = list(api_config.grid_properties.keys())
            if value.lower() not in grid_property_names:
                raise ValueError(
                    f"The second meadata entry for a grid property argument "
                    f"should be one of {grid_property_names}, but found "
                    f"'{value}'.")

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
            self._validate_name(value)
            self._name = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.write_fortran_string()

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

            arg_list = meta_arg.children[1]
            if len(arg_list.children) != 3:
                raise ParseError(
                    f"There should be 3 kernel metadata entries for a field "
                    f"argument, but found {len(arg_list.children)} in "
                    f"{str(meta_arg)}.")

            access = arg_list.children[0].string
            self._validate_access(access)
            self._access = access

            stagger = arg_list.children[1].string
            self._validate_stagger(stagger)
            self._stagger = stagger

            if isinstance(arg_list.children[2], Fortran2003.Name):
                form = arg_list.children[2].string
                self._validate_form(form)
                self._form = form
                self._stencil = None
            else:  # Stencil
                name = arg_list.children[2].children[0].string
                self._validate_stencil_name(name)
                self._form = name
                self._stencil = []
                for stencil_dim in arg_list.children[2].children[1].children:
                    self._stencil.append(stencil_dim.children[0])
                self._validate_stencil(self._stencil)

        def write_fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            if self.stencil:
                return (f"go_arg({self.access}, {self.stagger}, "
                        f"{self.form}({', '.join(self.stencil)}))")
            return f"go_arg({self.access}, {self.stagger}, {self.form})"

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_ACCESS_TYPES:
                raise ValueError(
                    f"The first metadata entry for a field argument should "
                    f"be one of {const.VALID_ACCESS_TYPES}, but found "
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
                self._parent.write_fortran_string()

        @staticmethod
        def _validate_stagger(value):
            '''Check that 'value' is a valid 'stagger' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_FIELD_GRID_TYPES:
                raise ValueError(
                    f"The second metadata entry for a field argument should "
                    f"be one of {const.VALID_FIELD_GRID_TYPES}, but found "
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
                self._parent.write_fortran_string()

        @staticmethod
        def _validate_form(value):
            '''Check that 'value' is a valid 'form' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"The third metadata entry for a field should "
                    f"be one of {const.VALID_STENCIL_NAMES} or "
                    f"'go_stencil(...)', but found '{value}'.")

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
                self._parent.write_fortran_string()

        @staticmethod
        def _validate_stencil_name(value):
            '''Check that value is the expected stencil name.

            :raises ValueError: if an invalid stencil name is found.

            '''
            const = GOceanConstants()
            if value.lower() != const.VALID_STENCIL_NAME:
                raise ValueError(
                    f"The third metadata entry for a field should "
                    f"be {const.VALID_STENCIL_NAME}(...) if it contains "
                    f"arguments, but found '{value}'.")

        @staticmethod
        def _validate_stencil(value_list):
            '''Check that 'value_list' is a valid list of 'stencil' elements.

            :param value_list: the values to check.
            :type value_list: List[str]

            raises TypeError: if the supplied argument is not a list.
            raises ValueError: if the supplied list is not of size 3.
            raises TypeError: if any of the list entries are not \
                strings.
            raises ValueError: if any of the list entries do not \
                conform to the expected format.

            '''
            if not isinstance(value_list, list):
                raise TypeError(
                    f"Stencil entries should be provided as a list, but found "
                    f"'{type(value_list).__name__}'.")
            if len(value_list) != 3:
                raise ValueError(
                    f"If the third metadata entry is a stencil, it should "
                    f"contain 3 arguments, but found "
                    f"{len(value_list)}.")
            pattern = re.compile("[01]{3,3}")
            for value in value_list:
                if not isinstance(value, str):
                    raise TypeError(
                        f"Stencil entries should be strings, but found "
                        f"'{type(value).__name__}'.")
                if not pattern.match(value):
                    raise ValueError(
                        f"Stencil entries should follow the pattern "
                        f"[01]{{3:3}}, but found '{value}'.")

        @property
        def stencil(self):
            '''
            :returns: the stencil value, or None if there is no stencil.
            :rtype: Optional[List[str]]
            '''
            return self._stencil

        @stencil.setter
        def stencil(self, value_list):
            '''
            :param value_list: the new value for stencil.
            :type value_list: List[str]
            '''
            self._validate_stencil(value_list)
            self._stencil = value_list
            if self._form.upper() != "GO_STENCIL":
                self._form = "GO_STENCIL"
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.write_fortran_string()

    class ScalarArg():
        '''Internal class to capture Kernel metadata argument information for
        a scalar.

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

            arg_list = meta_arg.children[1]
            if len(arg_list.children) != 3:
                raise ParseError(
                    f"There should be 3 kernel metadata entries for a scalar "
                    f"argument, but found {len(arg_list.children)} in "
                    f"{str(meta_arg)}.")

            access = arg_list.children[0].string
            self._validate_access(access)
            self._access = access

            datatype = arg_list.children[1].string
            self._validate_datatype(datatype)
            self._datatype = datatype

            form = arg_list.children[2].string
            self._validate_form(form)
            self._form = form

        def write_fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            return f"go_arg({self.access}, {self.datatype}, {self.form})"

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_ACCESS_TYPES:
                raise ValueError(
                    f"The first metadata entry for a scalar argument should "
                    f"be one of {const.VALID_ACCESS_TYPES}, but found "
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
                self._parent.write_fortran_string()

        @staticmethod
        def _validate_datatype(value):
            '''Check that 'value' is a valid scalar 'datatype' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_SCALAR_TYPES:
                raise ValueError(
                    f"The second metadata entry for a scalar argument should "
                    f"be one of {const.VALID_SCALAR_TYPES}, but found "
                    f"'{value}'.")

        @property
        def datatype(self):
            '''
            :returns: the value of access.
            :rtype: str
            '''
            return self._datatype

        @datatype.setter
        def datatype(self, value):
            '''
            :param str value: the new value for datatype.
            '''
            self._validate_datatype(value)
            self._datatype = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.write_fortran_string()

        @staticmethod
        def _validate_form(value):
            '''Check that 'value' is a valid 'form' value.

            :param str value: the value to check.

            raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"The third metadata entry for a scalar should "
                    f"be one of {const.VALID_STENCIL_NAMES}, but "
                    f"found '{value}'.")

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
                self._parent.write_fortran_string()
