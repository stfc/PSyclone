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
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import DataTypeSymbol, Symbol, UnknownFortranType


class GOceanKernel(Routine):
    '''A GOcean-specific kernel. This specialises the generic Routine node
    and adds in any domain-specific information.

    '''
    def metadata(self):
        ''' GOcean metadata '''
        return self._metadata

    def lower(self):
        ''' xxx '''
        pass

class GOceanKernelMetadata():
    ''' xxx '''

    def __init__(self):
        self._itererates_over = None
        self._index_offset = None
        self._meta_args = None
        self._code = None

    def lower_to_psyir(self):
        ''' xxx '''
        return UnknownFortranType(self.fortran_string())
        
    @staticmethod
    def create_from_psyir(datatype):
        ''' xxx '''
        if not isinstance(datatype, UnknownFortranType):
            raise InternalError(
                f"Expected kernel metadata to be stored in the PSyIR as "
                f"an UnknownFortranType, but found "
                f"{type(datatype).__name__}.")

        kernel_metadata = GOceanKernelMetadata()

        # In an UnknownFortranType, the declaration is stored as a
        # string, so create an fparser2 parse tree.
        reader = FortranStringReader(datatype.declaration)
        try:
            spec_part = Fortran2003.Derived_Type_Def(reader)
        except Fortran2003.NoMatchError:
            # pylint: disable=raise-missing-from
            raise InternalError(
                f"Expected kernel metadata to be a Fortran derived type, but "
                f"found '{datatype.declaration}'.")

        const = GOceanConstants()
        # Extract and store the required 'iterates_over',
        # 'index_offset' and 'code' properties from the parse tree

        # the value of iterates over (go_all_pts, ...)
        value = GOceanKernelMetadata._get_property(spec_part, "iterates_over").string
        kernel_metadata.iterates_over = value

        # the value of index offset (NE, ...)
        value = GOceanKernelMetadata._get_property(spec_part, "index_offset").string
        kernel_metadata.index_offset = value

        # the name of the procedure that this metadata refers to.
        kernel_metadata.procedure = GOceanKernelMetadata._get_property(spec_part, "code").string

        # meta_args contains arguments which have
        # properties. Therefore create appropriate (GridArg, ScalarArg
        # or FieldArg) instances to capture this information.
        kernel_metadata._meta_args = GOceanKernelMetadata._get_property(spec_part, "meta_args")
        args = walk(kernel_metadata.meta_args, Fortran2003.Ac_Value_List)
        if not args:
            raise ParseError(
                f"meta_args should be a list, but found '{str(meta_args)}' "
                f"in '{spec_part}'.")

        kernel_metadata._meta_args = []
        for meta_arg in args[0].children:
            if len(meta_arg.children[1].children) == 2:
                # Grid args have 2 arguments
                kernel_metadata.meta_args.append(GOceanKernelMetadata.GridArg(meta_arg, kernel_metadata))
            elif len(meta_arg.children[1].children) == 3:
                # scalar and field args have 3 arguments
                arg2 = meta_arg.children[1].children[1].string.lower()
                if arg2 in const.VALID_FIELD_GRID_TYPES:
                    kernel_metadata.meta_args.append(GOceanKernelMetadata.FieldArg(meta_arg, kernel_metadata))
                elif arg2 in const.VALID_SCALAR_TYPES:
                    kernel_metadata.meta_args.append(GOceanKernelMetadata.ScalarArg(meta_arg, kernel_metadata))
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

        return kernel_metadata

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

    def fortran_string(self):
        '''
        :returns: the metadata represented by this instance as a Fortran \
            string.
        :rtype: str
        '''
        go_args = []
        for go_arg in self.meta_args:
            go_args.append(go_arg.fortran_string())
        go_args_str = ", ".join(go_args)
        result = (
            f"TYPE, EXTENDS(kernel_type) :: {self.name}\n"
            f"TYPE(go_arg), DIMENSION({len(self.meta_args)}) :: "
            f"meta_args = (/{go_args_str}/)\n"
            f"  INTEGER :: ITERATES_OVER = {self.iterates_over}\n"
            f"  INTEGER :: index_offset = {self.index_offset}\n"
            f"  CONTAINS\n"
            f"  PROCEDURE, NOPASS :: code => {self.procedure}\n"
            f"END TYPE {self.name}\n")
        return result

    @staticmethod
    def _validate_iterates_over(value):
        '''Check that 'value' is a valid 'iterates_over' value (go_all_pts,
        ...).

        :param str value: the value to check.

        :raises ValueError: if the supplied value is invalid.

        '''
        const = GOceanConstants()
        if value.lower() not in const.VALID_ITERATES_OVER:
            raise ValueError(
                f"Expected one of {str(const.VALID_ITERATES_OVER)} for "
                f"'iterates_over' metadata, but found '{value}'.")

    @property
    def iterates_over(self):
        '''
        :returns: the name of the quantity that this kernel is intended to \
            iterate over.
        :rtype: str
        '''
        return self._iterates_over

    @iterates_over.setter
    def iterates_over(self, value):
        '''
        :param str value: set the iterates_over metadata to the \
            specified value.
        '''
        self._validate_iterates_over(value)
        self._iterates_over = value

    @staticmethod
    def _validate_index_offset(value):
        '''Check that 'value' is a valid 'index_offset' value (go_offset_ne,
        ...).

        :param str value: the value to check.

        :raises ValueError: if the supplied value is invalid.

        '''
        const = GOceanConstants()
        if value.lower() not in const.SUPPORTED_OFFSETS:
            raise ValueError(
                f"Expected one of {str(const.SUPPORTED_OFFSETS)} for "
                f"'index_offset' metadata, but found '{value}'.")

    @property
    def index_offset(self):
        '''
        :returns: the name of the quantity that specifies the index \
            offset (how different field indices relate to each other).
        :rtype: str
        '''
        return self._index_offset

    @index_offset.setter
    def index_offset(self, value):
        '''
        :param str value: set the index_offset metadata to the \
          specified value.
        '''
        self._validate_index_offset(value)
        self._index_offset = value

    @property
    def meta_args(self):
        '''
        :returns: a list of 'meta_arg' objects which capture the \
            metadata values of the kernel arguments.
        :rtype: List[:py:class:`psyclone.psyir.common.kernel.\
            KernelMetadataSymbol.KernelMetadataArg`]
        '''
        return self._meta_args

    @property
    def procedure(self):
        '''
        :returns: the kernel procedure name specified by the metadata.
        :rtype: str
        '''
        return self._code

    @procedure.setter
    def procedure(self, value):
        '''
        :param str value: set the procedure name specified in the \
            metadata to the specified value.
        '''
        self._code = value

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

            # We do not use the setters here for setting the values of
            # access and name as the setters update the underlying
            # string representation and we don't want/need to do that
            # as we are actually using the value of the string
            # representation to set these values.

            # access descriptor (read, write, ...)
            access = arg_list.children[0].string
            self._validate_access(access)
            self._access = access

            # name of the grid property (grid_mask_t, ...)
            name = arg_list.children[1].string
            self._validate_name(name)
            self._name = name

        def fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            return f"go_arg({self.access}, {self.name})"

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value (go_read, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_ACCESS_TYPES:
                raise ValueError(
                    f"The first metadata entry for a grid property argument "
                    f"should be a valid access descriptor (one of "
                    f"{const.VALID_ACCESS_TYPES}), but found '{value}'.")

        @property
        def access(self):
            '''
            :returns: the value of the access descriptor. This \
                specifies how the grid property is accessed (read, write, \
                readwrite).
            :rtype: str
            '''
            return self._access

        @access.setter
        def access(self, value):
            '''
            :param str value: set the access descriptor for this grid \
                property to the specified value.
            '''
            self._validate_access(value)
            self._access = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

        @staticmethod
        def _validate_name(value):
            '''Check that 'value' is a valid 'name' value for a GOcean grid
            property (go_grid_mask_t, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            config = Config.get()
            api_config = config.api_conf("gocean1.0")
            grid_property_names = list(api_config.grid_properties.keys())
            if value.lower() not in grid_property_names:
                raise ValueError(
                    f"The second metadata entry for a grid property argument "
                    f"should have a valid name (one of "
                    f"{grid_property_names}), but found '{value}'.")

        @property
        def name(self):
            '''
            :returns: the grid property name as specified by the metadata.
            :rtype: str
            '''
            return self._name

        @name.setter
        def name(self, value):
            '''
            :param str value: set the grid property name to the \
                specified value.
            '''
            self._validate_name(value)
            self._name = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

    class FieldArg():
        '''Internal class to capture Kernel metadata information for
        a field argument.

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

            # We do not use the setters here for setting the values of
            # access, grid_point_type, form and stencil as the setters
            # update the underlying string representation and we don't
            # want/need to do that as we are actually using the value
            # of the string representation to set these values.

            # access descriptor (go_read, go_write, ...)
            access = arg_list.children[0].string
            self._validate_access(access)
            self._access = access

            # grid point type (go_ct, ...)
            grid_point_type = arg_list.children[1].string
            self._validate_grid_point_type(grid_point_type)
            self._grid_point_type = grid_point_type

            if isinstance(arg_list.children[2], Fortran2003.Name):
                # form of access (go_pointwise, ...)
                form = arg_list.children[2].string
                self._validate_form(form)
                self._form = form
                self._stencil = None
            else:  # Stencil form (go_stencil) and stencil value
                # (e.g. [000, 111, 000])
                name = arg_list.children[2].children[0].string
                self._validate_stencil_name(name)
                self._form = name
                self._stencil = []
                for stencil_dim in arg_list.children[2].children[1].children:
                    self._stencil.append(stencil_dim.children[0])
                self._validate_stencil(self._stencil)

        def fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            if self.stencil:
                return (f"go_arg({self.access}, {self.grid_point_type}, "
                        f"{self.form}({', '.join(self.stencil)}))")
            return (f"go_arg({self.access}, {self.grid_point_type}, "
                    f"{self.form})")

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value (go_read, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_ACCESS_TYPES:
                raise ValueError(
                    f"The first metadata entry for a field argument should "
                    f"be a recognised name (one of "
                    f"{const.VALID_ACCESS_TYPES}), but found '{value}'.")

        @property
        def access(self):
            '''
            :returns: the access descriptor for this field \
                argument.
            :rtype: str
            '''
            return self._access

        @access.setter
        def access(self, value):
            '''
            :param str value: set the access descriptor to the \
                specified value.
            '''
            self._validate_access(value)
            self._access = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

        @staticmethod
        def _validate_grid_point_type(value):
            '''Check that 'value' is a valid 'grid_point_type' value
            (go_ct, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_FIELD_GRID_TYPES:
                raise ValueError(
                    f"The second metadata entry for a field argument should "
                    f"be a recognised name (one of "
                    f"{const.VALID_FIELD_GRID_TYPES}), but found '{value}'.")

        @property
        def grid_point_type(self):
            '''
            :returns: the value of the grid point type (go_ct, ...) \
                for the field argument.
            :rtype: str
            '''
            return self._grid_point_type

        @grid_point_type.setter
        def grid_point_type(self, value):
            '''
            :param str value: set the field grid point type (ct, ...) \
                to the specified value.
            '''
            self._validate_grid_point_type(value)
            self._grid_point_type = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

        @staticmethod
        def _validate_form(value):
            '''Check that 'value' is a valid 'form' value (go_pointwise, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"The third metadata entry for a field should "
                    f"be a recognised name (one of "
                    f"{const.VALID_STENCIL_NAMES} or 'go_stencil(...)'), "
                    f"but found '{value}'.")

        @property
        def form(self):
            '''
            :returns: the form of access for the field (pointwise, \
                stencil, ...).
            :rtype: str
            '''
            return self._form

        @form.setter
        def form(self, value):
            '''
            :param str value: set the form of access for the field to \
                the specified value.
            '''
            self._validate_form(value)
            self._form = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

        @staticmethod
        def _validate_stencil_name(value):
            '''Check that value is the expected stencil name (go_stencil).

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
            '''Check that 'value_list' is a valid list of 'stencil' elements
            (e.g. [000, 111, 000]).

            :param value_list: the values to check.
            :type value_list: List[str]

            :raises TypeError: if the supplied argument is not a list.
            :raises ValueError: if the supplied list is not of size 3.
            :raises TypeError: if any of the list entries are not strings.
            :raises ValueError: if any of the list entries do not \
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
                    f"{len(value_list)} in {value_list}.")
            # Match 3 integers which can each have the value 0 or 1
            # (e.g. 000, 010, 111, ...)
            pattern = re.compile("[01]{3,3}")
            for value in value_list:
                if not isinstance(value, str):
                    raise TypeError(
                        f"Stencil entries should be strings, but found "
                        f"'{type(value).__name__}'.")
                if not pattern.match(value):
                    raise ValueError(
                        f"Stencil entries should follow the pattern "
                        f"[01]{{3,3}}, but found '{value}'.")

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
            :param value_list: set the new stencil value, encoded as \
                three strings, each of three digits (0 or 1), see the \
                `psyclone user guide <https://psyclone.readthedocs.io/en/\
stable/gocean1p0.html#argument-metadata-meta-args>` \
                for more details.
            :type value_list: List[str]

            '''
            self._validate_stencil(value_list)
            self._stencil = value_list
            # If form was not GO_STENCIL, we need to set it to
            # GO_STENCIL now that we are providing a stencil value as
            # the format is GO_STENCIL(stencil) which is _form (
            # _stencil ).
            if self._form.upper() != "GO_STENCIL":
                self._form = "GO_STENCIL"
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

    class ScalarArg():
        '''Internal class to capture Kernel metadata information for
        a scalar argument.

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

            # We do not use the setters here for setting the values of
            # access, datatype and form as the setters update the
            # underlying string representation and we don't want/need
            # to do that as we are actually using the value of the
            # string representation to set these values.

            # access descriptor (read, write, ...)
            access = arg_list.children[0].string
            self._validate_access(access)
            self._access = access

            # datatype (real, integer, ...)
            datatype = arg_list.children[1].string
            self._validate_datatype(datatype)
            self._datatype = datatype

            # form of access (pointwise)
            form = arg_list.children[2].string
            self._validate_form(form)
            self._form = form

        def fortran_string(self):
            '''
            :returns: the metadata represented by this class as a \
                Fortran string.
            :rtype: str
            '''
            return f"go_arg({self.access}, {self.datatype}, {self.form})"

        @staticmethod
        def _validate_access(value):
            '''Check that 'value' is a valid 'access' value (go_read, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_ACCESS_TYPES:
                raise ValueError(
                    f"The first metadata entry for a scalar argument should "
                    f"be a recognised name (one of "
                    f"{const.VALID_ACCESS_TYPES}), but found '{value}'.")

        @property
        def access(self):
            '''
            :returns: the access descriptor for this scalar argument.
            :rtype: str
            '''
            return self._access

        @access.setter
        def access(self, value):
            '''
            :param str value: set the access descriptor for this
                scalar argument to the specified value.
            '''
            self._validate_access(value)
            self._access = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

        @staticmethod
        def _validate_datatype(value):
            '''Check that 'value' is a valid scalar 'datatype' value
            (go_r_scalar, ...).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_SCALAR_TYPES:
                raise ValueError(
                    f"The second metadata entry for a scalar argument should "
                    f"be a recognised name (one of "
                    f"{const.VALID_SCALAR_TYPES}), but found '{value}'.")

        @property
        def datatype(self):
            '''
            :returns: the datatype of the scalar argument.
            :rtype: str
            '''
            return self._datatype

        @datatype.setter
        def datatype(self, value):
            '''
            :param str value: set the scalar datatype to the specified \
                value.
            '''
            self._validate_datatype(value)
            self._datatype = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()

        @staticmethod
        def _validate_form(value):
            '''Check that 'value' is a valid 'form' value (go_pointwise).

            :param str value: the value to check.

            :raises ValueError: if the supplied value is invalid.

            '''
            const = GOceanConstants()
            if value.lower() not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"The third metadata entry for a scalar should "
                    f"be a recognised name (one of "
                    f"{const.VALID_STENCIL_NAMES}), but found '{value}'.")

        @property
        def form(self):
            '''
            :returns: the form of access for the scalar (pointwise ...).
            :rtype: str
            '''
            return self._form

        @form.setter
        def form(self, value):
            '''
            :param str value: set the form of access for the scalar to \
                the specified value.
            '''
            self._validate_form(value)
            self._form = value
            # Update the underlying string representation of the datatype.
            self._parent.datatype.declaration = \
                self._parent.fortran_string()



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


__all__ = ['KernelMetadataSymbol']
