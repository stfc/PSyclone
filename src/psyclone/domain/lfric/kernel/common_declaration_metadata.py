# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing the CommonDeclarationMetadata base class which
captures the common functionality for the LFRic kernel declaration
metadata i.e. metadata that is specified by the declaration of a
variable within the Fortran derived type that captures the metadata.

Declaration metadata is captured as an fparser2 class hierarchy in the
following form (the code in this file assumes this form when
traversing an fparser2 tree):
::

    Data_Component_Def_Stmt
        [0] Intrinsic_Type_Spec
            [0] <str> INTEGER
            [1] <NoneType>
        or
        [0] Declaration_Type_Spec
            [0] <str> TYPE
            [1] Type_Name.string: func_type

        [1] Component_Attr_Spec_List or <NoneType>
            ...
            Dimension_Component_Attr_Spec
                [0] <str> DIMENSION
                [1] Explicit_Shape_Spec_List
                    ...
                    Explicit_Shape_Spec
                        [0] <NoneType>
                        [1] Int_Literal_Constant
                            [0] <str> 1
                            [1] <NoneType>
                    ...
            ...
        [2] Component_Decl_List
            ...
            Component_Decl
                [0] Name.string: gh_shape
                [1] Explicit_Shape_Spec_List or <NoneType>
                    [0] <NoneType>
                    [1] Int_Literal_Constant
                        [0] <str> 1
                        [1] <NoneType>
                [2] <NoneType>
                [3] Component_Initialization
                    [0] <str> =
                    [1] Name.string: gh_quadrature_face
                    *OR*
                    [1] Array_Constructor (/ ... /)
                        [0] <str> (/
                        [1] Ac_Value_List
                            Name.string: gh_quadrature_face
                            ...
                        [2] <str> /)
            ...

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.common_metadata import CommonMetadata
from psyclone.parse.utils import ParseError


class CommonDeclarationMetadata(CommonMetadata):
    '''Class to capture common LFRic kernel declaration metadata.'''

    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Data_Component_Def_Stmt

    @staticmethod
    def scalar_declaration_string(datatype, name, value):
        '''Return the Fortran declaration associated with the datatype, name
        and value arguments.

        :param str datatype: the name of the Fortran datatype.
        :param str name: the name of the variable.
        :param str value: the value of the variable.

        '''
        return f"{datatype} :: {name} = {value}\n"

    @staticmethod
    def array_declaration_string(datatype, name, values):
        '''Return the Fortran declaration associated with the datatype, name
        and value arguments.

        :param str datatype: the name of the Fortran datatype.
        :param str name: the name of the variable.
        :param List[str] values: a list of variable values.

        '''
        values_str = ", ".join(values)
        num_values = len(values)
        return f"{datatype} :: {name}({num_values}) = (/{values_str}/)\n"

    @staticmethod
    def type_declaration_string(datatype, name, values):
        '''Return the Fortran declaration associated with the datatype, name
        and values arguments.

        :param str datatype: the name of the Fortran datatype.
        :param str name: the name of the variable.
        :param List[str] values: a list of variable values.

        '''
        values_str_list = [f"    {value.fortran_string()}" for value in values]
        values_str = ", &\n".join(values_str_list)
        num_values = len(values)
        return (
            f"type({datatype}) :: {name}({num_values}) = "
            f"(/ &\n{values_str}/)\n")

    @staticmethod
    def validate_node(fparser2_node, encoding):
        '''Check that the supplied fparser2_node is of the type specified by
        the encoding argument.

        :param fparser2_node: an fparser2 node.
        :type fparser2_node: subclass of \
            :py:class:`fparser.two.Fortran2003.Base`

        :param encoding: a type of fparser2 node.
        :type encoding: subclass of :py:class:`fparser.two.Fortran2003.Base`

        :raises TypeError: if the type of the supplied fparser2 node \
            is not the same as the supplied encoding.

        '''
        if not isinstance(fparser2_node, encoding):
            raise TypeError(
                f"Expected kernel metadata to be encoded as an "
                f"fparser2 {encoding.__name__} object but found type "
                f"'{type(fparser2_node).__name__}' with value "
                f"'{str(fparser2_node)}'.")

    @staticmethod
    def _validate_derived(fparser2_tree, type_name, name):
        '''Check that the supplied fparser2 tree captures the declaration of a
        derived type of type type_name.

        :param fparser2_tree: an fparser2 tree.
        :type fparser2_tree: subclass of \
            :py:class:`fparser.two.Fortran2003.Base`
        :param str type_name: the expected type name.
        :param str name: the name of the associated metadata.

        :raises TypeError: if the supplied fparser2 tree is not a \
            derived type or the name of the derived type does not match \
            the expected name.

        '''
        # fparser2_tree.children[0] should be an Declaration_Type_Spec
        # and its second child should be a Name containing the name of
        # the type.
        if not (isinstance(fparser2_tree.children[0],
                           Fortran2003.Declaration_Type_Spec) and
                fparser2_tree.children[0].children[1].string.lower() ==
                type_name.lower()):
            raise TypeError(
                f"In its Fortran form, '{name}' metadata should be encoded "
                f"as a 'type({type_name})', but found "
                f"'{str(fparser2_tree.children[0])}' in "
                f"'{str(fparser2_tree)}'.")

    @staticmethod
    def _validate_intrinsic(fparser2_tree, type_name, name):
        '''Check that the supplied fparser2 tree captures the declaration of
        an intrinsic type of type type_name.

        :param fparser2_node: an fparser2 node.
        :type fparser2_node: subclass of \
            :py:class:`fparser.two.Fortran2003.Base`
        :param str type_name: the expected type name.
        :param str name: the name of the associated metadata.

        :raises TypeError: if the supplied fparser2 tree is not an \
            intrinsic type or the name of the intrinsic type does not \
            match the expected name.

        '''
        # fparser2_tree.children[0] should be an Intrinsic_Type_Spec
        # and its first child should be a str containing the name of
        # the type.
        if not (isinstance(fparser2_tree.children[0],
                           Fortran2003.Intrinsic_Type_Spec) and
                fparser2_tree.children[0].children[0].lower() ==
                type_name.lower()):
            raise TypeError(
                f"In its Fortran form, '{name}' metadata should be encoded "
                f"as a {type_name}, but found "
                f"'{str(fparser2_tree.children[0])}' in "
                f"'{str(fparser2_tree)}'.")

    @staticmethod
    def validate_name_value(fparser2_tree, name):
        '''Check that the supplied variable declaration captured as an
        fparser2 tree declares a single variable with the name being
        the supplied 'name' argument and that this variable is
        initialised.

        :param fparser2_tree: an fparser2 tree.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Data_Component_Def_Stmt`
        :param str name: the expected variable name.

        :raises ParseError: if more than one variable is found in the \
            declaration.
        :raises ValueError: if the variable name does not match the \
            expected name.
        :raises ParseError: if the variable is not set to a value.

        '''
        # fparser2_tree.children[2] will be an Component_Decl_List, it
        # should have a single child that is a Component_Decl and the
        # first child of Component_Decl will be a Name containing
        # the name of the metadata.
        component_decl_list = fparser2_tree.children[2]
        num_vars = len(component_decl_list.children)
        if num_vars != 1:
            raise ParseError(
                f"In its Fortran form, '{name}' metadata should only contain "
                f"a single variable, but found '{num_vars}' in "
                f"'{str(fparser2_tree)}'.")
        var_declaration = component_decl_list.children[0]
        var_str = var_declaration.children[0].string
        if var_str.lower() != name.lower():
            raise ValueError(
                f"In its Fortran form, '{name}' metadata should be encoded "
                f"as a variable called '{name}', but found '{var_str}' "
                f"in '{str(fparser2_tree)}'.")
        # The variable should be initialised.
        component_initialisation = component_decl_list.children[0].children[3]
        if not component_initialisation:
            raise ParseError(
                f"'{name}' should be set to a value but none was found, in "
                f"'{str(fparser2_tree)}'.")

    @staticmethod
    def get_intrinsic_scalar_declaration(
            fparser2_tree, datatype, name, valid_values):
        '''Return the value of the variable. Also check that the supplied
        variable declaration captured as an fparser2 tree is an
        intrinsic of type 'datatype' with the specified 'name' which
        is initialised to a value that is one of the supplied
        'valid_values' and if not, raise an exception.

        :param fparser2_tree: an fparser2 tree.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Data_Component_Def_Stmt`
        :param str datatype: the expected intrinsic type name.
        :param str name: the expected variable name.
        :param List[str] valid_values: a list of values that the \
            variable could be set to.

        :returns: the value of the variable.
        :rtype: str

        :raises ParseError: if the variable declaration has any \
            unexpected attributes.
        :raises ParseError: if the scalar variable is initialised with \
            an array of values.

        '''
        CommonDeclarationMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        CommonDeclarationMetadata._validate_intrinsic(
            fparser2_tree, datatype, name)
        CommonDeclarationMetadata.validate_name_value(
            fparser2_tree, name)
        if fparser2_tree.children[1]:
            raise ParseError(
                f"The {datatype} intrinsic in the Fortran representation of "
                f"'{name}' metadata should have no attributes, but found "
                f"'{str(fparser2_tree.children[1])}' in "
                f"'{str(fparser2_tree)}'.")
        component_decl_list = fparser2_tree.children[2]
        var_declaration = component_decl_list.children[0]
        component_initialisation = var_declaration.children[3]
        if var_declaration.children[1]:
            raise ParseError(
                f"The {datatype} intrinsic in the Fortran representation of "
                f"'{name}' metadata should not be declared as an array, but "
                f"found '{str(var_declaration.children[1])}' in "
                f"'{str(fparser2_tree)}'.")
        scalar_value = str(component_initialisation.children[1])
        CommonDeclarationMetadata.validate_scalar_value(
            scalar_value, valid_values, name)
        return scalar_value

    @staticmethod
    def get_intrinsic_array_declaration(
            fparser2_tree, datatype, name, valid_values):
        '''Return the array values. Also check that the supplied variable
        declaration captured as an fparser2 tree is an intrinsic array
        of type 'datatype' with the specified 'name' which is
        initialised to a set of values, each of which are one of the
        supplied 'valid_values' and if not, raise an exception.

        :param fparser2_tree: an fparser2 tree.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Data_Component_Def_Stmt`
        :param str datatype: the expected intrinsic type name.
        :param str name: the expected variable name.
        :param List[str] valid_values: a list of values that the \
            variable could be set to.

        :returns: the values of the variable.
        :rtype: List[str]

        '''
        CommonDeclarationMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        CommonDeclarationMetadata._validate_intrinsic(
            fparser2_tree, datatype, name)
        CommonDeclarationMetadata.validate_name_value(
            fparser2_tree, name)
        values_list = CommonDeclarationMetadata._get_array(
            fparser2_tree, name)
        for value in values_list:
            CommonDeclarationMetadata.validate_scalar_value(
                value, valid_values, name)
        return values_list

    @staticmethod
    def get_derived_array_declaration(
            fparser2_tree, type_name, name, valid_values=None):
        '''Return the array values. Also check that the supplied variable
        declaration captured as an fparser2 tree is an derived type
        array of type 'type_name' with the specified 'name' which is
        initialised to a set of values, each of which are one of the
        supplied 'valid_values' and if not, raise an exception.

        :param fparser2_tree: an fparser2 tree.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Data_Component_Def_Stmt`
        :param str type_name: the expected derived type name.
        :param str name: the expected variable name.
        :param Optional[List[str]] valid_values: a list of values that \
            the variable could be set to. Defaults to None.

        :returns: the values of the variable.
        :rtype: List[str]

        '''
        CommonDeclarationMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        CommonDeclarationMetadata._validate_derived(
            fparser2_tree, type_name, name)
        CommonDeclarationMetadata.validate_name_value(
            fparser2_tree, name)
        values_list = CommonDeclarationMetadata._get_array(
            fparser2_tree, name)
        if valid_values:
            for value in values_list:
                CommonDeclarationMetadata.validate_scalar_value(
                    value, valid_values, name)
        return values_list

    @staticmethod
    def _get_array(fparser2_tree, name):
        '''Return the array values. Also check that the supplied variable
        declaration captured as an fparser2 tree is a one dimensional
        array and if not, raise an exception.

        :param fparser2_tree: an fparser2 tree.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Data_Component_Def_Stmt`
        :param str name: the expected variable name.

        :raises ParseError: if the variable declaration has unexpected \
            attributes.
        :raises ParseError: if this is not an array declaration.
        :raises ValueError: if the array is not one dimensional or the \
            array extent is not an integer value.
        :raises ValueError: if the array extent is less than one.
        :raises ValueError: if the variable is not set to a list of \
            values.
        :raises ParseError: if the length of the variables list of \
            values is not equal to the extent of the array.

        :returns: the values of the variable.
        :rtype: List[str]

        '''
        # Validate array extent.
        component_decl_list = fparser2_tree.children[2]
        var_declaration = component_decl_list.children[0]
        extent_str = None
        if fparser2_tree.children[1]:
            # The integer intrinsic could have a dimension attribute
            if len(fparser2_tree.children[1].children) != 1 or \
               not isinstance(fparser2_tree.children[1].children[0],
                              Fortran2003.Dimension_Component_Attr_Spec):
                raise ParseError(
                    f"The Fortran representation of '{name}' metadata should "
                    f"only have at most one attribute and that attribute "
                    f"should be 'dimension', but found "
                    f"'{str(fparser2_tree)}'.")
            # Get the Fortran representation of
            # Explicit_Shape_Spec_List.
            extent_str = str(
                fparser2_tree.children[1].children[0].children[1])
        elif var_declaration.children[1]:
            # The variable is declared as an array. Get the Fortran
            # representation of Explicit_Shape_Spec_List.
            extent_str = str(var_declaration.children[1])
        if not extent_str:
            raise ParseError(
                f"No dimension declarations found in '{str(fparser2_tree)}'.")
        # extent_str should be a single integer value greater than
        # 0, stored as a string.
        try:
            extent_value = int(extent_str)
        except ValueError as info:
            raise ValueError(
                f"If the Fortran representation of '{name}' metadata is an "
                f"array, it should be one-dimensional with an integer "
                f"value for the dimension extent, but found "
                f"'{extent_str}' in '{str(fparser2_tree)}'.") \
                from info
        if extent_value < 1:
            raise ValueError(
                f"The array extent should be at least 1, but found "
                f"'{extent_value}' in '{str(fparser2_tree)}'.")

        # Validate array values
        component_decl_list = fparser2_tree.children[2]
        component_initialisation = component_decl_list.children[0].children[3]
        array_constructor = component_initialisation.children[1]
        if not isinstance(
                array_constructor, Fortran2003.Array_Constructor):
            raise ValueError(
                f"Expected '{name}' to be set to a list of values, but "
                f"found '{str(array_constructor)}' in "
                f"'{str(fparser2_tree)}'.")
        var_list = []
        for value in array_constructor.children[1].children:
            value_str = str(value)
            var_list.append(value_str)

        if len(var_list) != extent_value:
            raise ParseError(
                f"The array extent '{extent_value}' and number of "
                f"'{name}' values '{len(var_list)}' differ in "
                f"'{str(fparser2_tree)}'.")
        return var_list

    @classmethod
    def validate_list(cls, values, expected_type):
        '''Check that the values argument is a list with at least one entry
        and that its entries are the same type as the expected_type
        argument.

        :param values: a list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArgMetadata`]
        :param expected_type: the type that the values are expected to \
            be.
        :type expected_type: :py:class:`psyclone.domain.lfric.kernel.\
            CommonArgMetadata`

        raises TypeError: if the supplied value is not a list.
        raises TypeError: if the supplied value is an empty list.
        raises TypeError: if any entry in the list is not of the \
            required type.

        '''
        if not isinstance(values, list):
            raise TypeError(f"{cls.__name__} values should be provided as "
                            f"a list but found '{type(values).__name__}'.")
        if not values:
            raise TypeError(
                f"The {cls.__name__} list should contain at least one "
                f"entry, but it is empty.")
        for value in values:
            if not isinstance(value, expected_type):
                raise TypeError(
                    f"The {cls.__name__} list should be a list containing "
                    f"objects of type {expected_type.__name__} but found "
                    f"'{value}', which is of type '{type(value).__name__}'.")


__all__ = ["CommonDeclarationMetadata"]
