# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
metadata.

'''
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric import LFRicConstants
from psyclone.parse.utils import ParseError


class CommonDeclarationMetadata:
    ''' xxx '''

    def scalar_declaration_string(datatype, name, value):
        ''' xxx '''
        return f"{datatype} :: {name} = {value}\n"

    def array_declaration_string(datatype, name, values):
        ''' xxx '''
        values_str = ", ".join(values)
        num_values = len(values)
        return f"{datatype} :: {name}({num_values}) = (/{values_str}/)\n"

    def type_declaration_string(datatype, name, values):
        ''' xxx '''
        values_str_list = [value.fortran_string() for value in values]
        values_str = ", ".join(values_str_list)
        num_values = len(values)
        return (
            f"type({datatype}) :: {name}({num_values}) = (/{values_str}/)\n")

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from Fortran.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of ShapesMetadata
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ShapesMetadata`

        '''
        fparser2_tree = cls.create_fparser2(
            fortran_string, Fortran2003.Data_Component_Def_Stmt)
        return cls.create_from_fparser2(fparser2_tree)

    @staticmethod
    def create_fparser2(fortran_string, encoding):
        '''Creates an fparser2 tree from a Fortran string. The resultant
        parent node of the tree will be the same type as the encoding
        argument if the string conforms to the encoding, otherwise an
        exception will be raised.

        :param str fortran_string: a string containing the metadata in \
           Fortran.
        :param encoding: the parent class with which we will encode the \
            Fortran string.
        :type encoding: type

        :returns: an fparser2 tree containing a metadata \
            argument.
        :rtype: :py:class:`fparser.two.Fortran2003.Base`

        :raises ValueError: if the Fortran string is not in the \
            expected form.

        '''
        # todo, integrate with common_arg
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        fparser2_tree = encoding(reader)
        if not fparser2_tree:
            raise ValueError(
                f"Expected kernel metadata to be a Fortran "
                f"{encoding.__name__}, but found '{fortran_string}'.")
        return fparser2_tree

    def validate_scalar_value(value, valid_values, name):
        ''' xxx '''
        if value.lower() not in valid_values:
            raise ValueError(
                f"The {name} metadata should be a recognised "
                f"value (one of {valid_values}) "
                f"but found '{value}'.")

    def validate_node(fparser2_node, encoding):
        ''' xxx '''
        if not isinstance(fparser2_node, encoding):
            raise TypeError(
                f"Expected kernel metadata to be encoded as an "
                f"fparser2 {encoding.__name__} object but found type "
                f"'{type(fparser2_node).__name__}' with value "
                f"'{str(fparser2_node)}'.")

    def validate_derived(fparser2_tree, type_name, name):
        ''' xxx '''
        # fparser2_tree.children[0] should be an Intrinsic_Type_Spec
        # and its first child should be a str containing "datatype"
        if not (isinstance(fparser2_tree.children[0],
                           Fortran2003.Declaration_Type_Spec) and
                fparser2_tree.children[0].children[1].string.lower() ==
                type_name.lower()):
            raise TypeError(
                f"In Fortran, {name} metadata should be encoded as a "
                f"'type({type_name})', but found "
                f"'{str(fparser2_tree.children[0])}' in "
                "'{str(fparser2_tree)}'.")

    def validate_intrinsic(fparser2_tree, datatype, name):
        ''' xxx '''
        # fparser2_tree.children[0] should be an Intrinsic_Type_Spec
        # and its first child should be a str containing "datatype"
        if not (isinstance(fparser2_tree.children[0],
                           Fortran2003.Intrinsic_Type_Spec) and
                fparser2_tree.children[0].children[0].lower() ==
                datatype.lower()):
            raise TypeError(
                f"In Fortran, {name} metadata should be encoded as an "
                f"{datatype}, but found '{str(fparser2_tree.children[0])}' "
                f"in '{str(fparser2_tree)}'.")

    def validate_datatype_name_value(fparser2_tree, datatype, name):
        ''' xxx '''

        # GH_SHAPE
        # fparser2_tree.children[2] will be an Component_Decl_List, it
        # should have a single child that is a Component_Decl and the
        # first child of Component_Decl will be a Name containing
        # "gh_shape"
        component_decl_list = fparser2_tree.children[2]
        num_vars = len(component_decl_list.children)
        if num_vars != 1:
            raise ParseError(
                f"In Fortran, {name} metadata should only contain a single "
                f"variable, but found '{num_vars}' in '{str(fparser2_tree)}'.")
        gh_shape_declaration = component_decl_list.children[0]
        gh_shape_str = gh_shape_declaration.children[0].string
        if gh_shape_str.lower() != name.lower():
            raise ValueError(
                f"In Fortran, {name} metadata should be encoded as a "
                f"variable called {name}, but found '{gh_shape_str}' "
                f"in '{str(fparser2_tree)}'.")
        component_initialisation = component_decl_list.children[0].children[3]
        if not component_initialisation:
            raise ParseError(
                f"{name} should be set to a value but none was found, in "
                f"'{str(fparser2_tree)}'.")

    def validate_intrinsic_scalar_declaration(
            fparser2_tree, datatype, name, valid_values):
        ''' xxx '''
        CommonDeclarationMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        CommonDeclarationMetadata.validate_intrinsic(
            fparser2_tree, "INTEGER", name)
        CommonDeclarationMetadata.validate_datatype_name_value(
            fparser2_tree, datatype, name)
        if fparser2_tree.children[1]:
            raise ParseError(
                f"The integer intrinsic in the Fortran representation of "
                f"{name} metadata should have no attributes, but found "
                f"'{str(fparser2_tree.children[1])}' in "
                f"'{str(fparser2_tree)}'.")
        component_decl_list = fparser2_tree.children[2]
        gh_shape_declaration = component_decl_list.children[0]
        component_initialisation = gh_shape_declaration.children[3]
        if gh_shape_declaration.children[1]:
            raise ParseError(
                f"The integer intrinsic in the Fortran representation of "
                f"{name} metadata should not be declared as an array, but "
                f"found '{str(gh_shape_declaration.children[1])}' in "
                f"'{str(fparser2_tree)}'.")
        scalar_value = str(component_initialisation.children[1])
        CommonDeclarationMetadata.validate_scalar_value(
            scalar_value, valid_values, name)
        return scalar_value

    def validate_intrinsic_array_declaration(
            fparser2_tree, datatype, name, valid_values):
        ''' xxx '''
        CommonDeclarationMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        CommonDeclarationMetadata.validate_intrinsic(
            fparser2_tree, datatype, name)
        CommonDeclarationMetadata.validate_datatype_name_value(
            fparser2_tree, datatype, name)
        shapes_list = CommonDeclarationMetadata.validate_array(
            fparser2_tree, name)
        for shape in shapes_list:
            CommonDeclarationMetadata.validate_scalar_value(
                shape, valid_values, name)
        return shapes_list

    def validate_derived_array_declaration(
            fparser2_tree, type_name, name):
        ''' xxx '''
        CommonDeclarationMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        CommonDeclarationMetadata.validate_derived(
            fparser2_tree, type_name, name)
        CommonDeclarationMetadata.validate_datatype_name_value(
            fparser2_tree, type_name, name)
        values_list = CommonDeclarationMetadata.validate_array(
            fparser2_tree, name)
        return values_list

    def validate_array(fparser2_tree, name):
        # validate array extent
        component_decl_list = fparser2_tree.children[2]
        gh_shape_declaration = component_decl_list.children[0]
        extent_str = None
        if fparser2_tree.children[1]:
            # The integer intrinsic could have a dimension attribute
            if len(fparser2_tree.children[1].children) != 1 or \
               not isinstance(fparser2_tree.children[1].children[0],
                              Fortran2003.Dimension_Component_Attr_Spec):
                raise ParseError(
                    f"The integer intrinsic in the Fortran representation of "
                    f"{name} metadata should only have at most one attribute "
                    f"and that attribute should be 'dimension', but found "
                    f"'{str(fparser2_tree)}'.")
            # Get the Fortran representation of
            # Explicit_Shape_Spec_List.
            extent_str = str(
                fparser2_tree.children[1].children[0].children[1])
        elif gh_shape_declaration.children[1]:
            # The gh_shape is declared as an array. Get the Fortran
            # representation of Explicit_Shape_Spec_List.
            extent_str = str(gh_shape_declaration.children[1])
        if not extent_str:
            raise ParseError(
                f"No dimension declarations found in '{str(fparser2_tree)}'.")
        # extent_str should be a single integer value greater than
        # 0, stored as a string.
        try:
            extent_value = int(extent_str)
        except ValueError as info:
            raise ValueError(
                f"If the Fortran representation of {name} metadata is an "
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
                f"Expected {name} to be set to a list of values, but "
                f"found '{str(array_constructor)}' in "
                f"'{str(fparser2_tree)}'.")
        shapes_list = []
        for value in array_constructor.children[1].children:
            value_str = str(value)
            shapes_list.append(value_str)

        if len(shapes_list) != extent_value:
            raise ParseError(
                f"The array extent '{extent_value}' and number of "
                f"{name} values '{len(shapes_list)}' differ in "
                f"'{str(fparser2_tree)}'.")
        return shapes_list

    @staticmethod
    def create_from_fparser2_ignore(fparser2_tree):
        '''Create an instance of ShapesMetadata from an fparser2 tree.

        LFRic shape metadata can have a scalar and array form. Two
        versions of the array form are supported:

        integer :: gh_shape = gh_quadrature_face
        integer :: gh_shape(2) = (/ gh_quadrature_face, gh_evaluator /)
        integer, dimension(2) :: gh_shape = &
                 (/ gh_quadrature_face, gh_evaluator /)

        The fparser2 class hierarchy for the different flavours
        of valid gh_shape metadata above is given below:

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

        :param fparser2_tree: fparser2 tree capturing the shapes metadata
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of ShapesMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ShapesMetadata`

        '''
