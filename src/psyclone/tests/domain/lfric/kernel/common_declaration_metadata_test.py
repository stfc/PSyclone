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
# Authors: R. W. Ford and N. Nobre, STFC Daresbury Lab

'''Module containing tests for the CommonDeclarationMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import (
    CommonDeclarationMetadata, MetaFuncsArgMetadata)
from psyclone.parse.utils import ParseError


def test_scalar_declaration_string():
    '''Test that the scalar_declaration_string method behaves as
    expected.

    '''
    result = CommonDeclarationMetadata.scalar_declaration_string(
        "datatype", "name", "value")
    assert result == "datatype :: name = value\n"


def test_array_declaration_string():
    '''Test that the array_declaration_string method behaves as
    expected.

    '''
    result = CommonDeclarationMetadata.array_declaration_string(
        "datatype", "name", ["value1", "value2"])
    assert result == "datatype :: name(2) = (/value1, value2/)\n"


def test_type_declaration_string():
    '''Test that the type_declaration_string method behaves as
    expected. Make use of the existing MetaFuncsArgMetadata class to
    perform the test.

    '''
    args = [MetaFuncsArgMetadata("w0", basis_function=True),
            MetaFuncsArgMetadata("w1", basis_function=True)]
    result = CommonDeclarationMetadata.type_declaration_string(
        "datatype", "name", args)
    assert (result == "type(datatype) :: name(2) = (/ &\n"
            "    func_type(w0, gh_basis), &\n"
            "    func_type(w1, gh_basis)/)\n")


def test_validate_node():
    '''Test that the validate_node method behaves as expected.'''

    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.validate_node(
            Fortran2003.Name("hello"), Fortran2003.Comment)
    assert ("Expected kernel metadata to be encoded as an fparser2 Comment "
            "object but found type 'Name' with value 'hello'."
            in str(info.value))
    CommonDeclarationMetadata.validate_node(
        Fortran2003.Name("hello"), Fortran2003.Name)


@pytest.mark.usefixtures("parser")
def test_validate_derived():
    '''Test that the validate_derived method behaves as expected.'''

    # Child is not declaration_type_spec (it is intrinsic_type_space).
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata._validate_derived(
            fparser2_tree, "my_type", "metadata_name")
    assert ("In its Fortran form, 'metadata_name' metadata should be "
            "encoded as a 'type(my_type)', but found 'INTEGER' in "
            "'INTEGER :: my_var'." in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var")

    # Incorrect type name.
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata._validate_derived(
            fparser2_tree, "invalid_type", "metadata_name")
    assert ("In its Fortran form, 'metadata_name' metadata should be encoded "
            "as a 'type(invalid_type)', but found 'TYPE(my_type)' in "
            "'TYPE(my_type) :: my_var'." in str(info.value))

    CommonDeclarationMetadata._validate_derived(
        fparser2_tree, "my_type", "metadata_name")


@pytest.mark.usefixtures("parser")
def test_validate_intrinsic():
    '''Test that the validate_intrinsic method behaves as expected.'''

    # Child is not intrinsic_type_spec (it is declaration_type_spec).
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata._validate_intrinsic(
            fparser2_tree, "integer", "my_metadata")
    assert ("In its Fortran form, 'my_metadata' metadata should be encoded "
            "as a integer, but found 'TYPE(my_type)' in 'TYPE(my_type) :: "
            "my_var'." in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var")

    # Incorrect intrinsic type.
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata._validate_intrinsic(
            fparser2_tree, "real", "my_metadata")
    assert ("In its Fortran form, 'my_metadata' metadata should be encoded "
            "as a real, but found 'INTEGER' in 'INTEGER :: my_var'."
            in str(info.value))

    CommonDeclarationMetadata._validate_intrinsic(
        fparser2_tree, "integer", "my_metadata")


@pytest.mark.usefixtures("parser")
def test_validate_name_value():
    '''Test that the validate_name_value method behaves as expected.'''

    # More than one variable declared.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: var1, var2\n")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.validate_name_value(
            fparser2_tree, "my_var")
    assert ("In its Fortran form, 'my_var' metadata should only contain a "
            "single variable, but found '2' in 'INTEGER :: var1, var2'."
            in str(info.value))

    # Incorrect variable name.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: invalid\n")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata.validate_name_value(
            fparser2_tree, "my_var")
    assert ("In its Fortran form, 'my_var' metadata should be encoded as a "
            "variable called 'my_var', but found 'invalid' in 'INTEGER :: "
            "invalid'." in str(info.value))

    # No initial value.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var\n")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.validate_name_value(
            fparser2_tree, "my_var")
    assert ("'my_var' should be set to a value but none was found, in "
            "'INTEGER :: my_var'." in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var = value\n")
    CommonDeclarationMetadata.validate_name_value(fparser2_tree, "my_var")


@pytest.mark.usefixtures("parser")
def test_get_intrinsic_scalar_declaration():
    '''Test that the get_intrinsic_scalar_declaration method behaves
    as expected.

    '''
    # Calls validate_node.
    fparser2_tree = Fortran2003.Name("hello")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Data_Component_Def_Stmt object but found type 'Name' with "
            "value 'hello'." in str(info.value))

    # Calls _validate_intrinsic.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("In its Fortran form, 'my_var' metadata should be encoded as a "
            "integer, but found 'TYPE(my_type)' in 'TYPE(my_type) :: my_var'."
            in str(info.value))

    # Calls validate_name_value.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("Parse Error: 'my_var' should be set to a value but none was "
            "found, in 'INTEGER :: my_var'." in str(info.value))

    # Has additional attributes.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer, pointer :: my_var = value1")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("The integer intrinsic in the Fortran representation of 'my_var' "
            "metadata should have no attributes, but found 'POINTER' in "
            "'INTEGER, POINTER :: my_var = value1'." in str(info.value))

    # Is an array.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(1) = (/value1/)")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("The integer intrinsic in the Fortran representation of 'my_var' "
            "metadata should not be declared as an array, but found '1' in "
            "'INTEGER :: my_var(1) = (/value1/)'." in str(info.value))

    # Calls validate_scalar_value.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var = invalid")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("The 'my_var' metadata should be a recognised value (one of "
            "['value1', 'value2']) but found 'invalid'." in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var = value2\n")
    result = CommonDeclarationMetadata.get_intrinsic_scalar_declaration(
        fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert result == "value2"


@pytest.mark.usefixtures("parser")
def test_get_intrinsic_array_declaration():
    '''Test that the get_intrinsic_array_declaration method behaves as
    expected.

    '''
    # Calls validate_node.
    fparser2_tree = Fortran2003.Name("hello")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.get_intrinsic_array_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Data_Component_Def_Stmt object but found type 'Name' with "
            "value 'hello'." in str(info.value))

    # Calls _validate_intrinsic.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var(2)")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.get_intrinsic_array_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("In its Fortran form, 'my_var' metadata should be encoded as a "
            "integer, but found 'TYPE(my_type)' in 'TYPE(my_type) :: "
            "my_var(2)'." in str(info.value))

    # Calls validate_name_value.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(2)")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_intrinsic_array_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("Parse Error: 'my_var' should be set to a value but none was "
            "found, in 'INTEGER :: my_var(2)'." in str(info.value))

    # Calls _get_array.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var = x")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_intrinsic_array_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("Parse Error: No dimension declarations found in "
            "'INTEGER :: my_var = x'." in str(info.value))

    # Calls validate_scalar_value.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(1) = (/invalid/)")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata.get_intrinsic_array_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("The 'my_var' metadata should be a recognised value (one of "
            "['value1', 'value2']) but found 'invalid'." in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(2) = (/value1, value2/)")
    result = CommonDeclarationMetadata.get_intrinsic_array_declaration(
        fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert result == ["value1", "value2"]


@pytest.mark.usefixtures("parser")
def test_get_derived_array_declaration():
    '''Test that the get_derived_array_declaration method behaves
    as expected.

    '''
    # Calls validate_node.
    fparser2_tree = Fortran2003.Name("hello")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.get_derived_array_declaration(
            fparser2_tree, "integer", "my_var", ["value1", "value2"])
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Data_Component_Def_Stmt object but found type 'Name' with "
            "value 'hello'." in str(info.value))

    # Calls _validate_derived.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var")
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.get_derived_array_declaration(
            fparser2_tree, "my_type", "my_var", ["value1", "value2"])
    assert ("In its Fortran form, 'my_var' metadata should be encoded as a "
            "'type(my_type)', but found 'INTEGER' in 'INTEGER :: my_var'."
            in str(info.value))

    # Calls validate_name_value.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_derived_array_declaration(
            fparser2_tree, "my_type", "my_var", ["value1", "value2"])
    assert ("Parse Error: 'my_var' should be set to a value but none was "
            "found, in 'TYPE(my_type) :: my_var'." in str(info.value))

    # Calls _get_array.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var = value")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata.get_derived_array_declaration(
            fparser2_tree, "my_type", "my_var", ["value1", "value2"])
    assert ("No dimension declarations found in 'TYPE(my_type) :: my_var "
            "= value'." in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var(1) = (/invalid/)")

    # Calls validate_scalar_value.
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata.get_derived_array_declaration(
            fparser2_tree, "my_type", "my_var", ["value1", "value2"])
    assert ("The 'my_var' metadata should be a recognised value (one of "
            "['value1', 'value2']) but found 'invalid'." in str(info.value))

    # OK with no validation.
    result = CommonDeclarationMetadata.get_derived_array_declaration(
        fparser2_tree, "my_type", "my_var")
    assert result == ["invalid"]

    # OK with validation.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "type(my_type) :: my_var(2) = (/value1, value2/)")
    result = CommonDeclarationMetadata.get_derived_array_declaration(
        fparser2_tree, "my_type", "my_var", ["value1", "value2"])
    assert result == ["value1", "value2"]


@pytest.mark.usefixtures("parser")
def test_get_array():
    '''Test that the _get_array method behaves as expected.'''

    # Invalid attribute.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer, pointer :: my_var")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("Parse Error: The Fortran representation of 'metadata_name' "
            "metadata should only have at most one attribute and that "
            "attribute should be 'dimension', but found 'INTEGER, "
            "POINTER :: my_var'." in str(info.value))

    # Not an array.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("No dimension declarations found in 'INTEGER :: my_var'."
            in str(info.value))

    # Multi-dimensional array.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(2,2)")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("If the Fortran representation of 'metadata_name' metadata is "
            "an array, it should be one-dimensional with an integer value "
            "for the dimension extent, but found '2, 2' in 'INTEGER :: "
            "my_var(2, 2)'." in str(info.value))

    # Non-integer array extent.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(n)")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("If the Fortran representation of 'metadata_name' metadata is "
            "an array, it should be one-dimensional with an integer value "
            "for the dimension extent, but found 'n' in 'INTEGER :: "
            "my_var(n)'." in str(info.value))

    # Array extent is less than 1.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(0)")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("The array extent should be at least 1, but found '0' in "
            "'INTEGER :: my_var(0)'." in str(info.value))

    # Not initialised to a list of values.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(1) = value")
    with pytest.raises(ValueError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("Expected 'metadata_name' to be set to a list of values, but "
            "found 'value' in 'INTEGER :: my_var(1) = value'."
            in str(info.value))

    # Array extent and list length do not match.
    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(3) = (/value1, value2/)")
    with pytest.raises(ParseError) as info:
        CommonDeclarationMetadata._get_array(
            fparser2_tree, "metadata_name")
    assert ("The array extent '3' and number of 'metadata_name' values '2' "
            "differ in 'INTEGER :: my_var(3) = (/value1, value2/)'."
            in str(info.value))

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer :: my_var(2) = (/value1, value2/)")
    result = CommonDeclarationMetadata._get_array(
        fparser2_tree, "metadata_name")
    assert result == ["value1", "value2"]

    fparser2_tree = Fortran2003.Data_Component_Def_Stmt(
        "integer, dimension(2) :: my_var = (/value1, value2/)")
    result = CommonDeclarationMetadata._get_array(
        fparser2_tree, "metadata_name")
    assert result == ["value1", "value2"]


def test_validate_list():
    '''Test that the validate_list method behaves as expected.'''

    # Not a list.
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.validate_list(None, str)
    assert ("CommonDeclarationMetadata values should be provided as a list "
            "but found 'NoneType'." in str(info.value))

    # Empty list.
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.validate_list([], str)
    assert ("The CommonDeclarationMetadata list should contain at least "
            "one entry, but it is empty." in str(info.value))

    # Invalid list member types.
    with pytest.raises(TypeError) as info:
        CommonDeclarationMetadata.validate_list(["hello", 1], str)
    assert ("The CommonDeclarationMetadata list should be a list containing "
            "objects of type str but found '1', which is of type 'int'."
            in str(info.value))
