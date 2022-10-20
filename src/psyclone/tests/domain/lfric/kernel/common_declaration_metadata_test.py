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

'''Module containing tests for the ShapesMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.shapes_metadata import ShapesMetadata
from psyclone.parse.utils import ParseError


def test_init_invalid():
    '''Test that an exception is raised if invalid initial values are
    provided when constructing an instance of the ShapesMetadata
    class.

    '''
    with pytest.raises(TypeError) as info:
        _ = ShapesMetadata("invalid")
    assert ("shape values should be provided as a list but found 'str'."
            in str(info.value))


def test_init():
    '''Test that valid initial values provided when constructing an
    instance of ShapesMetadata are stored as expected.

    '''
    shape_values = ["gh_quadrature_XYoZ", "gh_evaluator"]
    shapes_metadata = ShapesMetadata(shape_values)
    assert shapes_metadata._shapes == [value.lower() for value in shape_values]


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    shape_values = ["gh_quadrature_XYoZ"]
    shapes_metadata = ShapesMetadata(shape_values)
    fortran_string = shapes_metadata.fortran_string()
    expected = ("INTEGER :: GH_SHAPE = gh_quadrature_xyoz\n")
    assert fortran_string == expected

    shape_values = ["gh_quadrature_XYoZ", "gh_evaluator"]
    shapes_metadata = ShapesMetadata(shape_values)
    fortran_string = shapes_metadata.fortran_string()
    expected = (
        "INTEGER :: GH_SHAPE(2) = (/gh_quadrature_xyoz, gh_evaluator/)\n")
    assert fortran_string == expected


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    fortran_string = "integer :: gh_shape = gh_evaluator"
    shapes_metadata = ShapesMetadata.create_from_fortran_string(
        fortran_string)
    assert shapes_metadata.shapes == ["gh_evaluator"]


def test_create_fparser2():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    fortran_string = "invalid"
    with pytest.raises(ValueError) as info:
        _ = ShapesMetadata.create_fparser2(
            fortran_string, Fortran2003.Data_Component_Def_Stmt)
    assert ("Expected kernel metadata to be a Fortran "
            "Data_Component_Def_Stmt, but found 'invalid'." in str(info.value))

    fortran_string = "INTEGER :: gh_shape = gh_quadrature_XYoZ"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    assert str(fparser2_tree) == fortran_string


def test_create_from_fparser2_error():
    '''Test that the create_from_fparser2 method raises the expected
    exceptions when supplied with invalid input.

    '''
    with pytest.raises(TypeError) as info:
        ShapesMetadata.create_from_fparser2(None)
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Data_Component_Def_Stmt object but found type 'NoneType' with "
            "value 'None'." in str(info.value))

    fortran_string = "real :: gh_shape = value"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(TypeError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("In Fortran, GH_SHAPE metadata should be encoded as an INTEGER, "
            "but found 'REAL' in 'REAL :: gh_shape = value'."
            in str(info.value))

    fortran_string = "integer :: gh_shape, gh_shape2"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ParseError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("In Fortran, GH_SHAPE metadata should only contain a single "
            "variable, but found '2' in 'INTEGER :: gh_shape, gh_shape2'."
            in str(info.value))

    fortran_string = "integer :: gh_ship"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("In Fortran, GH_SHAPE metadata should be encoded as a variable "
            "called GH_SHAPE, but found 'gh_ship' in 'INTEGER :: gh_ship'."
            in str(info.value))

    fortran_string = "integer, pointer :: gh_shape = gh_evaluator"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ParseError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("The Fortran representation of GH_SHAPE metadata should only have "
            "at most one attribute and that attribute should be 'dimension', "
            "but found 'INTEGER, POINTER :: gh_shape = gh_evaluator'."
            in str(info.value))

    fortran_string = "integer, dimension(2), pointer :: gh_shape = value"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ParseError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("The Fortran representation of GH_SHAPE metadata should only have "
            "at most one attribute and that attribute should be 'dimension', "
            "but found 'INTEGER, DIMENSION(2), POINTER :: gh_shape = value'."
            in str(info.value))

    fortran_string = "integer, dimension(n) :: gh_shape = value"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("If the Fortran representation of GH_SHAPE metadata is an array, "
            "it should be one-dimensional with an integer value for the "
            "dimension extent, but found 'n' in 'INTEGER, DIMENSION(n) :: "
            "gh_shape = value'." in str(info.value))

    # Use the alternate form of array declaration to check that is
    # also working with exceptions
    fortran_string = "integer :: gh_shape(1,1) = value"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("If the Fortran representation of GH_SHAPE metadata is an array, "
            "it should be one-dimensional with an integer value for the "
            "dimension extent, but found '1, 1' in 'INTEGER :: "
            "gh_shape(1, 1) = value'." in str(info.value))

    fortran_string = "integer, dimension(0) :: gh_shape = value"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("The array extent should be at least 1, but found '0' in "
            "'INTEGER, DIMENSION(0) :: gh_shape = value'." in str(info.value))

    fortran_string = "integer :: gh_shape"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ParseError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("GH_SHAPE should be set to a value but none was found, in "
            "'INTEGER :: gh_shape'." in str(info.value))

    fortran_string = "integer :: gh_shape(1) = gh_evaluator"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected GH_SHAPE to be set to a list of values, but found "
            "'gh_evaluator' in 'INTEGER :: gh_shape(1) = gh_evaluator'."
            in str(info.value))

    fortran_string = "integer :: gh_shape(1) = (/ invalid /)"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("The GH_SHAPE metadata should be a recognised value (one of "
            "['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge', 'gh_evaluator']) but found 'invalid'."
            in str(info.value))

    fortran_string = "integer :: gh_shape(2) = (/ gh_evaluator /)"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ParseError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("The array extent '2' and number of GH_SHAPE values '1' differ "
            "in 'INTEGER :: gh_shape(2) = (/gh_evaluator/)'."
            in str(info.value))

    fortran_string = "integer :: gh_shape = invalid"
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ValueError) as info:
        ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert ("The GH_SHAPE metadata should be a recognised value (one of "
            "['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge', 'gh_evaluator']) but found 'invalid'."
            in str(info.value))


@pytest.mark.parametrize("fortran_string, expected_shapes_list", [
    ("INTEGER :: gh_shape = gh_quadrature_XYoZ", ["gh_quadrature_xyoz"]),
    ("INTEGER, dimension(1) :: gh_shape = (/gh_quadrature_edge/)",
     ["gh_quadrature_edge"]),
    ("INTEGER :: gh_shape(2) = (/gh_quadrature_face, gh_evaluator/)",
     ["gh_quadrature_face", "gh_evaluator"])])
def test_create_from_fparser2(fortran_string, expected_shapes_list):
    '''Test that the create_from_fparser2 method works as expected.'''
    fparser2_tree = ShapesMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    shapes_metadata = ShapesMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(shapes_metadata, ShapesMetadata)
    assert shapes_metadata.shapes == expected_shapes_list


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    shape_values = ["gh_evaluator", "gh_quadrature_face"]
    shapes_metadata = ShapesMetadata(shape_values)
    assert shapes_metadata.shapes == shape_values
    # Check that the getter makes a copy of the list
    assert shapes_metadata.shapes is not shapes_metadata._shapes

    shape_values = ["gh_quadrature_xyoz", "gh_quadrature_edge"]
    shapes_metadata.shapes = shape_values
    assert shapes_metadata._shapes == shape_values
    # Check that the setter makes a copy of the list
    assert shapes_metadata._shapes is not shape_values

    shape_values = ["gh_quadrature_XYoZ", "GH_QUADRATURE_EDGE"]
    shapes_metadata.shapes = shape_values
    assert shapes_metadata._shapes == [value.lower() for value in shape_values]

    with pytest.raises(TypeError) as info:
        shapes_metadata.shapes = "invalid"
    assert ("shape values should be provided as a list but found 'str'."
            in str(info.value))
    with pytest.raises(TypeError) as info:
        shapes_metadata.shapes = []
    assert ("The shapes list should contain at least one entry, but it is "
            "empty." in str(info.value))
    with pytest.raises(TypeError) as info:
        shapes_metadata.shapes = [None]
    assert ("shapes should be a list of str, but found 'NoneType'."
            in str(info.value))
    with pytest.raises(ValueError) as info:
        shapes_metadata.shapes = ["invalid"]
    assert ("The shape metadata should be a recognised value (one of "
            "['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge', 'gh_evaluator']) but found 'invalid'."
            in str(info.value))
