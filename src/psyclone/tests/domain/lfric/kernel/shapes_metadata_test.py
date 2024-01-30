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

'''Module containing tests for the ShapesMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import ShapesMetadata


def test_init():
    '''Test that an instance of ShapesMetadata can be created and that its
    initial values are stored as expected.

    '''
    shape_values = ["gh_quadrature_XYoZ", "gh_evaluator"]
    shapes_metadata = ShapesMetadata(shape_values)
    assert isinstance(shapes_metadata, ShapesMetadata)
    assert shapes_metadata._shapes == [value.lower() for value in shape_values]


def test_init_error():
    '''Test that invalid input to the constructor causes the expected
    exception to be raised.

    '''
    with pytest.raises(TypeError) as info:
        _ = ShapesMetadata(None)
    assert ("ShapesMetadata values should be provided as a list but found "
            "'NoneType'." in str(info.value))


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

    # Test that the setter lowers the input values
    shape_values = ["gh_quadrature_XYoZ", "GH_QUADRATURE_EDGE"]
    shapes_metadata.shapes = shape_values
    assert shapes_metadata._shapes == [value.lower() for value in shape_values]


def test_setter_errors():
    '''Test that the setter raises the expected exceptions.'''

    shape_values = ["gh_evaluator", "gh_quadrature_face"]
    shapes_metadata = ShapesMetadata(shape_values)

    with pytest.raises(TypeError) as info:
        shapes_metadata.shapes = "invalid"
    assert ("ShapesMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        shapes_metadata.shapes = []
    assert ("The ShapesMetadata list should contain at least one entry, but "
            "it is empty." in str(info.value))

    with pytest.raises(TypeError) as info:
        shapes_metadata.shapes = [None]
    assert ("The ShapesMetadata list should be a list containing objects of "
            "type str but found 'None', which is of type 'NoneType'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        shapes_metadata.shapes = ["invalid"]
    assert ("The 'shape' metadata should be a recognised value (one of "
            "['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge', 'gh_evaluator']) but found 'invalid'."
            in str(info.value))
