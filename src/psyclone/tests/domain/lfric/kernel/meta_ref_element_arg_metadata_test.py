# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the MetaRefElementArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import MetaRefElementArgMetadata


def test_create():
    '''Test that an instance of MetaRefElementArgMetadata can be created
    successfully. Also check the input value with mixed case.

    '''
    ref_element_arg = MetaRefElementArgMetadata("Normals_To_Faces")
    assert isinstance(ref_element_arg, MetaRefElementArgMetadata)
    assert ref_element_arg.reference_element == "normals_to_faces"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test that all relevant check and get methods are called
    by raising exceptions within them, as well as checking for valid
    input (with varying case).

    '''
    with pytest.raises(TypeError) as info:
        _ = MetaRefElementArgMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = MetaRefElementArgMetadata.create_fparser2(
        "hello(x)", Fortran2003.Part_Ref)

    with pytest.raises(ValueError) as info:
        _ = MetaRefElementArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name "
            "'reference_element_data_type' and be in the form "
            "'reference_element_data_type(...)', but found 'hello(x)'."
            in str(info.value))

    fparser2_tree = MetaRefElementArgMetadata.create_fparser2(
        "reference_element_data_type(x,y)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaRefElementArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have 1 arguments, but "
            "found 2 in 'reference_element_data_type(x, y)'."
            in str(info.value))

    fparser2_tree = MetaRefElementArgMetadata.create_fparser2(
        "reference_element_data_type(invalid)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaRefElementArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("The 'reference element property' metadata should be a recognised "
            "value (one of ['normals_to_horizontal_faces', "
            "'normals_to_vertical_faces', 'normals_to_faces', "
            "'outward_normals_to_horizontal_faces', "
            "'outward_normals_to_vertical_faces', "
            "'outward_normals_to_faces']) but found 'invalid'."
            in str(info.value))

    fparser2_tree = MetaRefElementArgMetadata.create_fparser2(
        "reference_element_data_type(NORMALS_TO_FACES)", Fortran2003.Part_Ref)
    ref_element_arg = MetaRefElementArgMetadata.create_from_fparser2(
        fparser2_tree)
    assert ref_element_arg.reference_element == "normals_to_faces"


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    fortran_string = "reference_element_data_type(normals_to_faces)"
    ref_element_arg = MetaRefElementArgMetadata.create_from_fortran_string(
        fortran_string)
    result = ref_element_arg.fortran_string()
    assert result == fortran_string


def test_reference_element_setter_getter():
    '''Test that the reference element setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    ref_element_arg = MetaRefElementArgMetadata("normals_to_faces")
    with pytest.raises(ValueError) as info:
        ref_element_arg.reference_element = "invalid"
    assert ("The 'reference element property' metadata should be a recognised "
            "value (one of ['normals_to_horizontal_faces', "
            "'normals_to_vertical_faces', 'normals_to_faces', "
            "'outward_normals_to_horizontal_faces', "
            "'outward_normals_to_vertical_faces', "
            "'outward_normals_to_faces']) but found 'invalid'."
            in str(info.value))
    ref_element_arg.reference_element = "outward_normals_to_faces"
    assert ref_element_arg.reference_element == "outward_normals_to_faces"
