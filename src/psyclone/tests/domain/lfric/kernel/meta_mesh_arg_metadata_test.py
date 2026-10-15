# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the MetaMeshArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import MetaMeshArgMetadata


def test_create():
    '''Test that an instance of MetaMeshArgMetadata can be created
    successfully. Also check the input value with mixed case.

    '''
    mesh_arg = MetaMeshArgMetadata("Adjacent_Face")
    assert isinstance(mesh_arg, MetaMeshArgMetadata)
    assert mesh_arg.mesh == "adjacent_face"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test that all relevant check and get methods are called
    by raising exceptions within them, as well as checking for valid
    input (with varying case).

    '''
    with pytest.raises(TypeError) as info:
        _ = MetaMeshArgMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = MetaMeshArgMetadata.create_fparser2(
        "hello(x)", Fortran2003.Part_Ref)

    with pytest.raises(ValueError) as info:
        _ = MetaMeshArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name "
            "'mesh_data_type' and be in the form "
            "'mesh_data_type(...)', but found 'hello(x)'."
            in str(info.value))

    fparser2_tree = MetaMeshArgMetadata.create_fparser2(
        "mesh_data_type(x,y)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaMeshArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have 1 arguments, but "
            "found 2 in 'mesh_data_type(x, y)'."
            in str(info.value))

    fparser2_tree = MetaMeshArgMetadata.create_fparser2(
        "mesh_data_type(invalid)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaMeshArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("The 'mesh property' metadata should be a recognised value (one "
            "of ['adjacent_face']) but found 'invalid'." in str(info.value))

    fparser2_tree = MetaMeshArgMetadata.create_fparser2(
        "mesh_data_type(ADJACENT_FACE)", Fortran2003.Part_Ref)
    mesh_arg = MetaMeshArgMetadata.create_from_fparser2(
        fparser2_tree)
    assert mesh_arg.mesh == "adjacent_face"


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    fortran_string = "mesh_data_type(adjacent_face)"
    mesh_arg = MetaMeshArgMetadata.create_from_fortran_string(
        fortran_string)
    result = mesh_arg.fortran_string()
    assert result == fortran_string


def test_mesh_setter_getter():
    '''Test that the reference element setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    mesh_arg = MetaMeshArgMetadata("adjacent_face")
    with pytest.raises(ValueError) as info:
        mesh_arg.mesh = "invalid"
    assert ("The 'mesh property' metadata should be a recognised value (one "
            "of ['adjacent_face']) but found 'invalid'." in str(info.value))
    mesh_arg.mesh = "adjacent_face"
    assert mesh_arg.mesh == "adjacent_face"
