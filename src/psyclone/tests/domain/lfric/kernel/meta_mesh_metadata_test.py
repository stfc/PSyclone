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

'''Module containing tests for the MetaMeshMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import MetaMeshArgMetadata, MetaMeshMetadata


def test_init():
    '''Test that an instance of MetaMeshMetadata can be created
    and that its initial values are stored as expected.

    '''
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    values = [meta_mesh_arg]
    metadata = MetaMeshMetadata(values)
    assert isinstance(metadata, MetaMeshMetadata)
    assert isinstance(metadata._meta_mesh_args, list)
    assert len(metadata._meta_mesh_args) == 1
    assert metadata._meta_mesh_args[0] is meta_mesh_arg


def test_init_error():
    '''Test that invalid input to the constructor causes the expected
    exception to be raised.

    '''
    with pytest.raises(TypeError) as info:
        _ = MetaMeshMetadata(None)
    assert ("MetaMeshMetadata values should be provided as a list but found "
            "'NoneType'." in str(info.value))


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    values = [meta_mesh_arg]
    metadata = MetaMeshMetadata(values)
    fortran_string = metadata.fortran_string()
    expected = (
        f"type(MESH_DATA_TYPE) :: META_MESH(1) = (/ &\n"
        f"    {meta_mesh_arg.fortran_string()}/)\n")
    assert fortran_string == expected


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    values = [meta_mesh_arg]
    values_list_str = [value.fortran_string() for value in values]
    values_str = ", ".join(values_list_str)
    fortran_string = (f"type(mesh_data_type) :: meta_mesh({len(values)}) = "
                      f"(/{values_str}/)\n")
    metadata = MetaMeshMetadata.create_from_fortran_string(
        fortran_string)
    assert isinstance(metadata.meta_mesh_args, list)
    assert len(metadata.meta_mesh_args) == 1
    assert metadata.meta_mesh_args[0].fortran_string() == \
        meta_mesh_arg.fortran_string()


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 method works as expected.'''

    fortran_string = (
        "type(MESH_DATA_TYPE) :: META_MESH(1) = (/ &\n"
        "    mesh_data_type(adjacent_face)/)\n")
    fparser2_tree = MetaMeshMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    metadata = MetaMeshMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(metadata, MetaMeshMetadata)
    assert metadata.fortran_string() == fortran_string


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    values = [MetaMeshArgMetadata("adjacent_face")]
    metadata = MetaMeshMetadata(values)
    assert metadata.meta_mesh_args == values
    # Check that the getter makes a copy of the list
    assert metadata.meta_mesh_args is not metadata._meta_mesh_args

    metadata.meta_mesh_values = values
    assert metadata._meta_mesh_args == values
    # Check that the setter makes a copy of the list
    assert metadata._meta_mesh_args is not values


def test_setter_errors():
    '''Test that the setter raises the expected exceptions.'''

    values = [MetaMeshArgMetadata("adjacent_face")]
    metadata = MetaMeshMetadata(values)

    with pytest.raises(TypeError) as info:
        metadata.meta_mesh_args = "invalid"
    assert ("MetaMeshMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_mesh_args = []
    assert ("The MetaMeshMetadata list should contain at least one entry, "
            "but it is empty." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_mesh_args = [None]
    assert ("The MetaMeshMetadata list should be a list containing objects "
            "of type MetaMeshArgMetadata but found 'None', which is of "
            "type 'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_mesh_args = ["invalid"]
    assert ("The MetaMeshMetadata list should be a list containing objects "
            "of type MetaMeshArgMetadata but found 'invalid', which is of "
            "type 'str'." in str(info.value))
