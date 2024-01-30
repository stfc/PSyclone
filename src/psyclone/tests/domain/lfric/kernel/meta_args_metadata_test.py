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

'''Module containing tests for the MetaArgsMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import (
    FieldArgMetadata, FieldVectorArgMetadata, MetaArgsMetadata,
    ScalarArgMetadata)
from psyclone.parse.utils import ParseError


def test_init():
    '''Test that an instance of MetaArgsMetadata can be created
    and that its initial values are stored as expected.

    '''
    meta_args_arg = ScalarArgMetadata("GH_INTEGER", "GH_READ")
    values = [meta_args_arg]
    metadata = MetaArgsMetadata(values)
    assert isinstance(metadata, MetaArgsMetadata)
    assert isinstance(metadata._meta_args_args, list)
    assert len(metadata._meta_args_args) == 1
    assert metadata._meta_args_args[0] is meta_args_arg


def test_init_error():
    '''Test that invalid input to the constructor causes the expected
    exception to be raised.

    '''
    with pytest.raises(TypeError) as info:
        _ = MetaArgsMetadata(None)
    assert ("MetaArgsMetadata values should be provided as a list but found "
            "'NoneType'." in str(info.value))


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''
    meta_args_arg = FieldArgMetadata("GH_REAL", "GH_INC", "W0")
    values = [meta_args_arg]
    metadata = MetaArgsMetadata(values)
    fortran_string = metadata.fortran_string()
    expected = (
        f"type(ARG_TYPE) :: META_ARGS(1) = (/ &\n"
        f"    {meta_args_arg.fortran_string()}/)\n")
    assert fortran_string == expected


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    meta_args_arg1 = ScalarArgMetadata("GH_REAL", "GH_READ")
    meta_args_arg2 = FieldArgMetadata("GH_REAL", "GH_READWRITE", "w3")
    values = [meta_args_arg1, meta_args_arg2]
    values_list_str = [value.fortran_string() for value in values]
    values_str = ", ".join(values_list_str)
    fortran_string = (f"type(arg_type) :: meta_args({len(values)}) = "
                      f"(/{values_str}/)\n")
    metadata = MetaArgsMetadata.create_from_fortran_string(
        fortran_string)
    assert isinstance(metadata.meta_args_args, list)
    assert len(metadata.meta_args_args) == 2
    assert metadata.meta_args_args[0].fortran_string() == \
        meta_args_arg1.fortran_string()
    assert metadata.meta_args_args[1].fortran_string() == \
        meta_args_arg2.fortran_string()


def test_create_from_fparser2_error():
    '''Test that the create_from_fparser2 method raises the expected
    exception.

    '''
    fortran_string = (
        "type(ARG_TYPE) :: META_ARGS(1) = (/arg_type(GH_UNKNOWN)/)")
    fparser2_tree = MetaArgsMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    with pytest.raises(ParseError) as info:
        _ = MetaArgsMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected a 'meta_arg' entry to be a field, a scalar or an "
            "operator, but found 'arg_type(GH_UNKNOWN)" in str(info.value))


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 method works as expected.'''
    fortran_string = (
        "type(ARG_TYPE) :: META_ARGS(7) = (/ &\n"
        "    arg_type(gh_scalar, gh_real, gh_read), &\n"
        "    arg_type(gh_operator, gh_real, gh_read, w0, w1), &\n"
        "    arg_type(gh_columnwise_operator, gh_real, gh_read, w0, w1), &\n"
        "    arg_type(gh_field, gh_real, gh_write, w0), &\n"
        "    arg_type(gh_field*3, gh_real, gh_write, w0), &\n"
        "    arg_type(gh_field, gh_real, gh_write, w0, mesh_arg=gh_fine), &\n"
        "    arg_type(gh_field*3, gh_real, gh_write, w0, "
        "mesh_arg=gh_fine)/)\n")
    fparser2_tree = MetaArgsMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    metadata = MetaArgsMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(metadata, MetaArgsMetadata)
    assert metadata.fortran_string() == fortran_string


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    values = [ScalarArgMetadata("GH_REAL", "GH_READ"),
              FieldVectorArgMetadata("GH_REAL", "GH_WRITE", "W0", "3")]
    metadata = MetaArgsMetadata(values)
    assert metadata.meta_args_args == values
    # Check that the getter makes a copy of the list
    assert metadata.meta_args_args is not metadata._meta_args_args

    metadata.meta_args_values = values
    assert metadata._meta_args_args == values
    # Check that the setter makes a copy of the list
    assert metadata._meta_args_args is not values


def test_setter_errors():
    '''Test that the setter raises the expected exceptions.'''

    values = [ScalarArgMetadata("GH_REAL", "GH_READ"),
              FieldVectorArgMetadata("GH_REAL", "GH_WRITE", "W0", "3")]
    metadata = MetaArgsMetadata(values)

    with pytest.raises(TypeError) as info:
        metadata.meta_args_args = "invalid"
    assert ("MetaArgsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_args_args = []
    assert ("The MetaArgsMetadata list should contain at least one entry, "
            "but it is empty." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_args_args = [None]
    assert ("The MetaArgsMetadata list should be a list containing objects "
            "of type CommonMetaArgMetadata but found 'None', which is of "
            "type 'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_args_args = ["invalid"]
    assert ("The MetaArgsMetadata list should be a list containing objects "
            "of type CommonMetaArgMetadata but found 'invalid', which is of "
            "type 'str'." in str(info.value))
