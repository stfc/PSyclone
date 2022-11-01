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

'''Module containing tests for the ScalarArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata


def test_create():
    '''Test that an instance of ScalarArgMetadata can be created
    successfully.

    '''
    field_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    assert isinstance(field_arg, ScalarArgMetadata)
    assert field_arg.form == "gh_scalar"
    assert field_arg._datatype == "gh_real"
    assert field_arg._access == "gh_read"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test that all relevant check and get methods are called
    by raising exceptions within them, as well as checking for valid
    input.

    '''
    with pytest.raises(TypeError) as info:
        _ = ScalarArgMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "hello(x)", Fortran2003.Part_Ref)

    with pytest.raises(ValueError) as info:
        _ = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name 'arg_type' "
            "and be in the form 'arg_type(...)', but found 'hello(x)'."
            in str(info.value))

    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "arg_type(x)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have 3 arguments, but "
            "found 1 in 'arg_type(x)'." in str(info.value))

    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Scalars should have gh_scalar as their first metadata argument, "
            "but found 'GH_FIELD'." in str(info.value))

    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "arg_type(GH_SCALAR, GH_UNREAL, GH_READ)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '1' for metadata 'arg_type(GH_SCALAR, "
            "GH_UNREAL, GH_READ)'. The datatype descriptor value should be "
            "one of ['gh_real', 'gh_integer', 'gh_logical'], but found "
            "'GH_UNREAL'." in str(info.value))

    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "arg_type(GH_SCALAR, GH_REAL, GH_ERROR)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '2' for metadata 'arg_type(GH_SCALAR, "
            "GH_REAL, GH_ERROR)'. The access descriptor value should be one "
            "of ['gh_read'], but found 'GH_ERROR'." in str(info.value))

    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "arg_type(GH_SCALAR, GH_REAL, GH_READ)", Fortran2003.Part_Ref)
    field_arg = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert field_arg.form == "gh_scalar"
    assert field_arg._datatype == "gh_real"
    assert field_arg._access == "gh_read"


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    fortran_string = "arg_type(gh_scalar, gh_real, gh_read)"
    field_arg = ScalarArgMetadata.create_from_fortran_string(fortran_string)
    result = field_arg.fortran_string()
    assert result == fortran_string


def test_check_datatype():
    '''Test the check_datatype method works as expected.'''
    ScalarArgMetadata.check_datatype("GH_REAL")
    with pytest.raises(ValueError) as info:
        ScalarArgMetadata.check_datatype("invalid")
    assert ("The datatype descriptor value should be one of ['gh_real', "
            "'gh_integer', 'gh_logical'], but found 'invalid'."
            in str(info.value))


def test_check_access():
    '''Test the check_access method works as expected.'''
    ScalarArgMetadata.check_access("GH_READ")
    with pytest.raises(ValueError) as info:
        ScalarArgMetadata.check_access("invalid")
    assert ("The access descriptor value should be one of ['gh_read'], "
            "but found 'invalid'." in str(info.value))
