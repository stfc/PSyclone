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

'''Module containing tests for the FieldVectorArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import FieldVectorArgMetadata


@pytest.mark.parametrize("datatype, access, function_space", [
    ("GH_REAL", "GH_READ", "W0"), ("gh_real", "gh_read", "w0")])
def test_create(datatype, access, function_space):
    '''Test that an instance of FieldVectorArgMetadata can be created
    successfully. Also test that the arguments are case insensitive.

    '''
    field_vector_arg = FieldVectorArgMetadata(
        datatype, access, function_space, "2")
    assert isinstance(field_vector_arg, FieldVectorArgMetadata)
    assert field_vector_arg.form == "gh_field"
    assert field_vector_arg._datatype == "gh_real"
    assert field_vector_arg._access == "gh_read"
    assert field_vector_arg._function_space == "w0"
    assert field_vector_arg._vector_length == "2"


def test_init_invalid():
    '''Test that an invalid vector length supplied to the constructor
    raises the expected exception.

    '''
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArgMetadata("GH_REAL", "GH_READ", "W0", 1)
    assert ("The vector size should be a string but found int."
            in str(info.value))


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test that all relevant check and get methods are called
    by raising exceptions within them, as well as checking for valid
    input.

    '''
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArgMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = FieldVectorArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArgMetadata.create_from_fparser2(fparser2_tree)
    assert("The vector length metadata should be in the form "
           "'form*vector_length' but found 'GH_FIELD'." in str(info.value))

    fparser2_tree = FieldVectorArgMetadata.create_fparser2(
        "arg_type(GH_FEELED*3, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Metadata for 'field-vector' kernel arguments should have "
            "'gh_field' in their first metadata argument, but found "
            "'GH_FEELED'." in str(info.value))

    fparser2_tree = FieldVectorArgMetadata.create_fparser2(
        "arg_type(GH_FIELD*3, GH_UNREAL, GH_READ, W0)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '1' for metadata 'arg_type(GH_FIELD * 3, "
            "GH_UNREAL, GH_READ, W0)'. The 'datatype descriptor' metadata "
            "should be a recognised value (one of ['gh_real', 'gh_integer']) "
            "but found 'GH_UNREAL'." in str(info.value))

    fparser2_tree = FieldVectorArgMetadata.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_ERROR, W0)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '2' for metadata 'arg_type(GH_FIELD * 3, "
            "GH_REAL, GH_ERROR, W0)'. The 'access descriptor' metadata should "
            "be a recognised value (one of ['gh_read', 'gh_write', "
            "'gh_readwrite', 'gh_inc', 'gh_readinc']) but found 'GH_ERROR'."
            in str(info.value))

    fparser2_tree = FieldVectorArgMetadata.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, DOUBLE_U_ZERO)",
        Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '3' for metadata 'arg_type(GH_FIELD * 3, "
            "GH_REAL, GH_READ, DOUBLE_U_ZERO)'. The 'function space' metadata "
            "should be a recognised value (one of ['w3', 'wtheta', 'w2v', "
            "'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', 'w2trace', 'w2h', "
            "'w2htrace', 'any_w2', 'wchi', 'any_space_1', 'any_space_2', "
            "'any_space_3', 'any_space_4', 'any_space_5', 'any_space_6', "
            "'any_space_7', 'any_space_8', 'any_space_9', 'any_space_10', "
            "'any_discontinuous_space_1', 'any_discontinuous_space_2', "
            "'any_discontinuous_space_3', 'any_discontinuous_space_4', "
            "'any_discontinuous_space_5', 'any_discontinuous_space_6', "
            "'any_discontinuous_space_7', 'any_discontinuous_space_8', "
            "'any_discontinuous_space_9', 'any_discontinuous_space_10']) "
            "but found 'DOUBLE_U_ZERO'." in str(info.value))

    fparser2_tree = FieldVectorArgMetadata.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    field_vector_arg = FieldVectorArgMetadata.create_from_fparser2(
        fparser2_tree)
    assert field_vector_arg.form == "gh_field"
    assert field_vector_arg._datatype == "gh_real"
    assert field_vector_arg._access == "gh_read"
    assert field_vector_arg._function_space == "w0"
    assert field_vector_arg._vector_length == "3"


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    fortran_string = "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)"
    field_vector_arg = FieldVectorArgMetadata.create_from_fortran_string(
        fortran_string)
    result = field_vector_arg.fortran_string()
    assert result == fortran_string.lower()


def test_vector_length_setter_getter():
    '''Test that the vector length setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    field_vector_arg = FieldVectorArgMetadata("GH_REAL", "GH_READ", "W0", "2")

    with pytest.raises(ValueError) as info:
        field_vector_arg.vector_length = "invalid"
    assert ("The vector size should be a string containing an integer, "
            "but found 'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        field_vector_arg.vector_length = "1"
    assert ("The vector size should be an integer greater than 1 but found 1."
            in str(info.value))

    field_vector_arg.vector_length = "3"
    assert field_vector_arg.vector_length == "3"
