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

'''Module containing tests for the FieldArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import FieldArgMetadata


@pytest.mark.parametrize("datatype, access, function_space", [
    ("GH_REAL", "GH_READ", "W0"), ("gh_real", "gh_read", "w0")])
def test_create(datatype, access, function_space):
    '''Test that an instance of FieldArgMetadata can be created
    successfully. Also test that the arguments are case insensitive.

    '''
    field_arg = FieldArgMetadata(datatype, access, function_space)
    assert isinstance(field_arg, FieldArgMetadata)
    assert field_arg.form == "gh_field"
    assert field_arg._datatype == "gh_real"
    assert field_arg._access == "gh_read"
    assert field_arg._function_space == "w0"
    assert field_arg._stencil is None


def test_create_stencil():
    '''Test that an instance of FieldArgMetadata can be created
    successfully with optional stencil metadata.

    '''
    field_arg = FieldArgMetadata("gh_real", "gh_read", "w0", stencil="cross")
    assert isinstance(field_arg, FieldArgMetadata)
    assert field_arg.form == "gh_field"
    assert field_arg._datatype == "gh_real"
    assert field_arg._access == "gh_read"
    assert field_arg._function_space == "w0"
    assert field_arg._stencil == "cross"


def test_init_invalid_fs():
    '''Test that an invalid function space supplied to the constructor
    raises the expected exception.

    '''
    with pytest.raises(TypeError) as info:
        _ = FieldArgMetadata("GH_REAL", "GH_READ", None)
    assert ("The 'function space' value should be of type str, but found "
            "'NoneType'." in str(info.value))


def test_init_invalid_stencil():
    '''Test that an invalid stencil supplied to the constructor
    raises the expected exception.

    '''
    with pytest.raises(TypeError) as info:
        _ = FieldArgMetadata("GH_REAL", "GH_READ", "W0", stencil=1)
    assert ("The 'stencil' value should be of type str, but found "
            "'int'." in str(info.value))


@pytest.mark.parametrize(
    "metadata,expected_stencil",
    [("arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", None),
     ("arg_type(GH_FIELD, GH_REAL, GH_READ, W0, stencil(region))", "region")])
def test_get_metadata(metadata, expected_stencil):
    '''Test that the _get_metadata class method works as expected, with
    and without optional stencil metadata.

    '''
    fparser2_tree = FieldArgMetadata.create_fparser2(
        metadata, Fortran2003.Part_Ref)
    datatype, access, function_space, stencil = FieldArgMetadata._get_metadata(
        fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"
    assert stencil == expected_stencil


def test_get_stencil():
    '''Check that the get_stencil method behaves as expected.'''

    # No stencil metadata
    fparser2_tree = FieldArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    assert FieldArgMetadata.get_stencil(fparser2_tree) is None

    # Invalid stencil metadata ( not 'stencil(...' ).
    fparser2_tree = FieldArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, REGION)",
        Fortran2003.Part_Ref)
    with pytest.raises(TypeError) as info:
        FieldArgMetadata.get_stencil(fparser2_tree)
    assert ("The stencil metadata should be in the form 'stencil(type)' but "
            "found 'region'." in str(info.value))

    # Invalid stencil metadata ( not 'stencil(...)' ).
    fparser2_tree = FieldArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, STENCIL())",
        Fortran2003.Part_Ref)
    with pytest.raises(TypeError) as info:
        FieldArgMetadata.get_stencil(fparser2_tree)
    assert ("The stencil metadata should be in the form 'stencil(type)' but "
            "found 'stencil()'." in str(info.value))

    # valid stencil metadata
    fparser2_tree = FieldArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, STENCIL(REGION))",
        Fortran2003.Part_Ref)
    assert FieldArgMetadata.get_stencil(fparser2_tree) == "region"


@pytest.mark.parametrize("fortran_string", [
    "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)",
    "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, STENCIL(REGION))"])
def test_fortran_string(fortran_string):
    '''Test that the fortran_string method works as expected. Test with
    and without a stencil.

    '''
    field_arg = FieldArgMetadata.create_from_fortran_string(fortran_string)
    result = field_arg.fortran_string()
    assert result == fortran_string.lower()


def test_check_datatype():
    '''Test the check_datatype method works as expected.'''
    FieldArgMetadata.check_datatype("GH_REAL")
    with pytest.raises(ValueError) as info:
        FieldArgMetadata.check_datatype("invalid")
    assert ("The 'datatype descriptor' metadata should be a recognised value "
            "(one of ['gh_real', 'gh_integer']) but found 'invalid'."
            in str(info.value))


def test_check_access():
    '''Test the check_access method works as expected.'''
    FieldArgMetadata.check_access("GH_READ")
    with pytest.raises(ValueError) as info:
        FieldArgMetadata.check_access("invalid")
    assert ("The 'access descriptor' metadata should be a recognised value "
            "(one of ['gh_read', 'gh_write', 'gh_readwrite', 'gh_inc', "
            "'gh_readinc']) but found 'invalid'." in str(info.value))


def test_function_space_setter_getter():
    '''Test that the function space setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    field_arg = FieldArgMetadata("GH_REAL", "GH_READ", "W0")
    with pytest.raises(ValueError) as info:
        field_arg.function_space = "invalid"
    assert ("The 'function space' metadata should be a recognised value (one "
            "of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', "
            "'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi', "
            "'any_space_1', 'any_space_2', 'any_space_3', 'any_space_4', "
            "'any_space_5', 'any_space_6', 'any_space_7', 'any_space_8', "
            "'any_space_9', 'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10']) but found 'invalid'."
            in str(info.value))
    field_arg.function_space = "w3"
    assert field_arg.function_space == "w3"
    field_arg.function_space = "W3"
    assert field_arg.function_space == "w3"


def test_stencil_getter_setter():
    '''Test that the stencil setter and getter work as expected, including
    raising an exception if the value is invalid and checking for
    lower and upper case input.

    '''
    field_arg = FieldArgMetadata("GH_REAL", "GH_READ", "W0")
    with pytest.raises(ValueError) as info:
        field_arg.stencil = "invalid"
    assert ("The 'stencil' metadata should be a recognised value (one of "
            "['x1d', 'y1d', 'xory1d', 'cross', 'region', 'cross2d']) but "
            "found 'invalid'." in str(info.value))
    field_arg.stencil = "x1d"
    assert field_arg.stencil == "x1d"
    field_arg.stencil = "X1D"
    assert field_arg.stencil == "x1d"
