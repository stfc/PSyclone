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
# Author: L. Turner, Met Office

'''Module containing tests for the ArrayArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import ArrayArgMetadata


@pytest.mark.parametrize("datatype, access, array_ndims", [
    ("GH_REAL", "GH_READ", "1"), ("gh_real", "gh_read", "1")])
def test_create(datatype, access, array_ndims):
    '''Test that an instance of ArrayArgMetadata can be created
    successfully. Also test that the arguments are case insensitive.

    '''
    array_arg = ArrayArgMetadata(datatype, access, array_ndims)
    assert isinstance(array_arg, ArrayArgMetadata)
    assert array_arg.form == "gh_array"
    assert array_arg.datatype == "gh_real"
    assert array_arg.access == "gh_read"
    assert array_arg.array_ndims == "1"


def test_init_invalid_an():
    '''Test that an invalid array_size supplied to the constructor
    raises the expected exception.

    '''
    with pytest.raises(TypeError) as info:
        _ = ArrayArgMetadata("GH_REAL", "GH_READ", None)
    assert ("The 'array_size' value should be of type str, but found "
            "'NoneType'." in str(info.value))


@pytest.mark.parametrize(
    "metadata",
    ["arg_type(GH_ARRAY, GH_REAL, GH_READ, NRANKS*2)"])
def test_get_metadata(metadata):
    '''Test that the _get_metadata class method works as expected '''
    fparser2_tree = ArrayArgMetadata.create_fparser2(
        metadata, Fortran2003.Part_Ref)
    datatype, access, array_ndims  = ArrayArgMetadata._get_metadata(
        fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert array_ndims == "2"

# GET THIS WORKING
@pytest.mark.parametrize("fortran_string", [
    "arg_type(GH_ARRAY, GH_REAL, GH_READ, NRANKS*5)"])
def test_fortran_string(fortran_string):
    '''Test that the fortran_string method works as expected.'''
    array_arg = ArrayArgMetadata.create_from_fortran_string(fortran_string)
    result = array_arg.fortran_string()
    fortranstring = fortran_string
    assert result == fortran_string.lower()


def test_check_datatype():
    '''Test the check_datatype method works as expected.'''                  # expand to test gh_integer and gh_logical
    ArrayArgMetadata.check_datatype("GH_REAL")
    with pytest.raises(ValueError) as info:
        ArrayArgMetadata.check_datatype("invalid")
    assert ("The 'datatype descriptor' metadata should be a recognised value "
            "(one of ['gh_real', 'gh_integer', 'gh_logical']) but found "
            "'invalid'." in str(info.value))


def test_check_access():
    '''Test the check_access method works as expected.'''
    ArrayArgMetadata.check_access("GH_READ")
    with pytest.raises(ValueError) as info:
        ArrayArgMetadata.check_access("invalid")
    assert ("The 'access descriptor' metadata should be a recognised value "
            "(one of ['gh_read']) but found 'invalid'." in str(info.value))


# def test_array_size_setter():


def test_array_ndims_setter_getter():
    '''Test that the array_ndims setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    array_arg = ArrayArgMetadata("GH_REAL", "GH_READ", "2")

    with pytest.raises(ValueError) as info:
        array_arg.array_ndims = "invalid"
    assert ("The array size should be a string containing an integer, "
            "but found 'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        array_arg.array_ndims = "0"
    assert ("The array size should be an integer greater than or equal to "
            "1 but found 0." in str(info.value))

    array_arg.array_ndims = "3"
    assert array_arg.array_ndims == "3"
