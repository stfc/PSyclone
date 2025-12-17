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
# Author L. Turner, Met Office
# Modified A. Pirrie, Met Office

'''Module containing tests for the ScalarArrayArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel import ScalarArrayArgMetadata


@pytest.mark.parametrize("datatype, access, array_ndims", [
    ("GH_REAL", "GH_READ", 1), ("gh_real", "gh_read", 1)])
def test_create(datatype, access, array_ndims):
    '''Test that an instance of ScalarArrayArgMetadata can be created
    successfully. Also test that the arguments are case insensitive.

    '''
    array_arg = ScalarArrayArgMetadata(datatype, access, array_ndims)
    assert isinstance(array_arg, ScalarArrayArgMetadata)
    assert array_arg.form == "gh_scalar_array"
    assert array_arg.datatype == "gh_real"
    assert array_arg.access == "gh_read"
    assert array_arg.array_ndims == 1


@pytest.mark.parametrize("metadata",
                         ["arg_type(GH_SCALAR_ARRAY, GH_REAL, GH_READ, 2)"])
def test_get_metadata(metadata):
    '''Test that the _get_metadata class method works as expected '''
    fparser2_tree = ScalarArrayArgMetadata.create_fparser2(
        metadata, Fortran2003.Part_Ref)
    datatype, access, array_ndims = ScalarArrayArgMetadata._get_metadata(
        fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert array_ndims == 2


@pytest.mark.parametrize("fortran_string", [
    "arg_type(GH_SCALAR_ARRAY, GH_REAL, GH_READ, 5)"])
def test_fortran_string(fortran_string):
    '''Test that the fortran_string method works as expected.'''
    array_arg = ScalarArrayArgMetadata.create_from_fortran_string(
        fortran_string)
    result = array_arg.fortran_string()
    assert result == fortran_string.lower()


@pytest.mark.parametrize("datatype", ["GH_REAL", "GH_INTEGER", "GH_LOGICAL"])
def test_check_datatype(datatype):
    '''Test the check_datatype method works as expected.'''
    ScalarArrayArgMetadata.check_datatype(datatype)
    with pytest.raises(ValueError) as info:
        ScalarArrayArgMetadata.check_datatype("invalid")
    assert ("The 'datatype descriptor' metadata should be a recognised value "
            "(one of ['gh_real', 'gh_integer', 'gh_logical']) but found "
            "'invalid'." in str(info.value))


def test_check_access():
    '''Test the check_access method works as expected.'''
    ScalarArrayArgMetadata.check_access("GH_READ")
    with pytest.raises(ValueError) as info:
        ScalarArrayArgMetadata.check_access("invalid")
    const = LFRicConstants()
    assert (f"The 'access descriptor' metadata should be a recognised value "
            f"(one of {const.VALID_SCALAR_ACCESS_TYPES}) but found "
            f"'invalid'." in str(info.value))


def test_get_array_ndims():
    '''Test that the get_array_ndims method in the
    ScalarArrayArgMetadata class works as expected.

    '''

    fparser_tree = ScalarArrayArgMetadata.create_fparser2(
      "arg_type(GH_SCALAR_ARRAY, GH_REAL, GH_READ, invalid)",
      Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ScalarArrayArgMetadata.get_array_ndims(fparser_tree)
    assert ("The number of dimensions of a ScalarArray should be a string "
            "containing an integer, but found 'invalid'." in str(info.value))

    fparser_tree = ScalarArrayArgMetadata.create_fparser2(
      "arg_type(GH_SCALAR_ARRAY, GH_REAL, GH_READ, 0)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ScalarArrayArgMetadata.get_array_ndims(fparser_tree)
    assert ("The number of dimensions of a ScalarArray should be an integer "
            "greater than or equal to 1 but found 0." in str(info.value))

    fparser_tree = ScalarArrayArgMetadata.create_fparser2(
      "arg_type(GH_SCALAR_ARRAY, GH_REAL, GH_READ, 3)", Fortran2003.Part_Ref)
    array_length = ScalarArrayArgMetadata.get_array_ndims(fparser_tree)
    assert array_length == 3


def test_array_ndims_setter_getter():
    '''Test that the array_ndims setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    array_arg = ScalarArrayArgMetadata("GH_REAL", "GH_READ", 1)

    with pytest.raises(TypeError) as info:
        test_value = float(1.5)
        array_arg.array_ndims = test_value
    assert ("The type of value must be an integer, but found "
            "input of type <class 'float'>." in str(info.value))

    with pytest.raises(ValueError) as info:
        test_value = -1
        array_arg.array_ndims = test_value
    assert ("The number of dimensions of a ScalarArray should be an"
            " integer greater than or equal to 1 but found -1."
            in str(info.value))
