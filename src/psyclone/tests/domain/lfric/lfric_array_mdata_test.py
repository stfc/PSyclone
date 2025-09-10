# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

'''
Module containing pytest tests for the general LFRic array arguments
functionality (e.g. metadata, parsing, invoke calls).
'''

import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.domain.lfric import (LFRicArgDescriptor, LFRicConstants,
                                   LFRicKernMetadata)
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


ARRAY_CODE = '''
module testkern_mod

  type, extends(kernel_type) :: testkern_array_type
     type(arg_type), meta_args(5) =                                          &
          (/ arg_type(gh_scalar_array,   gh_real,    gh_read, 1),            &
             arg_type(gh_scalar_array,   gh_integer, gh_read, 2),            &
             arg_type(gh_scalar_array,   gh_logical, gh_read, 4),            &
             arg_type(gh_operator, gh_real,   gh_read, w2, w2),              &
             arg_type(gh_field,    gh_real,   gh_write, w3)                  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_array_type
contains
  subroutine testkern_code(a, b, c, d)
  end subroutine testkern_code
end module testkern_mod
'''


def test_ad_array_init_wrong_argument_type():
    ''' Test that an error is raised if something other than a ScalarArray
    is passed to the LFRicArgDescriptor._init_array() method. '''
    ast = fpapi.parse(ARRAY_CODE, ignore_comments=False)
    name = "testkern_array_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get an argument which is not a ScalarArray
    wrong_arg = metadata._inits[3]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over, 0)._init_array(wrong_arg)
    assert ("Expected a ScalarArray argument but got an argument of type "
            "'gh_operator'." in str(excinfo.value))


def test_ad_array_type_wrong_num_of_args():
    ''' Tests that an error is raised when the ScalarArray argument
    descriptor metadata for a ScalarArray has fewer than 4 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_read)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_array_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("a 'meta_arg' entry must have 4 arguments if its first "
            "argument is of ['gh_scalar_array'] type" in str(excinfo.value))


def test_ad_array_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    # check real array
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array, gh_unreal,    gh_read, 1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    const = LFRicConstants()
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            f"be a valid data type (one of {const.VALID_ARRAY_DATA_TYPES}), "
            f"but found 'gh_unreal' in 'arg_type(gh_scalar_array, gh_unreal, "
            f"gh_read, 1)'." in str(excinfo.value))
    # check integer array
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_integer, gh_read, 2)",
        "arg_type(gh_scalar_array,   gh_frac,    gh_read, 2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    const = LFRicConstants()
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            f"be a valid data type (one of {const.VALID_ARRAY_DATA_TYPES}), "
            f"but found 'gh_frac' in 'arg_type(gh_scalar_array, gh_frac, "
            f"gh_read, 2)'." in str(excinfo.value))
    # check logical array
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_logical, gh_read, 4)",
        "arg_type(gh_scalar_array, gh_illogical, gh_read, 4)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    const = LFRicConstants()
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            f"be a valid data type (one of {const.VALID_ARRAY_DATA_TYPES}), "
            f"but found 'gh_illogical' in 'arg_type(gh_scalar_array, "
            f"gh_illogical, gh_read, 4)'." in str(excinfo.value))


def test_ad_array_init_wrong_data_type(monkeypatch):
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_array() method. '''
    ast = fpapi.parse(ARRAY_CODE, ignore_comments=False)
    name = "testkern_array_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get a ScalarArray argument descriptor and set a wrong data type
    scalar_arg = metadata._inits[0]
    scalar_arg.args[1].name = "gh_double"
    const = LFRicConstants()
    # Now try to trip the error by making the initial test think
    # that 'gh_double' is actually a valid data type
    monkeypatch.setattr(
        target=LFRicConstants, name="VALID_ARG_DATA_TYPES",
        value=LFRicConstants.VALID_ARG_DATA_TYPES + ["gh_double"])
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            scalar_arg, metadata.iterates_over, 0)._init_scalar(scalar_arg)
    assert (f"Expected one of {const.VALID_ARRAY_DATA_TYPES} as the "
            f"ScalarArray data type but got "
            f"'gh_double'." in str(excinfo.value))


def test_ad_array_type_no_write():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_WRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_write, 1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("ScalarArray arguments must have read-only ('gh_read') "
            "access but found 'gh_write'" in str(excinfo.value))


def test_ad_array_type_no_inc():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_INC' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_inc, 1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("ScalarArray arguments must have read-only ('gh_read') "
            "access but found 'gh_inc'" in str(excinfo.value))


def test_ad_array_type_no_readwrite():
    ''' Tests that an error is raised when the argument descriptor
    metadata for an array specifies 'GH_READWRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real, gh_readwrite, 1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("ScalarArray arguments must have read-only ('gh_read') "
            "access but found 'gh_readwrite'" in str(excinfo.value))


def test_ad_array_type_no_sum():
    ''' Tests that an error is raised when the argument descriptor
    metadata for an array specifies 'GH_SUM' access (reduction). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_sum,  1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_array_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("ScalarArray arguments must have read-only ('gh_read') "
            "access but found 'gh_sum'" in str(excinfo.value))


def test_no_vector_array():
    ''' Tests that we raise an error when kernel metadata erroneously
    specifies a vector scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array*3, gh_real,  gh_read, 1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("vector notation is only supported for ['gh_field'] argument "
            "types but found 'gh_scalar_array * 3'" in str(excinfo.value))


@pytest.mark.parametrize("array_ind, array_type, array_ndims", [
    (0, "gh_real", 1), (1, "gh_integer", 2), (2, "gh_logical", 4)])
def test_arg_descriptor_array(array_ind, array_type, array_ndims):
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for all three types of valid ScalarArray argument:
    'real', 'integer' and 'logical'.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(ARRAY_CODE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast, name="testkern_array_type")
    array_descriptor = metadata.arg_descriptors[array_ind]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(array_descriptor)
    expected_output = (
        f"LFRicArgDescriptor object\n"
        f"  argument_type[0]='gh_scalar_array'\n"
        f"  data_type[1]='{array_type}'\n"
        f"  access_descriptor[2]='gh_read'\n"
        f"  array_ndims[3]='{array_ndims}'")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert array_descriptor.argument_type == "gh_scalar_array"
    assert array_descriptor.data_type == array_type
    assert array_descriptor.array_ndims == array_ndims
    assert array_descriptor.function_spaces == []
    assert str(array_descriptor.access) == "READ"
    assert array_descriptor.mesh is None
    assert array_descriptor.stencil is None


def test_n_not_integer():
    ''' Tests that we raise an error when n is not an integer'''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,  gh_real,  gh_read, 0.5)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("the ScalarArray notation must be in the format 'n' "
            "where 'n' is an integer, but '0.5' was found in "
            "'arg_type(gh_scalar_array, gh_real, gh_read, 0.5)'."
            in str(excinfo.value))


def test_n_less_than_one():
    ''' Tests that we raise an error when n is less than 1'''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 0)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("the ScalarArray notation must be in the format 'n' "
            "where 'n' is an integer >= 1. However, found n = '0' in "
            "'arg_type(gh_scalar_array, gh_real, gh_read, 0)'."
            in str(excinfo.value))
