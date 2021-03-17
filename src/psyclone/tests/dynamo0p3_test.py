# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic and A. Coughtrie, Met Office,
#          C. M. Maynard, Met Office/University of Reading,
#          J. Henrichs, Bureau of Meteorology.

''' This module tests the Dynamo 0.3 API using pytest. '''

# imports
from __future__ import absolute_import, print_function
import os
import sys
import pytest

import fparser
from fparser import api as fpapi

from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import FunctionSpace
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory, InvokeSchedule, HaloExchange
from psyclone.errors import GenerationError, InternalError
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, \
    DynLoop, DynGlobalSum, HaloReadAccess, \
    KernCallArgList, DynACCEnterDataDirective, VALID_INTRINSIC_TYPES

from psyclone.gen_kernel_stub import generate
from psyclone.configuration import Config
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.psyir.transformations import LoopFuseTrans

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
# Get the root directory of this PSyclone distribution
ROOT_PATH = os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))))
# Construct the path to the default configuration file
DEFAULT_CFG_FILE = os.path.join(ROOT_PATH, "config", "psyclone.cfg")

TEST_API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"


# tests


CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                              &
          (/ arg_type(gh_scalar,   gh_real,    gh_read),         &
             arg_type(gh_field,    gh_real,    gh_inc,  w1),     &
             arg_type(gh_field,    gh_real,    gh_read, w2),     &
             arg_type(gh_operator, gh_real,    gh_read, w2, w2), &
             arg_type(gh_field,    gh_real,    gh_read, w3),     &
             arg_type(gh_scalar,   gh_integer, gh_read)          &
           /)
     type(func_type), dimension(3) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w2, gh_diff_basis),          &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a, b, c, d)
  end subroutine testkern_qr_code
end module testkern_qr
'''

# Functions


def test_arg_descriptor_wrong_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata is not of type arg_type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_field,    gh_real,    gh_read, w2)",
                        "arg_typ(gh_field,    gh_real,    gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("each 'meta_arg' entry must be of type 'arg_type'" in
            str(excinfo.value))


def test_arg_descriptor_vector():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected when we have a field vector. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change the meta-data so that the second argument is a vector
    code = CODE.replace("(gh_field,    gh_real,    gh_inc,  w1)",
                        "(gh_field*3,  gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    dkm = DynKernMetadata(ast, name=name)
    field_descriptor = dkm.arg_descriptors[1]

    # Assert correct string representation from LFRicArgDescriptor
    field_descriptor_str = str(field_descriptor)
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_inc'\n"
        "  function_space[3]='w1'")
    assert expected in field_descriptor_str

    # Check LFRicArgDescriptor argument properties
    assert field_descriptor.argument_type == "gh_field"
    assert field_descriptor.data_type == "gh_real"
    assert field_descriptor.function_space == "w1"
    assert field_descriptor.function_spaces == ['w1']
    assert str(field_descriptor.access) == "INC"
    assert field_descriptor.mesh is None
    assert field_descriptor.stencil is None
    assert field_descriptor.vector_size == 3


def test_ad_scalar_old_metadata():
    ''' Test that the LFRicArgDescriptor argument representation supports
    old-style scalar arguments' names GH_REAL and GH_INTEGER.
    TODO: This support and the test will be removed in #874.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("gh_scalar, ", "")
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")

    # Check representation of a real scalar (note: "gh_real"
    # argument type is translated to "gh_scalar")
    real_scalar_descriptor = metadata.arg_descriptors[0]
    result = str(real_scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check representation of an integer scalar (note: "gh_integer"
    # argument type is translated to "gh_scalar")
    int_scalar_descriptor = metadata.arg_descriptors[5]
    result = str(int_scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_integer'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result


def test_ad_scalar_init_wrong_argument_type():
    ''' Test that an error is raised if something other than a scalar
    is passed to the LFRicArgDescriptor._init_scalar() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get an argument which is not a scalar
    wrong_arg = metadata._inits[3]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over)._init_scalar(wrong_arg)
    assert ("Expected a scalar argument but got an argument of type "
            "'gh_operator'." in str(excinfo.value))


def test_ad_scalar_type_too_few_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has fewer than 3 args.
    Note: This general check is also valid for all other argument types.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("In the LFRic API each 'meta_arg' entry must have at least "
                "3 args, but found 2 in 'arg_type(gh_scalar, gh_real)'."
                in str(excinfo.value))


def test_ad_scalar_type_too_many_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has more than 3 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_read, w1)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("each 'meta_arg' entry must have 3 arguments if its first "
                "argument is 'gh_{r,i}scalar', but found 4 in "
                "'arg_type(gh_scalar, gh_integer, gh_read, w1)'." in
                str(excinfo.value))


def test_ad_scalar_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar,   gh_read)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API the 2nd argument of a 'meta_arg' "
            "entry should be a valid data type (one of "
            "['gh_real', 'gh_integer']), but found 'gh_read' in "
            "'arg_type(gh_scalar, gh_read)'." in str(excinfo.value))


def test_ad_scalar_init_wrong_data_type(monkeypatch):
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_scalar() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get a scalar argument descriptor and set a wrong data type
    scalar_arg = metadata._inits[0]
    scalar_arg.args[1].name = "gh_double"
    # Now try to trip the error by making the initial test think
    # that 'gh_double' is actually a valid data type
    monkeypatch.setattr(
        target=LFRicArgDescriptor, name="VALID_ARG_DATA_TYPES",
        value=LFRicArgDescriptor.VALID_ARG_DATA_TYPES + ["gh_double"])
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            scalar_arg, metadata.iterates_over)._init_scalar(scalar_arg)
    assert ("Expected one of {0} as the scalar data type but got 'gh_double'.".
            format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES) in
            str(excinfo.value))


def test_ad_scalar_type_no_write():
    ''' Tests that an error is raised when the argument descriptor metadata
    for a real or an integer scalar specifies 'GH_WRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_write)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_write'" in
                str(excinfo.value))


def test_ad_scalar_type_no_inc():
    ''' Tests that an error is raised when the argument descriptor metadata
    for a real or an integer scalar specifies 'GH_INC' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real, gh_inc)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_inc'" in
                str(excinfo.value))


def test_ad_int_scalar_type_no_sum():
    ''' Tests that an error is raised when the argument descriptor metadata
    for an integer scalar specifies 'GH_SUM' access (reduction). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_scalar,   gh_integer, gh_read)",
                        "arg_type(gh_scalar,   gh_integer, gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("reduction access 'gh_sum' is only valid with a real scalar "
            "argument, but a scalar argument with 'gh_integer' data type "
            in str(excinfo.value))


def test_ad_field_init_wrong_type():
    ''' Test that an error is raised if something other than a field
    is passed to the LFRicArgDescriptor._init_field() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get an argument which is not a field
    wrong_arg = metadata._inits[0]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over)._init_field(
                wrong_arg, metadata.iterates_over)
    assert ("Expected a field argument but got an argument of type "
            "'gh_scalar'" in str(excinfo.value))


def test_ad_field_init_wrong_iteration_space():
    ''' Test that an error is raised if a wrong iteration space
    (other than ['cell_column', 'dof']) is passed to the
    LFRicArgDescriptor._init_field() method.

    TODO #870 update this test with correct error msg once 'dofs' and
    'cells' are no longer permitted.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_arg = metadata._inits[1]
    # Set a wrong iteration space
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            field_arg, metadata.iterates_over)._init_field(
                field_arg, "ncolours")
    assert ("Invalid operates_on 'ncolours' in the kernel metadata (expected "
            "one of ['cells', 'cell_column', 'domain', 'dofs', 'dof'])." in
            str(excinfo.value))


def test_ad_field_type_too_few_args():
    ''' Tests that an error is raised when the field argument descriptor
    metadata for a field has fewer than 3 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_field,    gh_real,    gh_inc,  w1)",
                        "arg_type(gh_field,    gh_real,    gh_inc)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("each 'meta_arg' entry must have at least 4 arguments if its "
            "first argument is of ['gh_field'] type" in str(excinfo.value))


def test_ad_fld_type_too_many_args():
    ''' Tests that an error is raised when the field argument descriptor
    metadata has more than 4 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace(
        "arg_type(gh_field,    gh_real,    gh_inc,  w1)",
        "arg_type(gh_field,    gh_real,    gh_inc,  w1, w1, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("each 'meta_arg' entry must have at most 5 arguments if its "
            "first argument is of ['gh_field'] type" in str(excinfo.value))


def test_ad_fld_type_1st_arg():
    ''' Tests that an error is raised when the 1st argument is invalid. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_field,    gh_real,    gh_inc,  w1)",
                        "arg_type(gh_hedge,    gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 1st argument of a 'meta_arg' entry should be a valid "
            "argument type (one of {0}), but found 'gh_hedge'".
            format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
            in str(excinfo.value))


def test_ad_invalid_type():
    ''' Tests that an error is raised when an invalid descriptor type
    name is provided as the first argument (parsing arguments other than
    field vectors). '''
    fparser.logging.disable(fparser.logging.CRITICAL)

    # Check a FunctionVar expression but with a wrong argument type name
    code = CODE.replace("gh_operator", "gh_operato", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 1st argument of a 'meta_arg' entry should be a valid "
            "argument type (one of {0}), but found 'gh_operato'".
            format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
            in str(excinfo.value))

    # Check other type of expression (here array Slicing)
    code = CODE.replace("gh_operator", ":", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 1st argument of a 'meta_arg' entry should be a valid "
            "argument type (one of {0}), but found ':'".
            format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
            in str(excinfo.value))


def test_ad_invalid_access_type():
    ''' Tests that an error is raised when an invalid access
    name is provided as the second argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("(gh_scalar,   gh_integer, gh_read)",
                        "(gh_scalar,   gh_integer, gh_ead)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    api_config = Config.get().api_conf("dynamo0.3")
    valid_access_names = api_config.get_valid_accesses_api()
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("argument 3 of a 'meta_arg' entry must be a valid "
            "access descriptor (one of {0}), but found 'gh_ead'".
            format(valid_access_names) in str(excinfo.value))


def test_ad_invalid_iteration_space():
    ''' Tests that an error is raised in LFRicArgDescriptor
    when passing an invalid iteration space to constructor
    (other than "cells" or "dofs"). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[1]
    # Extract an arg_type object that we can use to create an
    # LFRicArgDescriptor object
    arg_type = field_descriptor._arg_type
    with pytest.raises(InternalError) as excinfo:
        _ = LFRicArgDescriptor(arg_type, "colours")
    assert ("Expected operates_on in the kernel metadata to be one of "
            "['cells', 'cell_column', 'domain', 'dofs', 'dof'] but got "
            "'colours'." in str(excinfo.value))


def test_invalid_vector_operator():
    ''' Tests that an error is raised when a vector does not use "*"
    as its operator. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace(
        "(gh_field,    gh_real,    gh_inc,  w1)",
        "(gh_field+3,  gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "must use '*' as the separator" in str(excinfo.value)


def test_invalid_vector_value_type():
    ''' Tests that an error is raised when a vector value is not a valid
    integer. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("(gh_field,    gh_real,    gh_inc,  w1)",
                        "(gh_field*n,  gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the field vector notation must be in the format 'field*n' "
            "where 'n' is an integer, but the following 'n' was found "
            in str(excinfo.value))


def test_invalid_vector_value_range():
    ''' Tests that an error is raised when a vector value is not a valid
    value (<2). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("(gh_field,    gh_real,    gh_inc,  w1)",
                        "(gh_field*1,  gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 1st argument of a 'meta_arg' entry may be a field vector "
            "with format 'field*n' where n is an integer > 1. However, "
            "found n = 1" in str(excinfo.value))

# Testing that an error is raised when a vector value is not provided is
# not required here as it causes a parse error in the generic code.


def test_missing_shape_both():
    ''' Check that we raise the correct error if a kernel requiring
    quadrature/evaluator fails to specify the shape of the evaluator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Remove the line specifying the shape of the evaluator
    code = CODE.replace(
        "     integer :: gh_shape = gh_quadrature_XYoZ\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("must also supply the shape of that evaluator by setting "
            "'gh_shape' in the kernel meta-data but this is missing "
            "for kernel 'testkern_qr_type'" in str(excinfo.value))


def test_missing_shape_basis_only():
    ''' Check that we raise the correct error if a kernel specifying
    that it needs gh_basis fails to specify the shape of the evaluator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Alter meta-data so only requires gh_basis
    code1 = CODE.replace(
        "     type(func_type), dimension(3) :: meta_funcs =  &\n"
        "          (/ func_type(w1, gh_basis),               &\n"
        "             func_type(w2, gh_diff_basis),          &\n"
        "             func_type(w3, gh_basis, gh_diff_basis) &\n",
        "     type(func_type), dimension(1) :: meta_funcs =  &\n"
        "          (/ func_type(w1, gh_basis)                &\n", 1)
    # Remove the line specifying the shape of the evaluator
    code = code1.replace(
        "     integer :: gh_shape = gh_quadrature_XYoZ\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("must also supply the shape of that evaluator by setting "
            "'gh_shape' in the kernel meta-data but this is missing "
            "for kernel 'testkern_qr_type'" in str(excinfo.value))


def test_missing_eval_shape_diff_basis_only():
    ''' Check that we raise the correct error if a kernel specifying
    that it needs gh_diff_basis fails to specify the shape of the evaluator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Alter meta-data so only requires gh_diff_basis
    code1 = CODE.replace(
        "     type(func_type), dimension(3) :: meta_funcs =  &\n"
        "          (/ func_type(w1, gh_basis),               &\n"
        "             func_type(w2, gh_diff_basis),          &\n"
        "             func_type(w3, gh_basis, gh_diff_basis) &\n",
        "     type(func_type), dimension(1) :: meta_funcs =  &\n"
        "          (/ func_type(w1, gh_diff_basis)           &\n", 1)
    # Remove the line specifying the shape of the evaluator
    code = code1.replace(
        "     integer :: gh_shape = gh_quadrature_XYoZ\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("must also supply the shape of that evaluator by setting "
            "'gh_shape' in the kernel meta-data but this is missing "
            "for kernel 'testkern_qr_type'" in str(excinfo.value))


def test_invalid_shape():
    ''' Check that we raise the correct error if a kernel requiring
    quadrature/evaluator specifies an unrecognised shape for the evaluator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Specify an invalid shape for the evaluator
    code = CODE.replace(
        "gh_shape = gh_quadrature_XYoZ",
        "gh_shape = quadrature_wrong", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("request one or more valid gh_shapes (one of ['gh_quadrature_xyoz'"
            ", 'gh_quadrature_face', 'gh_quadrature_edge', 'gh_evaluator']) "
            "but got '['quadrature_wrong']' for kernel 'testkern_qr_type'"
            in str(excinfo.value))


def test_unnecessary_shape():
    ''' Check that we raise the correct error if a kernel meta-data specifies
    an evaluator shape but does not require quadrature or an evaluator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Remove the need for basis or diff-basis functions
    code = CODE.replace(
        "     type(func_type), dimension(3) :: meta_funcs =  &\n"
        "          (/ func_type(w1, gh_basis),               &\n"
        "             func_type(w2, gh_diff_basis),          &\n"
        "             func_type(w3, gh_basis, gh_diff_basis) &\n"
        "           /)\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_qr_type' specifies one or more gh_shapes "
            "(['gh_quadrature_xyoz']) but does not need an evaluator because "
            "no basis or differential basis functions are required"
            in str(excinfo.value))


def test_kernel_call_invalid_iteration_space():
    ''' Check that we raise an exception if we attempt to generate kernel
    call for a kernel with an unsupported iteration space.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1.14_single_invoke_dofs.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        _ = psy.gen
    assert ("The LFRic API supports calls to user-supplied kernels that "
            "operate on one of ['cells', 'cell_column', 'domain'], but "
            "kernel 'testkern_dofs_code' operates on 'dof'."
            in str(excinfo.value))


def test_field(tmpdir):
    ''' Tests that a call with a set of fields, no basis functions and
    no distributed memory, produces correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = psy.gen
    output = (
        "  MODULE single_invoke_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_type\n"
        "  END MODULE single_invoke_psy")
    assert output in str(generated_code)


def test_field_deref(tmpdir, dist_mem):
    ''' Tests that a call with a set of fields (some obtained by
    de-referencing derived types) and no basis functions produces
    correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.13_single_invoke_field_deref.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)
    output = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, est_f2, m1, "
        "est_m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n")
    assert output in generated_code
    if dist_mem:
        output = "      USE mesh_mod, ONLY: mesh_type\n"
        assert output in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, est_f2, m1, est_m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, est_f2_proxy, m1_proxy, "
        "est_m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n")
    assert output in generated_code
    if dist_mem:
        output = "      TYPE(mesh_type), pointer :: mesh => null()\n"
        assert output in generated_code
    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      est_f2_proxy = est_f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      est_m2_proxy = est_m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      !\n"
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh => f1_proxy%vspace%get_mesh()\n"
        )
        assert output in generated_code
    output = (
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => est_f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => est_m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n")
    assert output in generated_code
    output = (
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = est_f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = est_f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = est_m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = est_m2_proxy%vspace%get_undf()\n"
        "      !\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (est_f2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL est_f2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (est_m2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL est_m2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=1,mesh%get_last_halo_cell(1)\n")
        assert output in generated_code
    else:
        output = (
            "      ! Call our kernels\n"
            "      !\n"
            "      DO cell=1,f1_proxy%vspace%get_ncell()\n")
        assert output in generated_code
    output = (
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, "
        "est_f2_proxy%data, m1_proxy%data, est_m2_proxy%data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !")
        assert output in generated_code


def test_field_fs(tmpdir):
    ''' Tests that a call with a set of fields making use of all
    function spaces and no basis functions produces correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.5_single_invoke_fs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    output = (
        "  MODULE single_invoke_fs_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_type(f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7)\n"
        "      USE testkern_fs_mod, ONLY: testkern_fs_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2, f3, f4, m3, "
        "m4, f5, f6, m5, m6, m7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, m7_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_any_w2(:,:) => null(), "
        "map_w0(:,:) => null(), map_w1(:,:) => null(), map_w2(:,:) => "
        "null(), map_w2broken(:,:) => null(), map_w2h(:,:) => null(), "
        "map_w2htrace(:,:) => null(), map_w2trace(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2vtrace(:,:) => null(), map_w3(:,:) "
        "=> null(), map_wchi(:,:) => null(), map_wtheta(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w0, "
        "undf_w0, ndf_w3, undf_w3, ndf_wtheta, undf_wtheta, ndf_w2h, "
        "undf_w2h, ndf_w2v, undf_w2v, ndf_w2broken, undf_w2broken, "
        "ndf_w2trace, undf_w2trace, ndf_w2htrace, undf_w2htrace, "
        "ndf_w2vtrace, undf_w2vtrace, ndf_wchi, undf_wchi, ndf_any_w2, "
        "undf_any_w2\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert output in generated_code
    output = (
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      m4_proxy = m4%get_proxy()\n"
        "      f5_proxy = f5%get_proxy()\n"
        "      f6_proxy = f6%get_proxy()\n"
        "      m5_proxy = m5%get_proxy()\n"
        "      m6_proxy = m6%get_proxy()\n"
        "      m7_proxy = m7%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => m1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => f5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => f6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => m5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => m6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => m7_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = m1_proxy%vspace%get_ndf()\n"
        "      undf_w0 = m1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2broken\n"
        "      !\n"
        "      ndf_w2broken = m4_proxy%vspace%get_ndf()\n"
        "      undf_w2broken = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2trace\n"
        "      !\n"
        "      ndf_w2trace = f5_proxy%vspace%get_ndf()\n"
        "      undf_w2trace = f5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2htrace\n"
        "      !\n"
        "      ndf_w2htrace = f6_proxy%vspace%get_ndf()\n"
        "      undf_w2htrace = f6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2vtrace\n"
        "      !\n"
        "      ndf_w2vtrace = m5_proxy%vspace%get_ndf()\n"
        "      undf_w2vtrace = m5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wchi\n"
        "      !\n"
        "      ndf_wchi = m6_proxy%vspace%get_ndf()\n"
        "      undf_wchi = m6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = m7_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = m7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_fs_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, f3_proxy%data, f4_proxy%data, "
        "m3_proxy%data, m4_proxy%data, f5_proxy%data, f6_proxy%data, "
        "m5_proxy%data, m6_proxy%data, m7_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, "
        "map_w2h(:,cell), ndf_w2v, undf_w2v, map_w2v(:,cell), ndf_w2broken, "
        "undf_w2broken, map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_clean(1)\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_type\n"
        "  END MODULE single_invoke_fs_psy")
    assert output in generated_code


def test_real_scalar(tmpdir):
    ''' Tests that we generate correct code when a kernel takes a single,
    real scalar argument (plus fields).

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data,"
        " m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code


def test_int_scalar(tmpdir):
    ''' Tests that we generate correct code when a kernel takes a single,
    integer scalar argument (plus fields).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected = (
        "    SUBROUTINE invoke_0_testkern_one_int_scalar_type"
        "(f1, iflag, f2, m1, m2)\n"
        "      USE testkern_one_int_scalar_mod, ONLY: "
        "testkern_one_int_scalar_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      INTEGER(KIND=i_def), intent(in) :: iflag\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "iflag, f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n")
    assert expected in generated_code


def test_two_real_scalars(tmpdir):
    ''' Tests that we generate correct code when a kernel has two real,
    scalar arguments.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected = (
        "    SUBROUTINE invoke_0_testkern_two_real_scalars_type(a, f1, f2, "
        "m1, m2, b)\n"
        "      USE testkern_two_real_scalars_mod, ONLY: "
        "testkern_two_real_scalars_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a, b\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_two_real_scalars_code(nlayers, a, "
        "f1_proxy%data, f2_proxy%data, m1_proxy%data, m2_proxy%data, "
        "b, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code


def test_two_int_scalars(tmpdir):
    ''' Tests that we generate correct code when a kernel has two integer,
    scalar arguments.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.6_single_invoke_2_int_scalars.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected = (
        "    SUBROUTINE invoke_0(iflag, f1, f2, m1, m2, istep)\n"
        "      USE testkern_two_int_scalars_mod, ONLY: "
        "testkern_two_int_scalars_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      INTEGER(KIND=i_def), intent(in) :: iflag, istep\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_two_int_scalars_code(nlayers, iflag, "
        "f1_proxy%data, f2_proxy%data, m1_proxy%data, m2_proxy%data, istep, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code
    # Check that we pass iflag by value in the second kernel call
    expected = (
        "        CALL testkern_two_int_scalars_code(nlayers, 1, "
        "f1_proxy%data, f2_proxy%data, m1_proxy%data, m2_proxy%data, iflag, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code


def test_two_scalars(tmpdir):
    ''' Tests that we generate correct code when a kernel has two scalar
    arguments, one real and one integer.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    expected = (
        "    SUBROUTINE invoke_0_testkern_two_scalars_type(a, f1, f2, m1, "
        "m2, istep)\n"
        "      USE testkern_two_scalars_mod, ONLY: testkern_two_scalars_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      INTEGER(KIND=i_def), intent(in) :: istep\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_two_scalars_code(nlayers, a, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, istep, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n")
    assert expected in generated_code


def test_no_vector_scalar():
    ''' Tests that we raise an error when kernel metadata erroneously
    specifies a vector scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        vectname = argname + " * 3"
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + vectname + ", gh_real, gh_read)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("vector notation is only supported for ['gh_field'] "
                "argument types but found '{0}'".format(vectname) in
                str(excinfo.value))


def test_vector_field(tmpdir):
    ''' Tests that a vector field is declared correctly in the PSy
    layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("SUBROUTINE invoke_0_testkern_coord_w0_type(f1, chi, f2)" in
            generated_code)
    assert "TYPE(field_type), intent(in) :: f1, chi(3), f2" in generated_code


def test_vector_field_2(tmpdir):
    ''' Tests that a vector field is indexed correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # all references to chi_proxy should be chi_proxy(1)
    assert "chi_proxy%" not in generated_code
    assert generated_code.count("chi_proxy(1)%vspace") == 5
    # use each chi field individually in the kernel
    assert ("chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data" in
            generated_code)


def test_vector_field_deref(tmpdir, dist_mem):
    ''' Tests that a vector field is declared correctly in the PSy
    layer when it is obtained by de-referencing a derived type in the
    Algorithm layer.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "8.1_vector_field_deref.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)
    assert ("SUBROUTINE invoke_0_testkern_coord_w0_type(f1, box_chi, f2)" in
            generated_code)
    assert ("TYPE(field_type), intent(in) :: f1, box_chi(3), f2" in
            generated_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_any_space_1(tmpdir):
    ''' Tests that any_space is implemented correctly in the PSy
    layer. Includes more than one type of any_space declaration
    and func_type basis functions on any_space.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11_any_space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("INTEGER(KIND=i_def), pointer :: map_aspc1_a(:,:) => null(), "
            "map_aspc2_b(:,:) => null(), map_w0(:,:) => null()\n"
            in generated_code)
    assert ("REAL(KIND=r_def), allocatable :: basis_aspc1_a_qr(:,:,:,:),"
            " basis_aspc2_b_qr(:,:,:,:)" in generated_code)
    assert ("ALLOCATE (basis_aspc1_a_qr(dim_aspc1_a, ndf_aspc1_a, "
            "np_xy_qr, np_z_qr))" in generated_code)
    assert ("ALLOCATE (basis_aspc2_b_qr(dim_aspc2_b, ndf_aspc2_b, "
            "np_xy_qr, np_z_qr))" in generated_code)
    assert ("map_aspc1_a => a_proxy%vspace%get_whole_dofmap()" in
            generated_code)
    assert ("map_aspc2_b => b_proxy%vspace%get_whole_dofmap()" in
            generated_code)
    assert ("CALL testkern_any_space_1_code(nlayers, a_proxy%data, rdt, "
            "b_proxy%data, c_proxy(1)%data, c_proxy(2)%data, c_proxy(3)%data, "
            "ndf_aspc1_a, undf_aspc1_a, map_aspc1_a(:,cell), "
            "basis_aspc1_a_qr, ndf_aspc2_b, undf_aspc2_b, "
            "map_aspc2_b(:,cell), basis_aspc2_b_qr, ndf_w0, undf_w0, "
            "map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in generated_code)
    assert ("DEALLOCATE (basis_aspc1_a_qr, basis_aspc2_b_qr, diff_basis_w0_qr)"
            in generated_code)


def test_any_space_2(tmpdir):
    ''' Tests that any_space is implemented correctly in the PSy
    layer. Includes multiple declarations of the same space, no
    func_type declarations and any_space used with an operator.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.1_any_space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "INTEGER(KIND=i_def), intent(in) :: istp" in generated_code
    assert ("INTEGER(KIND=i_def), pointer :: map_aspc1_a(:,:) => null()"
            in generated_code)
    assert "INTEGER(KIND=i_def) ndf_aspc1_a, undf_aspc1_a" in generated_code
    assert "ndf_aspc1_a = a_proxy%vspace%get_ndf()" in generated_code
    assert "undf_aspc1_a = a_proxy%vspace%get_undf()" in generated_code
    assert ("map_aspc1_a => a_proxy%vspace%get_whole_dofmap()"
            in generated_code)
    assert ("CALL testkern_any_space_2_code(cell, nlayers, a_proxy%data, "
            "b_proxy%data, c_proxy%ncell_3d, c_proxy%local_stencil, istp, "
            "ndf_aspc1_a, undf_aspc1_a, map_aspc1_a(:,cell))"
            in generated_code)


def test_op_any_space_different_space_1(tmpdir):
    ''' Tests that any_space is implemented correctly in the PSy layer.
    Includes different spaces for an operator and no other fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.2_any_space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "ndf_aspc2_a = a_proxy%fs_from%get_ndf()" in generated_code
    assert "ndf_aspc1_a = a_proxy%fs_to%get_ndf()" in generated_code


def test_op_any_space_different_space_2(tmpdir):
    ''' Tests that any_space is implemented correctly in the PSy
    layer in a more complicated example.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.3_any_space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "ndf_aspc1_b = b_proxy%fs_to%get_ndf()" in generated_code
    assert "dim_aspc1_b = b_proxy%fs_to%get_dim_space()" in generated_code
    assert "ndf_aspc2_b = b_proxy%fs_from%get_ndf()" in generated_code
    assert "ndf_aspc3_c = c_proxy%fs_to%get_ndf()" in generated_code
    assert "ndf_aspc4_d = d_proxy%fs_from%get_ndf()" in generated_code
    assert "undf_aspc4_d = d_proxy%fs_from%get_undf()" in generated_code
    assert "dim_aspc4_d = d_proxy%fs_from%get_dim_space()" in generated_code
    assert "ndf_aspc5_a = a_proxy%vspace%get_ndf()" in generated_code
    assert "undf_aspc5_a = a_proxy%vspace%get_undf()" in generated_code
    assert "CALL qr%compute_function(BASIS, b_proxy%fs_to, " in generated_code
    assert ("CALL qr%compute_function(BASIS, d_proxy%fs_from, " in
            generated_code)
    assert ("CALL qr%compute_function(DIFF_BASIS, d_proxy%fs_from, " in
            generated_code)
    assert "map_aspc5_a => a_proxy%vspace%get_whole_dofmap()" in generated_code
    assert "map_aspc4_d => f_proxy%vspace%get_whole_dofmap()" in generated_code


def test_op_any_discontinuous_space_1(tmpdir):
    ''' Tests that any_discontinuous_space is implemented correctly
    in the PSy layer. Includes multiple declarations of the same space,
    field vectors and any_discontinuous_space used with operators
    (same and different "to" and "from" spaces).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "11.4_any_discontinuous_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "REAL(KIND=r_def), intent(in) :: rdt" in generated_code
    assert ("INTEGER(KIND=i_def), pointer :: map_adspc1_f1(:,:) => null()"
            in generated_code)
    assert ("INTEGER(KIND=i_def) ndf_adspc1_f1, undf_adspc1_f1"
            in generated_code)
    assert "ndf_adspc1_f1 = f1_proxy(1)%vspace%get_ndf()" in generated_code
    assert "undf_adspc1_f1 = f1_proxy(1)%vspace%get_undf()" in generated_code
    assert ("map_adspc1_f1 => f1_proxy(1)%vspace%get_whole_dofmap()"
            in generated_code)
    assert "ndf_adspc3_op4 = op4_proxy%fs_to%get_ndf()" in generated_code
    assert "ndf_adspc7_op4 = op4_proxy%fs_from%get_ndf()" in generated_code
    assert ("CALL testkern_any_discontinuous_space_op_1_code(cell, nlayers, "
            "f1_proxy(1)%data, f1_proxy(2)%data, f1_proxy(3)%data, "
            "f2_proxy%data, op3_proxy%ncell_3d, op3_proxy%local_stencil, "
            "op4_proxy%ncell_3d, op4_proxy%local_stencil, rdt, "
            "ndf_adspc1_f1, undf_adspc1_f1, map_adspc1_f1(:,cell), "
            "ndf_adspc2_f2, undf_adspc2_f2, map_adspc2_f2(:,cell), "
            "ndf_adspc3_op4, ndf_adspc7_op4)" in generated_code)


def test_op_any_discontinuous_space_2(tmpdir):
    ''' Tests that any_discontinuous_space is implemented correctly in the
    PSy layer when including multiple spaces, operators on same and different
    "to" and "from" spaces and basis/differential basis functions.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "11.5_any_discontinuous_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "ndf_adspc4_f1 = f1_proxy%vspace%get_ndf()" in generated_code
    assert "undf_adspc4_f1 = f1_proxy%vspace%get_undf()" in generated_code
    assert ("map_adspc4_f1 => f1_proxy%vspace%get_whole_dofmap()"
            in generated_code)
    assert "ndf_adspc1_op1 = op1_proxy%fs_to%get_ndf()" in generated_code
    assert "ndf_adspc2_op1 = op1_proxy%fs_from%get_ndf()" in generated_code
    assert "dim_adspc4_f1 = f1_proxy%vspace%get_dim_space()" in generated_code
    assert ("diff_dim_adspc4_f1 = f1_proxy%vspace%get_dim_space_diff()"
            in generated_code)
    assert ("ALLOCATE (basis_adspc1_op1_qr(dim_adspc1_op1, ndf_adspc1_op1"
            in generated_code)
    assert ("ALLOCATE (diff_basis_adspc4_f1_qr(diff_dim_adspc4_f1, "
            "ndf_adspc4_f1" in generated_code)
    assert ("CALL qr%compute_function(BASIS, op1_proxy%fs_to, dim_adspc1_op1, "
            "ndf_adspc1_op1, basis_adspc1_op1_qr)" in generated_code)
    assert ("CALL qr%compute_function(DIFF_BASIS, f1_proxy%vspace, "
            "diff_dim_adspc4_f1, ndf_adspc4_f1, diff_basis_adspc4_f1_qr)"
            in generated_code)


def test_invoke_uniq_declns_invalid_argtype():
    ''' Tests that we raise an error when Invoke.unique_declarations() is
    called with at least one invalid argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_declarations(["not_a_type"])
    assert ("Invoke.unique_declarations() called with at least one invalid "
            "argument type. Expected one of {0} but found ['not_a_type'].".
            format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES) in
            str(excinfo.value))


def test_invoke_uniq_declns_invalid_access():
    ''' Tests that we raise an error when Invoke.unique_declarations() is
    called for an invalid access type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_declarations(["gh_field"],
                                                       access="invalid_acc")
    assert ("Invoke.unique_declarations() called with an invalid access "
            "type. Type is 'invalid_acc' instead of AccessType."
            in str(excinfo.value))


def test_invoke_uniq_declns_invalid_intrinsic():
    ''' Tests that we raise an error when Invoke.unique_declarations() is
    called for an invalid intrinsic type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_declarations(
            ["gh_scalar"], intrinsic_type="double")
    assert ("Invoke.unique_declarations() called with an invalid intrinsic "
            "argument data type. Expected one of {0} but found 'double'.".
            format(VALID_INTRINSIC_TYPES) in str(excinfo.value))


def test_invoke_uniq_declns_valid_access():
    ''' Tests that all valid access modes for user-defined arguments
    (AccessType.READ, AccessType.INC, AccessType.WRITE, AccessType.READWRITE)
    are accepted by Invoke.unique_declarations(). '''

    # Test READ and INC
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    fields_read_args = psy.invokes.invoke_list[0]\
        .unique_declarations(["gh_field"], access=AccessType.READ)
    fields_read = [arg.declaration_name for arg in fields_read_args]
    assert fields_read == ["f2", "m1", "m2"]
    fields_incremented_args = psy.invokes.invoke_list[0]\
        .unique_declarations(["gh_field"], access=AccessType.INC)
    fields_incremented = [arg.declaration_name for arg in
                          fields_incremented_args]
    assert fields_incremented == ["f1"]

    # Test WRITE
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke_w3_only_vector.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    fields_written_args = psy.invokes.invoke_list[0]\
        .unique_declarations(["gh_field"], access=AccessType.WRITE)
    fields_written = [arg.declaration_name for arg in fields_written_args]
    assert fields_written == ["f1(3)"]

    # Test READWRITE
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke_w2v.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    fields_readwritten_args = psy.invokes.invoke_list[0]\
        .unique_declarations(["gh_field"], access=AccessType.READWRITE)
    fields_readwritten = [arg.declaration_name for arg in
                          fields_readwritten_args]
    assert fields_readwritten == ["f1"]


def test_invoke_uniq_proxy_declns():
    ''' Tests that we raise an error when DynInvoke.unique_proxy_declarations()
    is called for an invalid argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_proxy_declarations(["not_a_type"])
    assert ("Expected one of {0} as a valid argument type but found "
            "['not_a_type'].".format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
            in str(excinfo.value))


def test_uniq_proxy_declns_invalid_access():
    ''' Tests that we raise an error when DynInvoke.unique_proxy_declarations()
    is called for an invalid access type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    api_config = Config.get().api_conf("dynamo0.3")
    valid_access_names = api_config.get_valid_accesses_api()
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_proxy_declarations(
            ["gh_field"],
            access="invalid_acc")
    assert ("Expected one of {0} as a valid access type but found "
            "'invalid_acc'.".format(valid_access_names) in str(excinfo.value))


def test_uniq_proxy_declns_invalid_intrinsic_type():
    ''' Tests that we raise an error when DynInvoke.unique_proxy_declarations()
    is called for an invalid intrinsic type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_proxy_declarations(
            ["gh_field"], intrinsic_type="not_intrinsic_type")
    assert ("Expected one of {0} as a valid intrinsic type but found "
            "'not_intrinsic_type'.".format(VALID_INTRINSIC_TYPES)
            in str(excinfo.value))


def test_dyninvoke_first_access():
    ''' tests that we raise an error if DynInvoke.first_access(name) is
    called for an argument name that doesn't exist '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].first_access("not_an_arg")
    assert 'Failed to find any kernel argument with name' \
        in str(excinfo.value)


def test_dyninvoke_uniq_declns_intent_inv_argtype():
    ''' Tests that we raise an error when DynInvoke.unique_declns_by_intent()
    is called with at least one invalid argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_declns_by_intent(["gh_invalid"])
    assert ("Invoke.unique_declns_by_intent() called with at least one invalid"
            " argument type. Expected one of {0} but found ['gh_invalid'].".
            format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES) in
            str(excinfo.value))


def test_dyninvoke_uniq_declns_intent_invalid_intrinsic():
    ''' Tests that we raise an error when Invoke.unique_declns_by_intent()
    is called for an invalid intrinsic type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(InternalError) as excinfo:
        psy.invokes.invoke_list[0].unique_declns_by_intent(
            ["gh_scalar"], intrinsic_type="triple")
    assert ("Invoke.unique_declns_by_intent() called with an invalid "
            "intrinsic argument data type. Expected one of {0} but "
            "found 'triple'.".format(VALID_INTRINSIC_TYPES)
            in str(excinfo.value))


def test_dyninvoke_uniq_declns_intent_fields():
    ''' Tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for 'gh_field' argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent(["gh_field"])
    args_inout = [arg.declaration_name for arg in args['inout']]
    assert args_inout == ['f1']
    assert args['out'] == []
    args_in = [arg.declaration_name for arg in args['in']]
    assert args_in == ['f2', 'm1', 'm2']


def test_dyninvoke_uniq_declns_intent_scalar():
    ''' Tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for 'gh_scalar' argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    real_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="real")
    int_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="integer")
    assert real_args['inout'] == []
    assert real_args['out'] == []
    assert int_args['inout'] == []
    assert int_args['out'] == []
    real_args_in = [arg.declaration_name for arg in real_args['in']]
    int_args_in = [arg.declaration_name for arg in int_args['in']]
    assert real_args_in == ['a']
    assert int_args_in == ['istep']


def test_dyninvoke_uniq_declns_intent_ops(tmpdir):
    ''' Tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for operator arguments. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.4_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent(["gh_operator"])
    assert args['inout'] == []
    args_out = [arg.declaration_name for arg in args['out']]
    assert args_out == ['op']
    assert args['in'] == []

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_dyninvoke_uniq_declns_intent_cma_ops(tmpdir):
    ''' Tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for columnwise operator arguments. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "20.5_multi_cma_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    args = psy.invokes.invoke_list[0]\
        .unique_declns_by_intent(["gh_columnwise_operator"])
    args_out = [arg.declaration_name for arg in args['out']]
    assert args_out == ['cma_op1']
    args_inout = [arg.declaration_name for arg in args['inout']]
    assert args_inout == ['cma_opc']
    args_in = [arg.declaration_name for arg in args['in']]
    assert args_in == ['cma_opb']

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_dyninvoke_arg_for_fs():
    ''' tests that we raise an error when DynInvoke.arg_for_funcspace() is
    called for an un-used space '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].arg_for_funcspace(FunctionSpace("wtheta",
                                                                   None))
    assert "No argument found on 'wtheta' space" \
        in str(excinfo.value)


def test_kernel_specific(tmpdir):
    ''' Test that a call to enforce boundary conditions is *not* added
    following a call to the matrix_vector_kernel_type kernel. Boundary
    conditions are now explicitly specified in the Algorithm as required.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "12_kernel_specific.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    output0 = "USE enforce_bc_kernel_mod, ONLY: enforce_bc_code"
    assert output0 not in generated_code
    output1 = "USE function_space_mod, ONLY: w1, w2, w2h, w2v\n"
    assert output1 not in generated_code
    output2 = "INTEGER(KIND=i_def) fs"
    assert output2 not in generated_code
    output3 = "INTEGER(KIND=i_def), pointer :: boundary_dofs(:,:) => null()"
    assert output3 not in generated_code
    output4 = "fs = f1%which_function_space()"
    assert output4 not in generated_code
    # We only call enforce_bc if the field is on a vector space
    output5 = (
        "IF (fs == w1 .or. fs == w2 .or. fs == w2h .or. fs == w2v .or. "
        "fs == any_w2) THEN\n"
        "        boundary_dofs => f1_proxy%vspace%get_boundary_dofs()\n"
        "      END IF")
    assert output5 not in generated_code
    output6 = (
        "IF (fs == w1 .or. fs == w2 .or. fs == w2h .or. fs == w2v .or. "
        "fs == any_w2) THEN\n"
        "          CALL enforce_bc_code(nlayers, f1_proxy%data, "
        "ndf_anyspc1_f1, undf_anyspc1_f1, map_anyspc1_f1(:,cell), "
        "boundary_dofs)")
    assert output6 not in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_multi_kernel_specific(tmpdir):
    ''' Test that a call to enforce boundary conditions is *not* added
    following multiple calls to the matrix_vector_kernel_type kernel.
    Boundary conditions must now be explicitly specified as part of the
    Algorithm.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.3_multi_kernel_specific.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    # Output must not contain any bc-related code
    output0 = "USE enforce_bc_kernel_mod, ONLY: enforce_bc_code"
    assert generated_code.count(output0) == 0
    output1 = "USE function_space_mod, ONLY: w1, w2, w2h, w2v, any_w2\n"
    assert generated_code.count(output1) == 0

    # first loop
    output1 = "INTEGER(KIND=i_def) fs\n"
    assert output1 not in generated_code
    output2 = "INTEGER(KIND=i_def), pointer :: boundary_dofs(:,:) => null()"
    assert output2 not in generated_code
    output3 = "fs = f1%which_function_space()"
    assert output3 not in generated_code
    # We only call enforce_bc if the field is on a vector space
    output4 = (
        "IF (fs == w1 .or. fs == w2 .or. fs == w2h .or. fs == w2v .or. "
        "fs == any_w2) THEN\n"
        "        boundary_dofs => f1_proxy%vspace%get_boundary_dofs()\n"
        "      END IF")
    assert output4 not in generated_code
    output5 = (
        "IF (fs == w1 .or. fs == w2 .or. fs == w2h .or. fs == w2v .or. "
        "fs == any_w2) THEN\n"
        "          CALL enforce_bc_code(nlayers, f1_proxy%data, "
        "ndf_anyspc1_f1, undf_anyspc1_f1, map_anyspc1_f1(:,cell), "
        "boundary_dofs)")
    assert output5 not in generated_code

    # second loop
    output6 = "INTEGER(KIND=i_def) fs_1\n"
    assert output6 not in generated_code
    output7 = "INTEGER(KIND=i_def), pointer :: boundary_dofs_1(:,:) => null()"
    assert output7 not in generated_code
    output8 = "fs_1 = f1%which_function_space()"
    assert output8 not in generated_code
    output9 = (
        "IF (fs_1 == w1 .or. fs_1 == w2 .or. fs_1 == w2h .or. fs_1 == w2v "
        ".or. fs_1 == any_w2) "
        "THEN\n"
        "        boundary_dofs_1 => f1_proxy%vspace%get_boundary_dofs()\n"
        "      END IF")
    assert output9 not in generated_code
    output10 = (
        "IF (fs_1 == w1 .or. fs_1 == w2 .or. fs_1 == w2h .or. fs_1 == w2v "
        ".or. fs_1 == any_w2) THEN\n"
        "          CALL enforce_bc_code(nlayers, f1_proxy%data, "
        "ndf_anyspc1_f1, undf_anyspc1_f1, map_anyspc1_f1(:,cell), "
        "boundary_dofs_1)")
    assert output10 not in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_field_bc_kernel(tmpdir):
    ''' Tests that a kernel with a particular name is recognised as a
    boundary condition kernel and that appopriate code is added to
    support this. This code is required as the dynamo0.3 api does not
    know about boundary conditions but this kernel requires them. This
    "hack" is only supported to get PSyclone to generate correct code
    for the current implementation of LFRic. Future APIs will not
    support any hacks.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.2_enforce_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen_code = str(psy.gen)
    assert ("INTEGER(KIND=i_def), pointer :: boundary_dofs_a(:,:) => "
            "null()" in gen_code)
    assert "boundary_dofs_a => a_proxy%vspace%get_boundary_dofs()" in gen_code
    assert ("CALL enforce_bc_code(nlayers, a_proxy%data, ndf_aspc1_a, "
            "undf_aspc1_a, map_aspc1_a(:,cell), boundary_dofs_a)"
            in gen_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_bc_kernel_field_only(monkeypatch, annexed, dist_mem):
    ''' Tests that the recognised boundary-condition kernel is rejected
    if it has an operator as argument instead of a field. Test with and
    without annexed as different numbers of halo exchanges are
    produced.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.2_enforce_bc_kernel.f90"),
                           api=TEST_API)
    if dist_mem and not annexed:
        idx = 1
    else:
        idx = 0
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[idx]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]
    # Monkeypatch the argument object so that it thinks it is an
    # operator rather than a field
    monkeypatch.setattr(arg, "_argument_type", value="gh_operator")
    # We have to monkey-patch the arg.ref_name() function too as
    # otherwise the first monkey-patch causes it to break. Since
    # it is a function we have to patch it with a temporary
    # function which we create using lambda.
    monkeypatch.setattr(arg, "ref_name",
                        lambda function_space=None: "vspace")
    with pytest.raises(GenerationError) as excinfo:
        _ = psy.gen
    assert ("Expected an argument of {0} type from which to look-up "
            "boundary dofs for kernel enforce_bc_code but got "
            "'gh_operator'".format(LFRicArgDescriptor.VALID_FIELD_NAMES)
            in str(excinfo.value))


def test_bc_kernel_anyspace1_only():
    ''' Tests that the recognised boundary-condition kernel is rejected
    if its argument is not specified as being on ANY_SPACE_1.

    '''
    from psyclone.dynamo0p3 import DynBoundaryConditions
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.2_enforce_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kernels = schedule.walk(DynKern)
    # Ensure that none of the arguments are listed as being on ANY_SPACE_1
    for fspace in kernels[0].arguments._unique_fss:
        fspace._orig_name = "W2"
    with pytest.raises(GenerationError) as err:
        _ = DynBoundaryConditions(invoke)
    assert ("enforce_bc_code kernel must have an argument on ANY_SPACE_1 but "
            "failed to find such an argument" in str(err.value))


def test_bc_op_kernel_wrong_args():
    '''Tests that the recognised operator boundary-condition kernel is
    rejected if it does not have exactly one argument.

    '''
    from psyclone.dynamo0p3 import DynBoundaryConditions
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kernels = schedule.walk(DynKern)
    # Ensure that the kernel has the wrong number of arguments - duplicate
    # the existing argument in the list
    kernels[0].arguments.args.append(kernels[0].arguments.args[0])
    with pytest.raises(GenerationError) as err:
        _ = DynBoundaryConditions(invoke)
    assert ("enforce_operator_bc_code kernel must have exactly one argument "
            "but found 2" in str(err.value))


def test_multikernel_invoke_1(tmpdir):
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke. We test the parts of the code that
    are incorrect at the time of writing.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check that argument names are not replicated
    assert "SUBROUTINE invoke_0(a, f1, f2, m1, m2)" in generated_code
    # Check that only one proxy initialisation is produced
    assert "f1_proxy = f1%get_proxy()" in generated_code
    # Check that we only initialise dofmaps once
    assert "map_w2 => f2_proxy%vspace%get_whole_dofmap()" in generated_code


def test_multikernel_invoke_qr(tmpdir):
    ''' Test that correct code is produced when there are multiple
    kernels with (the same) QR within an invoke. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.1_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = psy.gen
    # simple check that two kernel calls exist
    assert str(generated_code).count("CALL testkern_qr_code") == 2


def test_mkern_invoke_vec_fields():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with vector fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    # 1st test for duplication of name vector-field declaration
    assert ("TYPE(field_type), intent(in) :: f1, chi(3), chi(3)"
            not in generated_code)
    # 2nd test for duplication of name vector-field declaration
    assert ("TYPE(field_proxy_type) f1_proxy, chi_proxy(3), chi_proxy(3)"
            not in generated_code)


def test_multikern_invoke_oper():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with operators '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.4_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    # 1st test for duplication of name vector-field declaration
    assert "TYPE(field_type), intent(in) :: f1(3), f1(3)" not in generated_code
    # 2nd test for duplication of name vector-field declaration
    assert "TYPE(field_proxy_type) f1_proxy(3), f1_proxy(3)" not in \
        generated_code


def test_2kern_invoke_any_space(tmpdir):
    ''' Test correct code is generated when there are just two same
    kernels within an invoke with kernel fields declared as any_space.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.1_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("INTEGER(KIND=i_def), pointer :: map_aspc1_f1(:,:) => null(), "
            "map_aspc1_f2(:,:) => null()\n" in gen)
    assert "map_aspc1_f1 => f1_proxy%vspace%get_whole_dofmap()\n" in gen
    assert "map_aspc1_f2 => f2_proxy%vspace%get_whole_dofmap()\n" in gen
    assert (
        "        CALL testkern_any_space_2_code(cell, nlayers, f1_proxy%data,"
        " f2_proxy%data, op_proxy%ncell_3d, op_proxy%local_stencil, scalar, "
        "ndf_aspc1_f1, undf_aspc1_f1, map_aspc1_f1(:,cell))\n" in gen)
    assert "map_aspc1_f2 => f2_proxy%vspace%get_whole_dofmap()\n" in gen
    assert (
        "        CALL testkern_any_space_2_code(cell, nlayers, f2_proxy%data,"
        " f1_proxy%data, op_proxy%ncell_3d, op_proxy%local_stencil, scalar, "
        "ndf_aspc1_f2, undf_aspc1_f2, map_aspc1_f2(:,cell))\n" in gen)


def test_multikern_invoke_any_space(tmpdir):
    ''' Test that we generate correct code when there are multiple
    kernels within an invoke with kernel fields declared as any_space.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("INTEGER(KIND=i_def), pointer :: map_aspc1_f1(:,:) => null(), "
            "map_aspc1_f2(:,:) => null(), map_aspc2_f1(:,:) => null(), "
            "map_aspc2_f2(:,:) => null(), map_w0(:,:) => null()" in gen)
    assert (
        "REAL(KIND=r_def), allocatable :: basis_aspc1_f1_qr(:,:,:,:), "
        "basis_aspc2_f2_qr(:,:,:,:), diff_basis_w0_qr(:,:,:,:), "
        "basis_aspc1_f2_qr(:,:,:,:), basis_aspc2_f1_qr(:,:,:,:)" in gen)
    assert "ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()" in gen
    assert "ndf_aspc2_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert "ndf_w0 = f3_proxy(1)%vspace%get_ndf()" in gen
    assert "ndf_aspc1_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert ("CALL qr%compute_function(BASIS, f2_proxy%vspace, "
            "dim_aspc1_f2, ndf_aspc1_f2, basis_aspc1_f2_qr)" in gen)
    assert (
        "      map_aspc1_f1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc2_f2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => f3_proxy(1)%vspace%get_whole_dofmap()\n"
        "      map_aspc1_f2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc2_f1 => f1_proxy%vspace%get_whole_dofmap()\n"
        in gen)
    assert ("CALL testkern_any_space_1_code(nlayers, f1_proxy%data, rdt, "
            "f2_proxy%data, f3_proxy(1)%data, f3_proxy(2)%data, "
            "f3_proxy(3)%data, ndf_aspc1_f1, undf_aspc1_f1, "
            "map_aspc1_f1(:,cell), basis_aspc1_f1_qr, ndf_aspc2_f2, "
            "undf_aspc2_f2, map_aspc2_f2(:,cell), basis_aspc2_f2_qr, ndf_w0, "
            "undf_w0, map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr" in gen)


def test_mkern_invoke_multiple_any_spaces(tmpdir):
    ''' Test that we generate correct code when there are multiple
    kernels within an invoke with kernel fields declared as any_space.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()" in gen
    assert ("CALL qr%compute_function(BASIS, f1_proxy%vspace, "
            "dim_aspc1_f1, ndf_aspc1_f1, basis_aspc1_f1_qr)" in gen)
    assert "ndf_aspc2_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert ("CALL qr%compute_function(BASIS, f2_proxy%vspace, "
            "dim_aspc2_f2, ndf_aspc2_f2, basis_aspc2_f2_qr)" in gen)
    assert "ndf_aspc1_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert "ndf_aspc1_op = op_proxy%fs_to%get_ndf()" in gen
    assert "ndf_aspc5_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert "ndf_aspc1_op2 = op2_proxy%fs_to%get_ndf()" in gen
    assert "ndf_aspc3_op3 = op3_proxy%fs_to%get_ndf()" in gen
    assert gen.count("ndf_aspc4_op4 = op4_proxy%fs_from%get_ndf()") == 1
    assert "ndf_aspc3_op5" not in gen
    assert "ndf_aspc4_f1" not in gen
    # testkern_any_space_1_type requires GH_BASIS on ANY_SPACE_1 and 2 and
    # DIFF_BASIS on w0
    # f1 is on ANY_SPACE_1 and f2 is on ANY_SPACE_2. f3 is on W0.
    assert ("CALL qr%compute_function(BASIS, f1_proxy%vspace, "
            "dim_aspc1_f1, ndf_aspc1_f1, basis_aspc1_f1_qr)" in gen)
    assert ("CALL qr%compute_function(BASIS, f2_proxy%vspace, "
            "dim_aspc2_f2, ndf_aspc2_f2, basis_aspc2_f2_qr)" in gen)
    # testkern_any_space_4_type needs GH_BASIS on ANY_SPACE_1 which is the
    # to-space of op2
    assert ("CALL qr%compute_function(BASIS, op2_proxy%fs_to, "
            "dim_aspc1_op2, ndf_aspc1_op2, basis_aspc1_op2_qr)" in gen)
    # Need GH_BASIS and DIFF_BASIS on ANY_SPACE_4 which is to/from-space
    # of op4
    assert ("CALL qr%compute_function(BASIS, op4_proxy%fs_from, "
            "dim_aspc4_op4, ndf_aspc4_op4, basis_aspc4_op4_qr)" in gen)
    assert ("CALL qr%compute_function(DIFF_BASIS, op4_proxy%fs_from, "
            "diff_dim_aspc4_op4, ndf_aspc4_op4, diff_basis_aspc4_op4_qr)"
            in gen)


def test_loopfuse(dist_mem, tmpdir):
    ''' Tests whether loop fuse actually fuses and whether
    multiple maps are produced or not. Multiple maps are not an
    error but it would be nicer if there were only one '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    index = 0
    if dist_mem:
        index = 4
    loop1 = schedule.children[index]
    loop2 = schedule.children[index+1]
    trans = LoopFuseTrans()
    schedule, _ = trans.apply(loop1, loop2)
    invoke.schedule = schedule
    generated_code = psy.gen
    # only one loop
    assert str(generated_code).count("DO cell") == 1
    # only one map for each space
    assert str(generated_code).count("map_w1 =>") == 1
    assert str(generated_code).count("map_w2 =>") == 1
    assert str(generated_code).count("map_w3 =>") == 1
    # kernel call tests
    kern_idxs = []
    for idx, line in enumerate(str(generated_code).split('\n')):
        if "DO cell" in line:
            do_idx = idx
        if "CALL testkern_code(" in line:
            kern_idxs.append(idx)
        if "END DO" in line:
            enddo_idx = idx
    # two kernel calls
    assert len(kern_idxs) == 2
    # both kernel calls are within the loop
    for kern_id in kern_idxs:
        assert enddo_idx > kern_id > do_idx

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_named_psy_routine(dist_mem, tmpdir):
    ''' Check that we generate a subroutine with the expected name
    if an invoke is named. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Name should be all lower-case and with spaces replaced by underscores
    assert "SUBROUTINE invoke_important_invoke" in gen_code

# Tests for LFRic stub generator


def test_stub_non_existent_filename():
    ''' Fail if the file does not exist '''
    with pytest.raises(IOError) as excinfo:
        generate("non_existent_file.f90", api=TEST_API)
    assert "File 'non_existent_file.f90' not found" in str(excinfo.value)


def test_stub_invalid_api():
    ''' fail if the specified api is not supported '''
    with pytest.raises(GenerationError) as excinfo:
        generate(os.path.join(BASE_PATH, "ru_kernel_mod.f90"), api="dynamo0.1")
    assert "Unsupported API 'dynamo0.1' specified" in str(excinfo.value)


def test_stub_file_content_not_fortran():
    ''' fail if the kernel file does not contain fortran '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "dynamo0p3_test.py"), api=TEST_API)
    assert 'no parse pattern found' \
        in str(excinfo.value)


def test_stub_file_fortran_invalid():
    ''' fail if the fortran in the kernel is not valid '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_invalid_fortran.F90"),
                 api=TEST_API)
    assert 'contain <== no parse pattern found' in str(excinfo.value)


def test_file_fortran_not_kernel():
    ''' fail if file is valid fortran but is not a kernel file '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                 api=TEST_API)
    assert 'file does not contain a module. Is it a Kernel file?' \
        in str(excinfo.value)


def test_module_name_too_short():
    ''' fail if length of kernel module name is too short '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_short_name.F90"),
                 api=TEST_API)
    assert "too short to have '_mod' as an extension" in str(excinfo.value)


def test_module_name_convention():
    ''' Fail if kernel module name does not have _mod at end. '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_wrong_mod_name.F90"),
                 api=TEST_API)
    assert "does not have '_mod' as an extension" in str(excinfo.value)


def test_kernel_datatype_not_found():
    ''' fail if kernel datatype is not found '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_no_datatype.F90"),
                 api=TEST_API)
    assert 'Kernel type testkern_type does not exist' in str(excinfo.value)


STENCIL_CODE = '''
module stencil_mod
  type, extends(kernel_type) :: stencil_type
     type(arg_type), meta_args(2) =                                   &
          (/ arg_type(gh_field, gh_real, gh_inc, w1),                 &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(cross)) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => stencil_code
  end type stencil_type
contains
  subroutine stencil_code()
  end subroutine stencil_code
end module stencil_mod
'''


def test_stencil_metadata():
    ''' Check that we can parse Kernels with stencil metadata. '''
    ast = fpapi.parse(STENCIL_CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast)

    stencil_descriptor_0 = metadata.arg_descriptors[0]
    assert stencil_descriptor_0.stencil is None
    stencil_descriptor_1 = metadata.arg_descriptors[1]
    assert stencil_descriptor_1.stencil['type'] == 'cross'
    # stencil extent is not provided in the above metadata
    assert stencil_descriptor_1.stencil['extent'] is None

    # Check other LFRicArgDescriptor argument properties for a
    # field stencil argument
    assert stencil_descriptor_1.argument_type == "gh_field"
    assert stencil_descriptor_1.data_type == "gh_real"
    assert stencil_descriptor_1.function_space == "w2"
    assert stencil_descriptor_1.function_spaces == ['w2']
    assert str(stencil_descriptor_1.access) == "READ"
    assert stencil_descriptor_1.mesh is None
    assert stencil_descriptor_1.vector_size == 1


def test_field_metadata_too_many_arguments():
    ''' Check that we raise an exception if more than 4 arguments are
    provided in the metadata for a 'gh_field' argument type. '''
    result = STENCIL_CODE.replace(
        "(gh_field, gh_real, gh_read, w2, stencil(cross))",
        "(gh_field, gh_real, gh_read, w2, stencil(cross), w1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert ("each 'meta_arg' entry must have at most 5 arguments" in
            str(excinfo.value))


def test_unsupported_second_argument():
    '''Check that we raise an exception if stencil extent is specified, as
    we do not currently support it'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(NotImplementedError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "Kernels with fixed stencil extents are not currently supported" \
        in str(excinfo.value)


def test_arg_descriptor_funcs_method_error():
    ''' Tests that an internal error is raised in LFRicArgDescriptor
    when function_spaces is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._argument_type = "gh_fire_starter"
    with pytest.raises(InternalError) as excinfo:
        _ = field_descriptor.function_spaces
    assert ("Expected a valid argument type but got 'gh_fire_starter'."
            in str(excinfo.value))


def test_dynkernmetadata_read_fs_error():
    '''Tests that an exception is raised if a field on a read only
    function space is specified as being written to by the kernel
    metadata.

    '''
    code = (
        "module testkern_chi_write_mod\n"
        "  use argument_mod\n"
        "  use kernel_mod\n"
        "  use constants_mod\n"
        "  type, extends(kernel_type) :: testkern_chi_write_type\n"
        "     type(arg_type), dimension(2) :: meta_args =          &\n"
        "          (/ arg_type(gh_field, gh_real, gh_write, wchi), &\n"
        "             arg_type(gh_field, gh_real, gh_read,  wchi)  &\n"
        "           /)\n"
        "     integer :: operates_on = cell_column\n"
        "   contains\n"
        "     procedure, nopass :: code => testkern_chi_write_code\n"
        "  end type testkern_chi_write_type\n"
        "contains\n"
        "  subroutine testkern_chi_write_code()\n"
        "  end subroutine testkern_chi_write_code\n"
        "end module testkern_chi_write_mod\n")
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as info:
        _ = DynKernMetadata(ast)
    assert ("Found kernel metadata in 'testkern_chi_write_type' that "
            "specifies writing to the read-only function space 'wchi'."
            in str(info.value))


def test_dynkernelargument_intent_invalid(dist_mem):
    ''' Tests that an error is raised in DynKernelArgument when an invalid
    intent value is found. Tests with and without distributed memory. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    if dist_mem:
        idx = 4
    else:
        idx = 0
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[idx]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]
    arg._access = "invalid"
    with pytest.raises(GenerationError) as excinfo:
        _ = arg.intent
    assert ("In the LFRic API the argument access must be one of "
            "['gh_read', 'gh_write', 'gh_readwrite', 'gh_inc', 'gh_sum'], "
            "but found 'invalid'." in str(excinfo.value))


def test_arg_ref_name_method_error1():
    ''' Tests that an internal error is raised in DynKernelArgument
    when ref_name() is called with a function space that is not
    associated with this field'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    first_argument = first_kernel.arguments.args[1]
    with pytest.raises(GenerationError) as excinfo:
        # the argument is a field and is on "w1"
        _ = first_argument.ref_name(FunctionSpace("w3", None))
    assert 'not one of the function spaces associated with this argument' \
        in str(excinfo.value)


def test_arg_ref_name_method_error2():
    ''' Tests that an internal error is raised in DynKernelArgument
    when ref_name() is called when the argument type is not a field
    or an operator.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    first_argument = first_kernel.arguments.args[1]
    first_argument._argument_type = "gh_funky_instigator"
    with pytest.raises(GenerationError) as excinfo:
        _ = first_argument.ref_name()
    assert ("DynKernelArgument.ref_name(fs): Found unsupported argument "
            "type 'gh_funky_instigator'" in str(excinfo.value))


def test_arg_intent_error():
    ''' Tests that an internal error is raised in DynKernelArgument
    when intent() is called and the argument access property is not one of
    gh_{read,write,inc,readwrite} '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    first_argument = first_kernel.arguments.args[0]
    # Mess with the internal state of this argument object
    first_argument._access = "gh_not_an_intent"
    with pytest.raises(GenerationError) as excinfo:
        _ = first_argument.intent()
    assert ("In the LFRic API the argument access must be one of "
            "['gh_read', 'gh_write', 'gh_readwrite', 'gh_inc', 'gh_sum'], "
            "but found 'gh_not_an_intent'." in str(excinfo.value))


def test_arg_intrinsic_type_error():
    ''' Tests that an internal error is raised in creating argument
    'intrinsic_type' property when an invalid 'data_type' property is
    passed from the LFRicArgDescriptor class.

    '''
    from psyclone.dynamo0p3 import DynKernelArguments
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    call = invoke_info.calls[0].kcalls[0]
    kernel_metadata = call.ktype
    # Mess with the internal state of this argument descriptor
    # data type to trigger the internal error for intrinsic type
    kernel_metadata._arg_descriptors[0]._data_type = "gh_unreal"
    expected_descriptor = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_unreal'\n"
        "  access_descriptor[2]='gh_read'\n")
    with pytest.raises(InternalError) as excinfo:
        _ = DynKernelArguments(call, None)
    assert ("DynKernelArgument.__init__(): Found unsupported data "
            "type 'gh_unreal' in the kernel argument descriptor '{0}'.".
            format(expected_descriptor) in str(excinfo.value))


@pytest.mark.skipif(
    sys.version_info > (3,),
    reason="Deepcopy of function_space not working in Python 3")
def test_no_arg_on_space(monkeypatch):
    ''' Tests that DynKernelArguments.get_arg_on_space[,_name] raise
    the appropriate error when there is no kernel argument on the
    supplied space. '''
    from psyclone.psyGen import FieldNotFoundError
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    kernel_args = first_kernel.arguments
    # Test getting the argument by the meta-data name for the function space
    arg, fspace = kernel_args.get_arg_on_space_name("w2")
    assert arg.name == "f2"
    assert fspace.orig_name == "w2"
    with pytest.raises(FieldNotFoundError) as excinfo:
        _ = kernel_args.get_arg_on_space_name("not_a_space")
    assert ("there is no field or operator with function space not_a_space" in
            str(excinfo.value))
    # Now test get_arg_on_space - we need a FunctionSpace object for this
    fspace = arg.function_space
    arg = kernel_args.get_arg_on_space(fspace)
    assert arg.name == "f2"
    # Take a deep copy of the function space object so that we get a new
    # one whose state we can monkeypatch
    import copy
    fspace = copy.deepcopy(arg.function_space)
    monkeypatch.setattr(fspace, "_mangled_name", "not_a_space_name")
    with pytest.raises(FieldNotFoundError) as excinfo:
        _ = kernel_args.get_arg_on_space(fspace)
    assert ("there is no field or operator with function space w2 (mangled "
            "name = 'not_a_space_name')" in str(excinfo.value))


def test_arg_descriptor_func_method_error():
    ''' Tests that an internal error is raised in LFRicArgDescriptor
    when function_space is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._argument_type = "gh_fire_starter"
    with pytest.raises(InternalError) as excinfo:
        _ = field_descriptor.function_space
    assert ("Expected a valid argument type but got 'gh_fire_starter'."
            in str(excinfo.value))


def test_arg_descriptor_fld():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for a field argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[1]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(field_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_inc'\n"
        "  function_space[3]='w1'")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert field_descriptor.argument_type == "gh_field"
    assert field_descriptor.data_type == "gh_real"
    assert field_descriptor.function_space == "w1"
    assert field_descriptor.function_spaces == ['w1']
    assert str(field_descriptor.access) == "INC"
    assert field_descriptor.mesh is None
    assert field_descriptor.stencil is None
    assert field_descriptor.vector_size == 1


def test_arg_descriptor_real_scalar():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for a real scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    scalar_descriptor = metadata.arg_descriptors[0]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert scalar_descriptor.argument_type == "gh_scalar"
    assert scalar_descriptor.data_type == "gh_real"
    assert scalar_descriptor.function_space is None
    assert scalar_descriptor.function_spaces == []
    assert str(scalar_descriptor.access) == "READ"
    assert scalar_descriptor.mesh is None
    assert scalar_descriptor.stencil is None
    assert scalar_descriptor.vector_size == 0


def test_arg_descriptor_int_scalar():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for an integer scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    scalar_descriptor = metadata.arg_descriptors[5]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_integer'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert scalar_descriptor.argument_type == "gh_scalar"
    assert scalar_descriptor.data_type == "gh_integer"
    assert scalar_descriptor.function_space is None
    assert scalar_descriptor.function_spaces == []
    assert str(scalar_descriptor.access) == "READ"
    assert scalar_descriptor.mesh is None
    assert scalar_descriptor.stencil is None
    assert scalar_descriptor.vector_size == 0


def test_arg_descriptor_str_error():
    ''' Tests that an internal error is raised in LFRicArgDescriptor
    when __str__() is called and the internal type is an unexpected
    value. It should not be possible to get to here so we need to
    mess about with internal values to trip this.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._argument_type = "gh_fire_starter"
    with pytest.raises(InternalError) as excinfo:
        _ = str(field_descriptor)
    assert ("Expected a valid argument type but got 'gh_fire_starter'."
            in str(excinfo.value))


def test_arg_desc_func_space_tofrom_err():
    ''' Tests that an internal error is raised in LFRicArgDescriptor
    when function_space_to or function_space_from is called and the
    internal type is not an operator argument.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    with pytest.raises(InternalError) as excinfo:
        _ = field_descriptor.function_space_to
    assert ("In the LFRic API 'function_space_to' only makes sense "
            "for one of ['gh_operator', 'gh_columnwise_operator'], but "
            "this is a 'gh_scalar'") in str(excinfo.value)
    with pytest.raises(InternalError) as excinfo:
        _ = field_descriptor.function_space_from
    assert ("In the LFRic API 'function_space_from' only makes sense "
            "for one of ['gh_operator', 'gh_columnwise_operator'], but "
            "this is a 'gh_scalar'") in str(excinfo.value)


def test_unrecognised_fspace_error():
    ''' Tests that an error is raised in FunctionSpace initialisation when
    an unrecognised function space is supplied.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    with pytest.raises(InternalError) as excinfo:
        _ = FunctionSpace("not_a_space", first_kernel.arguments)
    assert ("Unrecognised function space 'not_a_space'. The supported spaces "
            "are {0}".format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES) in
            str(excinfo.value))


def test_mangle_no_space_error():
    ''' Tests that an error is raised in FunctionSpace.mangled_name when
    none of the provided kernel arguments are on the specified space.

    '''
    from psyclone.psyGen import FieldNotFoundError
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    with pytest.raises(FieldNotFoundError) as excinfo:
        _ = FunctionSpace("any_space_7", first_kernel.arguments).mangled_name
    assert ("No kernel argument found for function space 'any_space_7'"
            in str(excinfo.value))


def test_mangle_function_space():
    ''' Tests that we correctly mangle the function space name, including
    the creation of its short name.

    '''
    # Test any_space
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5.2_multikernel_invokes.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    fs_name = "any_space_2"
    mangled_name = FunctionSpace(fs_name, first_kernel.arguments).mangled_name
    short_name = FunctionSpace(fs_name, first_kernel.arguments).short_name
    assert mangled_name == "aspc2_f2"
    assert short_name == "aspc2"
    # Test any_discontinuous_space
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "11.4_any_discontinuous_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    fs_name = "any_discontinuous_space_1"
    mangled_name = FunctionSpace(fs_name, first_kernel.arguments).mangled_name
    short_name = FunctionSpace(fs_name, first_kernel.arguments).short_name
    assert mangled_name == "adspc1_f1"
    assert short_name == "adspc1"


def test_no_mangle_specified_function_space():
    ''' Test that we do not name-mangle a function space that is not
    any_space or any_discontinuous_space. Also test that an attempt to
    create a short name for such a space will fail.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    fs_name = "w2"
    mangled_name = FunctionSpace(fs_name, first_kernel.arguments).mangled_name
    short_name = FunctionSpace(fs_name, first_kernel.arguments).short_name
    assert mangled_name == fs_name
    assert short_name == fs_name
    # Try to call the internal _mangle_fs_name function with a FS name other
    # than any_*_space name (not allowed)
    with pytest.raises(InternalError) as excinfo:
        _ = FunctionSpace(fs_name, first_kernel.arguments)._mangle_fs_name()
    assert ("_mangle_fs_name: function space '{0}' is not one of {1} or {2} "
            "spaces.".format(fs_name, FunctionSpace.VALID_ANY_SPACE_NAMES,
                             FunctionSpace.VALID_ANY_DISCONTINUOUS_SPACE_NAMES)
            in str(excinfo.value))
    # Try to create a short name for this function space (not allowed)
    with pytest.raises(InternalError) as excinfo:
        _ = FunctionSpace(fs_name, first_kernel.arguments)._shorten_fs_name()
    assert ("_shorten_fs_name: function space '{0}' is not one of {1} or {2} "
            "spaces.".format(fs_name, FunctionSpace.VALID_ANY_SPACE_NAMES,
                             FunctionSpace.VALID_ANY_DISCONTINUOUS_SPACE_NAMES)
            in str(excinfo.value))


def test_fsdescriptors_get_descriptor():
    ''' Test that FSDescriptors.get_descriptor() raises the expected error
    when passed a function space for which there is no corresponding kernel
    argument '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.coded_kernels()[0]
    fspace = FunctionSpace("w0", None)
    with pytest.raises(GenerationError) as excinfo:
        first_kernel.fs_descriptors.get_descriptor(fspace)
    assert "there is no descriptor for function space w0" in str(excinfo.value)


def test_arg_descriptor_init_error(monkeypatch):
    ''' Tests that an internal error is raised in LFRicArgDescriptor
    when an invalid argument type is provided. However, this error never
    gets tripped due to an earlier test so we need to force the error by
    changing the internal state.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # TODO in #874: Remove code replacement for the old-syle field metadata
    code = CODE.replace(
        "arg_type(gh_field,    gh_real,    gh_inc,  w1)",
        "arg_type(gh_field, gh_inc, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[1]
    # Extract an arg_type object that we can use to create an
    # LFRicArgDescriptor object
    arg_type = field_descriptor._arg_type
    # Now try to trip the error by making the initial test think
    # that 'GH_INVALID' is actually valid
    monkeypatch.setattr(
        target=LFRicArgDescriptor, name="VALID_ARG_TYPE_NAMES",
        value=LFRicArgDescriptor.VALID_ARG_TYPE_NAMES + ["GH_INVALID"])
    arg_type.args[0].name = "GH_INVALID"
    with pytest.raises(InternalError) as excinfo:
        _ = LFRicArgDescriptor(arg_type, metadata.iterates_over)
    assert ("Failed argument validation for the 'meta_arg' entry "
            "'arg_type(GH_INVALID, gh_inc, w1)', should not get to "
            "here." in str(excinfo.value))


def test_func_descriptor_repr():
    ''' Tests the __repr__ output of a func_descriptor '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    func_descriptor = metadata.func_descriptors[0]
    func_str = repr(func_descriptor)
    assert "DynFuncDescriptor03(func_type(w1, gh_basis))" in func_str


def test_func_descriptor_str():
    ''' Tests the __str__ output of a func_descriptor '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    func_descriptor = metadata.func_descriptors[0]
    func_str = str(func_descriptor)
    output = (
        "DynFuncDescriptor03 object\n"
        "  name='func_type'\n"
        "  nargs=2\n"
        "  function_space_name[0] = 'w1'\n"
        "  operator_name[1] = 'gh_basis'")
    assert output in func_str


def test_dynkern_arg_for_fs():
    ''' Test that DynInvoke.arg_for_funcspace() raises an error if
    passed an invalid function space.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    with pytest.raises(InternalError) as err:
        _ = first_invoke.arg_for_funcspace(FunctionSpace("waah", "waah"))
    assert ("Unrecognised function space 'waah'. The supported spaces are "
            "{0}".format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES) in
            str(err.value))


def test_dist_memory_true():
    ''' Test that the distributed memory flag is on by default. '''
    Config._instance = None
    config = Config()
    config.load(config_file=DEFAULT_CFG_FILE)
    assert config.distributed_memory


def test_halo_dirty_1():
    ''' check halo_dirty call is added correctly with a simple example '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    expected = (
        "     END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n")
    assert expected in generated_code


def test_halo_dirty_2(tmpdir):
    ''' Check halo_dirty calls only for field "writers", that is fields with
    write, readwrite and inc access (not for read). '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.1_halo_writers.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    expected = (
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f5_proxy%set_dirty()\n"
        "      CALL f6_proxy%set_dirty()\n"
        "      CALL f7_proxy%set_dirty()\n"
        "      CALL f8_proxy%set_dirty()\n")

    assert expected in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_dirty_3():
    ''' check halo_dirty calls with multiple kernel calls '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = psy.gen
    assert str(generated_code).count("CALL f1_proxy%set_dirty()") == 2


def test_halo_dirty_4():
    ''' Check halo_dirty calls with field vectors. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    expected = (
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL chi_proxy(1)%set_dirty()\n"
        "      CALL chi_proxy(2)%set_dirty()\n"
        "      CALL chi_proxy(3)%set_dirty()\n"
        "      CALL f1_proxy%set_dirty()\n")
    assert expected in generated_code


def test_halo_dirty_5():
    ''' Check no halo_dirty calls for operators. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    assert "set_dirty()" not in generated_code
    assert "! Set halos dirty/clean" not in generated_code


def test_no_halo_dirty():
    '''check that no halo_dirty code is produced if distributed_memory is
    set to False'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    generated_code = str(psy.gen)
    assert "set_dirty()" not in generated_code
    assert "! Set halos dirty/clean" not in generated_code


def test_halo_exchange(tmpdir):
    ''' Test that a halo_exchange call is added for a loop with a
    stencil operation. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.2_halo_readers.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    output1 = (
        "     IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
        "      END IF\n"
        "      !\n")
    assert output1 in generated_code
    output2 = ("      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert output2 in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_exchange_inc(monkeypatch, annexed):
    '''test that appropriate halo exchange calls are added if we have a
    gh_inc operation and that the loop bounds included computation in
    the l1 halo. Test when annexed is False and True as a different
    number of halo exchanges are produced.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    output0 = (
        "      IF (a_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL a_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n")
    output1 = (
        "      IF (b_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL b_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (d_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL d_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (e_proxy(1)%is_dirty(depth=1)) THEN\n"
        "        CALL e_proxy(1)%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (e_proxy(2)%is_dirty(depth=1)) THEN\n"
        "        CALL e_proxy(2)%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (e_proxy(3)%is_dirty(depth=1)) THEN\n"
        "        CALL e_proxy(3)%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    output2 = (
        "      IF (f_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert output1 in result
    if annexed:
        assert result.count("halo_exchange") == 5
    else:
        assert output0 in result
        assert output2 in result
        assert result.count("halo_exchange") == 7


def test_no_halo_exchange_for_operator():
    ''' Test that no halo exchange is generated before a kernel that reads
    from an operator and updates a discontinuous field. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.7_operator_read.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    # This kernel reads from an operator and a scalar and these
    # do not require halos to be updated.
    assert "halo_exchange" not in result


def test_no_set_dirty_for_operator():
    ''' Test that we do not call set_dirty for an operator that is written
    by a kernel. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.6_operator_no_field_scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    # This kernel only writes to an operator and since operators are
    # cell-local this does not require us to call the is_dirty() method.
    assert "is_dirty" not in result


def test_halo_exchange_different_spaces(tmpdir):
    ''' Test that all of our different function spaces with a stencil
    access result in halo calls including any_space, any_w2 and
    any_discontinuous_space.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.3_halo_readers_all_fs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    assert result.count("halo_exchange") == 16
    # Check compilation
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_exchange_vectors_1(monkeypatch, annexed, tmpdir):
    ''' Test that halo exchange produces correct code for vector fields
    including a field with a gh_inc access. Test when annexed = False
    and True as halo exchanges are only produced when annexed = False.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4.1_halo_vector.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if annexed:
        assert result.count("halo_exchange(") == 0
    else:
        assert result.count("halo_exchange(") == 3
        for idx in range(1, 4):
            assert "f1_proxy("+str(idx)+")%halo_exchange(depth=1)" in result
        expected = ("      IF (f1_proxy(3)%is_dirty(depth=1)) THEN\n"
                    "        CALL f1_proxy(3)%halo_exchange(depth=1)\n"
                    "      END IF\n"
                    "      !\n"
                    "      DO cell=1,mesh%get_last_halo_cell(1)\n")
        assert expected in result


def test_halo_exchange_vectors(monkeypatch, annexed):
    '''Test that halo exchange produces correct code for vector
    fields. Test both a field with a stencil and a field with
    gh_inc. Test when annexed = False and True as a different number
    of halo exchanges are produced.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    if annexed:
        assert result.count("halo_exchange(") == 4
    else:
        assert result.count("halo_exchange(") == 7
        for idx in range(1, 4):
            assert "f1_proxy("+str(idx)+")%halo_exchange(depth=1)" in result
    for idx in range(1, 4):
        assert ("f2_proxy("+str(idx)+")%halo_exchange("
                "depth=f2_extent+1)" in result)
    expected = ("      IF (f2_proxy(4)%is_dirty(depth=f2_extent+1)) "
                "THEN\n"
                "        CALL f2_proxy(4)%halo_exchange(depth=f2_extent+1)\n"
                "      END IF\n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert expected in result


def test_halo_exchange_depths(tmpdir):
    ''' Test that halo exchange includes the correct halo depth with
    gh_write.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.5_halo_depth.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    expected = ("      IF (f2_proxy%is_dirty(depth=extent)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=extent)\n"
                "      END IF\n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=extent)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=extent)\n"
                "      END IF\n"
                "      !\n"
                "      IF (f4_proxy%is_dirty(depth=extent)) THEN\n"
                "        CALL f4_proxy%halo_exchange(depth=extent)\n"
                "      END IF\n"
                "      !\n"
                "      DO cell=1,mesh%get_last_edge_cell()\n")
    assert expected in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_exchange_depths_gh_inc(tmpdir, monkeypatch, annexed):
    ''' Test that halo exchange includes the correct halo depth when we
    have a gh_inc as this increases the required depth by 1 (as
    redundant computation is performed in the l1 halo). Test when
    annexed = False and True as a different number of halo exchanges
    are produced.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.6_halo_depth_2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    expected1 = (
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n")
    expected2 = (
        "      IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f3_proxy%is_dirty(depth=f3_extent+1)) THEN\n"
        "        CALL f3_proxy%halo_exchange(depth=f3_extent+1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f4_proxy%is_dirty(depth=f4_extent+1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=f4_extent+1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    if not annexed:
        assert expected1 in result
    assert expected2 in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_fs_discontinuous_inc_error():
    ''' Test that an error is raised if a discontinuous function space
    and 'gh_inc' are provided for the same field in the metadata. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for fspace in FunctionSpace.VALID_DISCONTINUOUS_NAMES:
        code = CODE.replace(
            "arg_type(gh_field,    gh_real,    gh_read, w3)",
            "arg_type(gh_field, gh_real, gh_inc, " + fspace + ")", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name="testkern_qr_type")
        assert ("In the LFRic API, allowed accesses for fields on "
                "discontinuous function spaces that are arguments to kernels "
                "that operate on either cell-columns or the domain are "
                "['gh_read', 'gh_write', 'gh_readwrite'], but found 'gh_inc' "
                "for '{0}'".format(fspace) in str(excinfo.value))


def test_fs_continuous_cells_write_or_readwrite_error():
    ''' Test that an error is raised if a field on a continuous
    function space is specified as having an access of 'gh_write'
    or 'gh_readwrite' in kernel metadata.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for fspace in FunctionSpace.CONTINUOUS_FUNCTION_SPACES:
        for acc in ["gh_write", "gh_readwrite"]:
            code = CODE.replace(
                "arg_type(gh_field,    gh_real,    gh_read, w2)",
                "arg_type(gh_field, gh_real, " + acc + ", " + fspace + ")", 1)
            ast = fpapi.parse(code, ignore_comments=False)
            with pytest.raises(ParseError) as excinfo:
                _ = DynKernMetadata(ast, name="testkern_qr_type")
            assert ("In the LFRic API, allowed accesses for fields on "
                    "continuous function spaces that are arguments to "
                    "kernels that operate on cell-columns are ['gh_read', "
                    "'gh_inc'], but found '{0}' for '{1}'".
                    format(acc, fspace) in str(excinfo.value))


def test_fs_anyspace_cells_write_or_readwrite_error():
    ''' Test that an error is raised if a field that is on 'any_space' "
    "(and therefore may be continuous) is specified as having 'gh_write' "
    "or 'gh_readwrite' access in the metadata.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for fspace in FunctionSpace.VALID_ANY_SPACE_NAMES:
        for acc in ["gh_write", "gh_readwrite"]:
            code = CODE.replace(
                "arg_type(gh_field,    gh_real,    gh_read, w2)",
                "arg_type(gh_field, gh_real, " + acc + ", " + fspace + ")", 1)
            ast = fpapi.parse(code, ignore_comments=False)
            with pytest.raises(ParseError) as excinfo:
                _ = DynKernMetadata(ast, name="testkern_qr_type")
            assert ("In the LFRic API, allowed accesses for fields on "
                    "continuous function spaces that are arguments to "
                    "kernels that operate on cell-columns are ['gh_read', "
                    "'gh_inc'], but found '{0}' for '{1}'".
                    format(acc, fspace) in str(excinfo.value))


def test_fs_anyspace_dofs_inc_error():
    ''' Test that an error is raised if a field on 'any_space' with
    'gh_inc' access is specified for a kernel that operates on DoFs. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    dof_code = CODE.replace("integer :: operates_on = cell_column",
                            "integer :: operates_on = dof", 1)
    for fspace in FunctionSpace.VALID_ANY_SPACE_NAMES:
        code = dof_code.replace(
            "arg_type(gh_field,    gh_real,    gh_inc,  w1)",
            "arg_type(gh_field, gh_real, gh_inc, " + fspace + ")", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name="testkern_qr_type")
        assert ("In the LFRic API, allowed field accesses for a kernel "
                "that operates on DoFs are ['gh_read', 'gh_write', "
                "'gh_readwrite'], but found 'gh_inc' for '{0}'".
                format(fspace) in str(excinfo.value))


def test_halo_exchange_view(capsys):
    ''' Test that the halo exchange view method returns what we expect. '''
    from psyclone.psyir.nodes.node import colored
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.2_halo_readers.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.get('invoke_0_testkern_stencil_type').schedule
    schedule.view()
    result, _ = capsys.readouterr()

    # Ensure we test for text containing the correct (colour) control codes
    sched = colored("InvokeSchedule", InvokeSchedule._colour)
    exch = colored("HaloExchange", HaloExchange._colour)

    expected = (
        sched + "[invoke='invoke_0_testkern_stencil_type', dm=True]\n"
        "    0: " + exch + "[field='f1', type='region', depth=1, "
        "check_dirty=True]\n"
        "    1: " + exch + "[field='f2', type='region', depth=f2_extent+1, "
        "check_dirty=True]\n"
        "    2: " + exch + "[field='f3', type='region', depth=1, "
        "check_dirty=True]\n"
        "    3: " + exch + "[field='f4', type='region', depth=1, "
        "check_dirty=True]\n")
    assert expected in result


def test_no_mesh_mod(tmpdir):
    '''test that we do not add a mesh module to the PSy layer if one is
    not required. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    assert "USE mesh_mod, ONLY: mesh_type" not in result
    assert "TYPE(mesh_type), pointer :: mesh => null()" not in result
    assert "mesh => a_proxy%vspace%get_mesh()" not in result


def test_mesh_mod(tmpdir):
    '''test that a mesh module is added to the PSy layer and a mesh object
    is created when required. One is required when we determine loop
    bounds for distributed memory '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    assert "USE mesh_mod, ONLY: mesh_type" in result
    assert "TYPE(mesh_type), pointer :: mesh => null()" in result
    output = ("      !\n"
              "      ! Create a mesh object\n"
              "      !\n"
              "      mesh => a_proxy%vspace%get_mesh()\n")
    assert output in result

# when we add build tests we should test that we can we get the mesh
# object from an operator


def test_field_gh_sum_invalid():
    ''' Tests that an error is raised when a field is specified with
    access type 'gh_sum'. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_field,    gh_real,    gh_read, w2)",
                        "arg_type(gh_field,    gh_real,    gh_sum,  w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API, allowed accesses for fields on "
            "continuous function spaces that are arguments to kernels "
            "that operate on cell-columns are ['gh_read', 'gh_inc'], but "
            "found 'gh_sum' for 'w2'" in str(excinfo.value))


def test_operator_gh_sum_invalid():
    ''' Tests that an error is raised when an operator is specified with
    access type 'gh_sum'. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_sum,  w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("allowed accesses for operators are ['gh_read', 'gh_write', "
            "'gh_readwrite'] because they behave as discontinuous "
            "quantities, but found 'gh_sum'" in str(excinfo.value))


def test_derived_type_arg(dist_mem, tmpdir):
    ''' Test that we generate a suitable name for a dummy variable
    in the PSy layer when its value in the algorithm layer is
    obtained from the component of a derived type or from a type-bound
    procedure call. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.2_single_invoke_1_int_from_derived_type.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check the four integer variables are named and declared correctly
    expected = (
        "    SUBROUTINE invoke_0(f1, my_obj_iflag, f2, m1, m2, "
        "my_obj_get_flag, my_obj_get_flag_1, my_obj_get_flag_2)\n")
    assert expected in gen
    expected = (
        "      INTEGER(KIND=i_def), intent(in) :: my_obj_iflag, "
        "my_obj_get_flag, my_obj_get_flag_1, my_obj_get_flag_2\n")
    assert expected in gen
    # Check that they are still named correctly when passed to the
    # kernels
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "my_obj_iflag, f2_proxy%data, m1_proxy%data, m2_proxy%data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))" in gen)
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "my_obj_get_flag, f2_proxy%data, m1_proxy%data, m2_proxy%data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))" in gen)
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "my_obj_get_flag_1, f2_proxy%data, m1_proxy%data, m2_proxy%data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))" in gen)
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "my_obj_get_flag_2, f2_proxy%data, m1_proxy%data, m2_proxy%data, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))" in gen)


def test_multiple_derived_type_args(dist_mem, tmpdir):
    ''' Test that we generate correct code when kernel arguments are
    supplied from the algorithm layer as different components of the
    same derived type object. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.3_single_invoke_multiple_derived_types.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check the four integer variables are named and declared correctly
    expected = (
        "    SUBROUTINE invoke_0(f1, obj_a_iflag, f2, m1, m2, "
        "obj_b_iflag, obj_a_obj_b, obj_b_obj_a)\n")
    assert expected in gen
    expected = (
        "      INTEGER(KIND=i_def), intent(in) :: obj_a_iflag, obj_b_iflag, "
        "obj_a_obj_b, obj_b_obj_a\n")
    assert expected in gen
    # Check that they are still named correctly when passed to the
    # kernels
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "obj_a_iflag, f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))" in gen)
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "obj_b_iflag, f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))" in gen)
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "obj_a_obj_b, f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))" in gen)
    assert (
        "CALL testkern_one_int_scalar_code(nlayers, f1_proxy%data, "
        "obj_b_obj_a, f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))" in gen)


def test_haloexchange_unknown_halo_depth():
    ''' If a stencil extent is provided in the kernel metadata then the
    value is stored in an instance of the DynHaloExchange class. This test
    checks that the value is stored as expected (although stencil extents
    in metadata are not currently supported in PSyclone).

    '''
    # load an example with an argument that has stencil metadata
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    # access the argument with stencil metadata
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[4].loop_body[0]
    stencil_arg = kernel.arguments.args[1]
    # artificially add an extent to the stencil metadata info
    stencil_arg.descriptor.stencil['extent'] = 10
    halo_exchange = schedule.children[1]
    assert halo_exchange._compute_halo_depth() == '11'


def test_haloexchange_correct_parent():
    '''Test that a dynamo haloexchange has the correct parent once it has
    been added to a schedule.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    for child in schedule.children:
        assert child.parent == schedule


def test_dynglobalsum_unsupported_argument():
    ''' Check that an instance of the DynGlobalSum class raises an
    exception for an unsupported argument type. '''
    # Get an instance of a non-scalar argument
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(InternalError) as err:
        _ = DynGlobalSum(argument)
    assert ("DynGlobalSum.init(): A global sum argument should be a scalar "
            "but found argument of type 'gh_field'." in str(err.value))


def test_dynglobalsum_unsupported_scalar():
    ''' Check that an instance of the DynGlobalSum class raises an
    exception if an unsupported scalar type is provided when distributed
    memory is enabled (dm=True).

    '''
    # Get an instance of an integer scalar
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        _ = DynGlobalSum(argument)
    assert ("DynGlobalSum currently only supports real scalars, but "
            "argument 'iflag' in Kernel 'testkern_one_int_scalar_code' "
            "has 'integer' intrinsic type." in str(err.value))


def test_dynglobalsum_nodm_error():
    ''' Check that an instance of the DynGlobalSum class raises an
    exception if it is instantiated with no distributed memory enabled
    (dm=False).

    '''
    # Get an instance of a real scalar
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as err:
        _ = DynGlobalSum(argument)
    assert ("It makes no sense to create a DynGlobalSum object when "
            "distributed memory is not enabled (dm=False)."
            in str(err.value))


def test_no_updated_args():
    ''' Check that we raise the expected exception when we encounter a
    kernel that does not write to any of its arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_field,    gh_real,    gh_inc,  w1)",
                        "arg_type(gh_field,    gh_real,    gh_read, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("An LFRic kernel must have at least one argument that is "
            "updated (written to) but found none for kernel "
            "'testkern_qr_type'." in str(excinfo.value))


def test_scalars_only_invalid():
    ''' Check that we raise the expected exception if we encounter a
    kernel that only has (read-only) scalar arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = '''
module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), meta_args(2) =                    &
          (/ arg_type(gh_scalar, gh_real,    gh_read), &
             arg_type(gh_scalar, gh_integer, gh_read)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_type
contains
  subroutine testkern_code(a, b)
  end subroutine testkern_code
end module testkern
'''
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("An LFRic kernel must have at least one argument that is "
            "updated (written to) but found none for kernel "
            "'testkern_type'." in str(excinfo.value))


def test_multiple_updated_field_args():
    ''' Check that we successfully parse a kernel that writes to more
    than one of its field arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_field,    gh_real,    gh_read, w2)",
                        "arg_type(gh_field,    gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    count = 0
    for descriptor in metadata.arg_descriptors:
        if descriptor.argument_type == "gh_field" and \
                descriptor.access != AccessType.READ:
            count += 1
    assert count == 2


def test_multiple_updated_op_args():
    ''' Check that we successfully parse the metadata for a kernel that
    writes to more than one of its field and operator arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_write, w1, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    count = 0
    for descriptor in metadata.arg_descriptors:
        if ((descriptor.argument_type == "gh_field" or
             descriptor.argument_type == "gh_operator") and
                descriptor.access != AccessType.READ):
            count += 1
    assert count == 2


def test_multiple_updated_scalar_args():
    ''' Check that we raise the expected exception when we encounter a
    kernel that writes to more than one of its field and scalar arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar,   gh_real,    gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A user-supplied LFRic kernel must not write/update a scalar "
            "argument but kernel 'testkern_qr_type' has a scalar "
            "argument with 'gh_sum' access." in str(excinfo.value))


def test_kernel_args_has_op():
    ''' Check that we raise an exception if the arg. type supplied to
    DynKernelArguments.has_operator() is not a valid operator. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    # Find the parsed code's Call class
    call = invoke_info.calls[0].kcalls[0]
    from psyclone.dynamo0p3 import DynKernelArguments
    dka = DynKernelArguments(call, None)
    with pytest.raises(GenerationError) as excinfo:
        _ = dka.has_operator(op_type="gh_field")
    assert "'op_type' must be a valid operator type" in str(excinfo.value)


def test_kerncallarglist_quad_rule_error(dist_mem, tmpdir):
    ''' Check that we raise the expected exception if we encounter an
    unsupported quadrature shape in the quad_rule() method. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "6_multiple_QR_per_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(DynLoop)[0]
    create_arg_list = KernCallArgList(loop.loop_body[0])
    # Add an invalid shape to the dict of qr rules
    create_arg_list._kern.qr_rules["broken"] = None
    with pytest.raises(NotImplementedError) as err:
        create_arg_list.quad_rule()
    assert ("no support implemented for quadrature with a shape of 'broken'"
            in str(err.value))


def test_multi_anyw2(dist_mem, tmpdir):
    ''' Check generated code works correctly when we have multiple any_w2
    fields. Particularly check that we only generate a single lookup.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.1_single_invoke_multi_anyw2.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        output = (
            "      ! Look-up dofmaps for each function space\n"
            "      !\n"
            "      map_any_w2 => f1_proxy%vspace%get_whole_dofmap()\n"
            "      !\n"
            "      ! Initialise number of DoFs for any_w2\n"
            "      !\n"
            "      ndf_any_w2 = f1_proxy%vspace%get_ndf()\n"
            "      undf_any_w2 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (f3_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f3_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=1,mesh%get_last_halo_cell(1)\n"
            "        !\n"
            "        CALL testkern_multi_anyw2_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f3_proxy%data, ndf_any_w2, "
            "undf_any_w2, map_any_w2(:,cell))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        assert output in generated_code
    else:
        output = (
            "      ! Look-up dofmaps for each function space\n"
            "      !\n"
            "      map_any_w2 => f1_proxy%vspace%get_whole_dofmap()\n"
            "      !\n"
            "      ! Initialise number of DoFs for any_w2\n"
            "      !\n"
            "      ndf_any_w2 = f1_proxy%vspace%get_ndf()\n"
            "      undf_any_w2 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
            "        !\n"
            "        CALL testkern_multi_anyw2_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f3_proxy%data, ndf_any_w2, "
            "undf_any_w2, map_any_w2(:,cell))\n"
            "      END DO")
        assert output in generated_code


def test_anyw2_vectors():
    '''Check generated code works correctly when we have any_w2 field
    vectors'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.3_single_invoke_anyw2_vector.f90"),
        api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        assert "f3_proxy(1) = f3(1)%get_proxy()" in generated_code
        assert "f3_proxy(2) = f3(2)%get_proxy()" in generated_code
        assert "f3_proxy(1)%data, f3_proxy(2)%data" in generated_code


def test_anyw2_operators(dist_mem, tmpdir):
    ''' Check generated code works correctly when we have any_w2 fields
    with operators.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.4_single_invoke_anyw2_operator.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = mm_w2_proxy%fs_from%get_ndf()\n"
        "      undf_any_w2 = mm_w2_proxy%fs_from%get_undf()\n")
    assert output in generated_code
    output = (
        "      dim_any_w2 = mm_w2_proxy%fs_from%get_dim_space()\n"
        "      ALLOCATE (basis_any_w2_qr(dim_any_w2, ndf_any_w2, "
        "np_xy_qr, np_z_qr))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(BASIS, mm_w2_proxy%fs_from, "
        "dim_any_w2, ndf_any_w2, basis_any_w2_qr)")
    assert output in generated_code


def test_anyw2_stencils(dist_mem, tmpdir):
    ''' Check generated code works correctly when we have any_w2 fields
    with stencils. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.5_single_invoke_anyw2_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap"
        "(STENCIL_CROSS,extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output in generated_code


def test_arg_discontinuous(monkeypatch, annexed):
    ''' Test that the discontinuous method in the Dynamo0.3 API argument
    class returns the correct values. Check that the code is generated
    correctly when annexed dofs are and are not computed by default as
    the number of halo exchanges produced is different in the two
    cases.

    '''

    # 1) Discontinuous fields return true
    # 1a) Check w3, wtheta and w2v in turn
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    if annexed:
        # no halo exchanges produced for the w3 example (reads from
        # continuous spaces)
        idchld_list = [0, 0, 0]
    else:
        # 3 halo exchanges produced for the w3 example (reads from
        # continuous spaces)
        idchld_list = [3, 0, 0]
    idarg_list = [4, 0, 0]
    fs_dict = dict(zip(FunctionSpace.DISCONTINUOUS_FUNCTION_SPACES[0:3],
                       zip(idchld_list, idarg_list)))
    for fspace in fs_dict.keys():
        filename = "1_single_invoke_" + fspace + ".f90"
        idchld = fs_dict[fspace][0]
        idarg = fs_dict[fspace][1]
        _, info = parse(os.path.join(BASE_PATH, filename),
                        api=TEST_API)
        psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
        schedule = psy.invokes.invoke_list[0].schedule
        kernel = schedule.children[idchld].loop_body[0]
        field = kernel.arguments.args[idarg]
        assert field.space == fspace
        assert field.discontinuous

    # 1b) w2broken, w2vtrace and wchi return true
    _, info = parse(
        os.path.join(BASE_PATH, "1.5.1_single_invoke_write_multi_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 12
    else:
        index = 13
    kernel = schedule.children[index].loop_body[0]
    # Test w2broken
    field = kernel.arguments.args[7]
    assert field.space == 'w2broken'
    assert field.discontinuous
    # Test w2vtrace
    field = kernel.arguments.args[11]
    assert field.space == 'w2vtrace'
    assert field.discontinuous
    # Test wchi
    field = kernel.arguments.args[4]
    assert field.space == 'wchi'
    assert not field.discontinuous

    # 1c) any_discontinuous_space returns true
    _, info = parse(
        os.path.join(BASE_PATH,
                     "1_single_invoke_any_discontinuous_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 0
    else:
        index = 2
    kernel = schedule.children[index].loop_body[0]
    field = kernel.arguments.args[0]
    assert field.space == 'any_discontinuous_space_1'
    assert field.discontinuous

    # 2) any_space field returns false
    _, info = parse(os.path.join(BASE_PATH, "11_any_space.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 4
    else:
        index = 5
    kernel = schedule.children[index].loop_body[0]
    field = kernel.arguments.args[0]
    assert field.space == 'any_space_1'
    assert not field.discontinuous

    # 3) Continuous field returns false
    # 3a) Test w1
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 3
    else:
        index = 4
    kernel = schedule.children[index].loop_body[0]
    field = kernel.arguments.args[1]
    assert field.space == 'w1'
    assert not field.discontinuous
    # 3b) Test w2trace and w2htrace
    _, info = parse(
        os.path.join(BASE_PATH,
                     "1.5.4_single_invoke_write_anyspace_w2trace.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 6
    else:
        index = 8
    kernel = schedule.children[index].loop_body[0]
    # Test w2trace
    field = kernel.arguments.args[3]
    assert field.space == 'w2trace'
    assert not field.discontinuous
    # Test w2htrace
    field = kernel.arguments.args[7]
    assert field.space == 'w2htrace'
    assert not field.discontinuous


def test_halo_stencil_redundant_computation():
    '''If a loop contains a kernel with a stencil access and the loop
    computes redundantly into the halo then the value of the stencil
    in the associated halo exchange is returned as type region
    irrespective of the type of kernel stencil. This is because the
    redundant computation will be performed all points (equivalent
    to a full halo) and there is no support for mixing accesses at
    different levels. In this example the kernel stencil is cross.'''

    _, info = parse(os.path.join(BASE_PATH,
                                 "19.1_single_stencil.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    stencil_halo_exchange = schedule.children[0]
    assert stencil_halo_exchange._compute_stencil_type() == "region"


def test_halo_same_stencils_no_red_comp(tmpdir):
    ''' If a halo has two or more different halo reads associated with it
    and the type of stencils are the same and the loops do not
    redundantly compute into the halo then the chosen stencil type for
    the halo exchange is the same as the kernel stencil type. In this
    case both are cross.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.8_halo_same_stencils.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    stencil_halo_exchange = schedule.children[1]
    assert stencil_halo_exchange._compute_stencil_type() == "cross"

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_different_stencils_no_red_comp(tmpdir):
    ''' If a halo has two or more different halo reads associated with it
    and the type of stencils are different and the loops do not
    redundantly compute into the halo then the chosen stencil type is
    region. In this case, one is xory and the other is cross, We could
    try to be more clever here in the future as the actual minimum is
    cross!

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.9_halo_different_stencils.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    stencil_halo_exchange = schedule.children[1]
    assert stencil_halo_exchange._compute_stencil_type() == "region"

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_comp_halo_intern_err(monkeypatch):
    '''Check that we raise an exception if the compute_halo_read_info method in
    dynhaloexchange does not find any read dependencies. This should
    never be the case. We use monkeypatch to force the exception to be
    raised'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    monkeypatch.setattr(field, "forward_read_dependencies", lambda: [])
    with pytest.raises(InternalError) as excinfo:
        halo_exchange._compute_halo_read_info()
    assert ("Internal logic error. There should be at least one read "
            "dependence for a halo exchange") in str(excinfo.value)


def test_halo_exch_1_back_dep(monkeypatch):
    '''Check that an internal error is raised if a halo exchange returns
    with more than one write dependency. It should only ever be 0 or 1.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    #
    monkeypatch.setattr(field, "backward_write_dependencies",
                        lambda ignore_halos=False: [1, 1])
    with pytest.raises(GenerationError) as excinfo:
        halo_exchange._compute_halo_write_info()
    assert ("Internal logic error. There should be at most one "
            "write dependence for a halo exchange. Found "
            "'2'") in str(excinfo.value)
    #
    monkeypatch.setattr(field, "backward_write_dependencies",
                        lambda ignore_halos=False: [])
    assert not halo_exchange._compute_halo_write_info()


def test_halo_ex_back_dep_no_call(monkeypatch):
    '''Check that an internal error is raised if a halo exchange
    write dependency is not a call.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.9_halo_different_stencils.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[1]
    field = halo_exchange.field
    write_dependencies = field.backward_write_dependencies()
    write_dependency = write_dependencies[0]
    monkeypatch.setattr(write_dependency, "_call",
                        lambda: halo_exchange)
    with pytest.raises(GenerationError) as excinfo:
        halo_exchange._compute_halo_write_info()
    # Note, we would expect the call type returned from HaloInfo to be
    # DynHaloExchange as that is the type of the halo_exchange
    # variable but lambda seems to result in the returning object
    # being of type 'function'. I'm not sure why. However, this does
    # not matter in practice as we are just trying to get PSyclone to
    # raise the appropriate exception.
    assert ("Generation Error: In HaloInfo class, field 'f2' should be from a "
            "call but found %s" % type(lambda: halo_exchange)
            in str(excinfo.value))


def test_HaloReadAccess_input_field():
    '''The HaloReadAccess class expects a DynKernelArgument or equivalent
    object as input. If this is not the case an exception is raised. This
    test checks that this exception is raised correctly.'''
    with pytest.raises(GenerationError) as excinfo:
        _ = HaloReadAccess(None)
    assert (
        "Generation Error: HaloInfo class expects an argument of type "
        "DynArgument, or equivalent, on initialisation, but found, "
        "'%s'" % type(None) in str(excinfo.value))


def test_HaloReadAccess_field_in_call():
    ''' The field passed to HaloReadAccess should be within a kernel or
    builtin. If it is not then an exception is raised. This test
    checks that this exception is raised correctly.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    with pytest.raises(GenerationError) as excinfo:
        _ = HaloReadAccess(field)
    assert ("field 'f1' should be from a call but found "
            "<class 'psyclone.dynamo0p3.DynHaloExchange'>"
            in str(excinfo.value))


def test_HaloReadAccess_field_not_reader():
    ''' The field passed to HaloReadAccess should be read within its
    associated kernel or builtin. If it is not then an exception is raised.
    This test checks that this exception is raised correctly

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke_wtheta.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as excinfo:
        _ = HaloReadAccess(argument)
    assert (
        "In HaloInfo class, field 'f1' should be one of ['gh_read', "
        "'gh_readwrite', 'gh_inc'], but found 'gh_write'"
        in str(excinfo.value))


def test_HaloRead_inv_loop_upper(monkeypatch):
    '''The upper bound of a loop in the compute_halo_read_info method within
    the HaloReadAccesss class should be recognised by the logic. If not an
    exception is raised and this test checks that this exception is
    raised correctly
    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    read_dependencies = field.forward_read_dependencies()
    read_dependency = read_dependencies[0]
    loop = read_dependency.call.parent.parent
    monkeypatch.setattr(loop, "_upper_bound_name", "invalid")
    with pytest.raises(GenerationError) as excinfo:
        halo_exchange._compute_halo_read_info()
    assert ("Internal error in HaloReadAccess._compute_from_field. Found "
            "unexpected loop upper bound name 'invalid'") in str(excinfo.value)


def test_HaloReadAccess_discontinuous_field(tmpdir):
    ''' When a discontinuous argument is read in a loop with an iteration
    space over 'ncells' then it only accesses local dofs. This test
    checks that HaloReadAccess works correctly in this situation '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_wtheta.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    arg = kernel.arguments.args[1]
    halo_access = HaloReadAccess(arg)
    assert not halo_access.max_depth
    assert halo_access.var_depth is None
    assert halo_access.literal_depth == 0
    assert halo_access.stencil_type is None

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_new_halo_exch_vect_field(monkeypatch):
    '''If a field requires (or may require) a halo exchange before it is
    accessed and it has more than one backward write dependency then it
    must be a vector (as a vector field requiring a halo exchange
    should have a halo exchange for each vector). The method
    create_halo_exchanges raises an exception if this is not the
    case. This test checks that the exception is raised
    correctly. This test relies on annexed = False as the required
    halo exchanges are not generated when annexed = True.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[7]
    kernel = loop.loop_body[0]
    f1_field = kernel.arguments.args[0]
    # by changing vector size we change
    # backward_write_dependencies. Therefore also patch this function
    # to return 3 arguments
    monkeypatch.setattr(f1_field, "backward_write_dependencies",
                        lambda ignore_halos=False: [1, 1, 1])
    monkeypatch.setattr(f1_field, "_vector_size", 1)
    with pytest.raises(GenerationError) as excinfo:
        loop.create_halo_exchanges()
    assert ("Error in create_halo_exchanges. Expecting field 'f1' to "
            "be a vector as it has multiple previous dependencies"
            in str(excinfo.value))


def test_new_halo_exch_vect_deps(monkeypatch):
    '''if a field requires (or may require) a halo exchange before it is
    called and it has more than one backward write dependencies then
    it must be a vector (as a vector field requiring a halo exchange
    should have a halo exchange for each vector) and its vector size
    must equal the number of dependencies. The method
    create_halo_exchanges raises an exception if this is not the
    case. This test checks that the exception is raised
    correctly. This test relies on annexed = False as the required
    halo exchanges are not generated when annexed = True.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[7]
    kernel = loop.loop_body[0]
    f1_field = kernel.arguments.args[0]
    # by changing vector size we change
    # backward_write_dependencies. Therefore also patch this function
    # to return 3 arguments
    monkeypatch.setattr(f1_field, "backward_write_dependencies",
                        lambda ignore_halos=False: [1, 1, 1])
    monkeypatch.setattr(f1_field, "_vector_size", 2)
    with pytest.raises(GenerationError) as excinfo:
        loop.create_halo_exchanges()
    assert (
        "Error in create_halo_exchanges. Expecting a dependence for each "
        "vector index for field 'f1' but the number of dependencies is '2' "
        "and the vector size is '3'." in str(excinfo.value))


def test_new_halo_exch_vect_deps2(monkeypatch):
    '''if a field requires (or may require) a halo exchange before it is
    called and it has more than one backward write dependencies then
    it must be a vector (as a vector field requiring a halo exchange
    should have a halo exchange for each component) and each
    dependency should be a halo exchange. The method
    create_halo_exchanges raises an exception if this is not the
    case. This test checks that the exception is raised
    correctly. This test relies on annexed = False as the required
    halo exchanges are not generated when annexed = True.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[7]
    kernel = loop.loop_body[0]
    f1_field = kernel.arguments.args[0]
    dependencies = f1_field.backward_write_dependencies()
    new_dependencies = []
    new_dependencies.extend(dependencies)
    # make one of the dependencies be me (an argument from a kernel)
    new_dependencies[2] = f1_field
    monkeypatch.setattr(f1_field, "backward_write_dependencies",
                        lambda ignore_halos=False: new_dependencies)
    with pytest.raises(GenerationError) as excinfo:
        loop.create_halo_exchanges()
    assert (
        "Error in create_halo_exchanges. Expecting all dependent nodes to be "
        "halo exchanges" in str(excinfo.value))


def test_halo_req_no_read_deps(monkeypatch):
    '''If the required method in a halo exchange object does not find any
    read dependencies then there has been an internal error and an
    exception will be raised. This test checks that this exception is
    raised correctly.'''

    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field

    monkeypatch.setattr(field, "_name", "unique")

    with pytest.raises(InternalError) as excinfo:
        _, _ = halo_exchange.required()
    assert ("Internal logic error. There should be at least one read "
            "dependence for a halo exchange" in str(excinfo.value))


def test_no_halo_exchange_annex_dofs(tmpdir, monkeypatch,
                                     annexed):
    ''' If a kernel writes to a discontinuous field and also reads from a
    continuous field then that fields annexed dofs are read (but not
    the rest of its level1 halo). If the previous modification of this
    continuous field makes the annexed dofs valid then no halo
    exchange is required. This is the case when the previous loop
    iterates over cells as it computes into the l1 halo by default
    precisely in order to ensure that the annexed dofs are correct for
    subsequent reading (whilst the rest of the l1 halo ends up being
    dirty).

    We test that this is True both when annexed dofs are computed by
    default and when they are not. In the former case we also get one
    fewer halo exchange call generated.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.7.1_halo_annexed.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if annexed:
        assert "CALL f1_proxy%halo_exchange" not in result
        assert "CALL f2_proxy%halo_exchange" not in result
    else:
        assert "CALL f1_proxy%halo_exchange" in result
        assert "CALL f2_proxy%halo_exchange" in result


def test_annexed_default():
    ''' Test that we do not compute annexed dofs by default (i.e. when
    using the default configuration file). '''
    Config._instance = None
    config = Config()
    config.load(config_file=DEFAULT_CFG_FILE)
    assert not config.api_conf(TEST_API).compute_annexed_dofs


def test_haloex_not_required(monkeypatch):
    '''The dynamic halo exchange required() logic should always return
    False if read dependencies are to annexed dofs and
    Config.compute_annexed_dofs is True, as they are computed by
    default when iterating over dofs and kept up-to-date by redundant
    computation when iterating over cells. However, it should return
    True if there are no previous write dependencies and
    Config.compute_annexed_dofs is False, as a previous writer may
    have iterated over dofs and only written to its own dofs, leaving
    the annexed dofs dirty. This test checks these two cases. Note the
    former case should currently never happen in real code as a halo
    exchange would not be added in the first place.
    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", False)
    _, info = parse(os.path.join(
        BASE_PATH, "1_single_invoke_w3.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    for index in range(3):
        haloex = schedule.children[index]
        assert haloex.required() == (True, False)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    for index in range(3):
        haloex = schedule.children[index]
        assert haloex.required() == (False, True)


def test_dyncollection_err1():
    ''' Check that the DynCollection constructor raises the expected
    error if it is not provided with a DynKern or DynInvoke. '''
    from psyclone.dynamo0p3 import DynProxies
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    with pytest.raises(InternalError) as err:
        _ = DynProxies(psy)
    assert ("DynCollection takes only a DynInvoke or a DynKern but"
            in str(err.value))


def test_dyncollection_err2(monkeypatch):
    ''' Check that the DynCollection constructor raises the expected
    error if it is not provided with a DynKern or DynInvoke. '''
    from psyclone.dynamo0p3 import DynProxies
    from psyclone.f2pygen import ModuleGen
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    # Create a valid sub-class of a DynCollection
    proxies = DynProxies(invoke)
    # Monkeypatch it to break internal state
    monkeypatch.setattr(proxies, "_invoke", None)
    with pytest.raises(InternalError) as err:
        proxies.declarations(ModuleGen(name="testmodule"))
    assert "DynCollection has neither a Kernel or an Invoke" in str(err.value)


def test_dyncelliterators_err(monkeypatch):
    ''' Check that the DynCellIterators constructor raises the expected
    error if it fails to find any field or operator arguments. '''
    from psyclone.dynamo0p3 import DynCellIterators
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    monkeypatch.setattr(invoke, "_psy_unique_vars", [])
    with pytest.raises(GenerationError) as err:
        _ = DynCellIterators(invoke)
    assert ("Cannot create an Invoke with no field/operator arguments."
            in str(err.value))

# tests for class kerncallarglist position methods


def test_kerncallarglist_positions_noquad(dist_mem):
    ''' Check that the positions methods (nlayers_positions, nqp_positions,
    ndf_positions) return the expected values when a kernel has no
    quadrature.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    index = 0
    if dist_mem:
        index = 4
    loop = schedule.children[index]
    kernel = loop.loop_body[0]
    create_arg_list = KernCallArgList(kernel)
    create_arg_list.generate()
    assert create_arg_list.nlayers_positions == [1]
    assert not create_arg_list.nqp_positions
    assert len(create_arg_list.ndf_positions) == 3
    assert create_arg_list.ndf_positions[0] == (7, "w1")
    assert create_arg_list.ndf_positions[1] == (10, "w2")
    assert create_arg_list.ndf_positions[2] == (13, "w3")


def test_kerncallarglist_positions_quad(dist_mem):
    ''' Check that the positions methods (nlayers_positions,
    nqp_positions, nqp_positions, ndf_positions) return the
    expected values when a kernel has xyoz quadrature.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    index = 0
    if dist_mem:
        index = 4
    loop = schedule.children[index]
    kernel = loop.loop_body[0]
    create_arg_list = KernCallArgList(kernel)
    create_arg_list.generate()
    assert create_arg_list.nlayers_positions == [1]
    assert len(create_arg_list.nqp_positions) == 1
    assert create_arg_list.nqp_positions[0]["horizontal"] == 21
    assert create_arg_list.nqp_positions[0]["vertical"] == 22
    assert len(create_arg_list.ndf_positions) == 3
    assert create_arg_list.ndf_positions[0] == (8, "w1")
    assert create_arg_list.ndf_positions[1] == (12, "w2")
    assert create_arg_list.ndf_positions[2] == (16, "w3")

# Class DynKernelArguments start


# (1/5) Method acc_args
def test_dynkernelarguments_acc_args_1():
    '''Test that the acc_args method in the DynKernelArguments class
    returns the expected arguments.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    kern = sched.kernels()[0]
    kern_args = kern.arguments
    acc_args = kern_args.acc_args
    assert acc_args == [
        'nlayers', 'a', 'f1_proxy', 'f1_proxy%data', 'f2_proxy',
        'f2_proxy%data', 'm1_proxy', 'm1_proxy%data', 'm2_proxy',
        'm2_proxy%data', 'ndf_w1', 'undf_w1', 'map_w1', 'ndf_w2', 'undf_w2',
        'map_w2', 'ndf_w3', 'undf_w3', 'map_w3']


# (2/5) Method acc_args
def test_dynkernelarguments_acc_args_2():
    '''Test that the acc_args method in the DynKernelArguments class
    returns the expected arguments when there is a field vector.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3_only_vector.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_w3_only_vector_type').schedule
    kern = sched.kernels()[0]
    kern_args = kern.arguments
    acc_args = kern_args.acc_args
    assert acc_args == [
        'nlayers', 'f1_proxy(1)', 'f1_proxy(2)', 'f1_proxy(3)',
        'f1_proxy(1)%data', 'f1_proxy(2)%data', 'f1_proxy(3)%data',
        'f2_proxy(1)', 'f2_proxy(2)', 'f2_proxy(3)',
        'f2_proxy(1)%data', 'f2_proxy(2)%data', 'f2_proxy(3)%data',
        'ndf_w3', 'undf_w3', 'map_w3']


# (3/5) Method acc_args
def test_dynkernelarguments_acc_args_3():
    '''Test that the acc_args method in the DynKernelArguments class
    returns the expected arguments when there is a stencil.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "19.1_single_stencil.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_stencil_type').schedule
    kern = sched.kernels()[0]
    kern_args = kern.arguments
    acc_args = kern_args.acc_args
    assert acc_args == [
        'nlayers', 'f1_proxy', 'f1_proxy%data', 'f2_proxy', 'f2_proxy%data',
        'f2_stencil_size', 'f2_stencil_dofmap', 'f3_proxy', 'f3_proxy%data',
        'f4_proxy', 'f4_proxy%data', 'ndf_w1', 'undf_w1', 'map_w1', 'ndf_w2',
        'undf_w2', 'map_w2', 'ndf_w3', 'undf_w3', 'map_w3']


# (4/5) Method acc_args
def test_dynkernelarguments_acc_args_4():
    '''Test that the acc_args method in the DynKernelArguments class
    returns the expected arguments when there is a stencil.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "19.26_single_stencil_cross2d.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_stencil_cross2d_type').schedule
    kern = sched.kernels()[0]
    kern_args = kern.arguments
    acc_args = kern_args.acc_args
    assert acc_args == [
        'nlayers', 'f1_proxy', 'f1_proxy%data', 'f2_proxy', 'f2_proxy%data',
        'f2_stencil_size', 'f2_max_branch_length', 'f2_stencil_dofmap',
        'f3_proxy', 'f3_proxy%data', 'f4_proxy', 'f4_proxy%data', 'ndf_w1',
        'undf_w1', 'map_w1', 'ndf_w2', 'undf_w2', 'map_w2', 'ndf_w3',
        'undf_w3', 'map_w3']


# (5/5) Method acc_args
def test_dynkernelarguments_acc_args_5():
    ''' Test that the acc_args method in the DynKernelArguments class
    returns the expected arguments when there is an operator.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "10_operator.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_operator_type').schedule
    kern = sched.kernels()[0]
    kern_args = kern.arguments
    acc_args = kern_args.acc_args
    assert acc_args == [
        'cell', 'nlayers', 'mm_w0_proxy', 'mm_w0_proxy%ncell_3d',
        'mm_w0_proxy%local_stencil', 'coord_proxy(1)', 'coord_proxy(2)',
        'coord_proxy(3)', 'coord_proxy(1)%data', 'coord_proxy(2)%data',
        'coord_proxy(3)%data', 'a', 'ndf_w0', 'undf_w0', 'map_w0',
        'basis_w0_qr', 'diff_basis_w0_qr', 'np_xy_qr', 'np_z_qr',
        'weights_xy_qr', 'weights_z_qr']


# (1/1) Method scalars
def test_dynkernelarguments_scalars():
    '''Test that the scalars method in the DynKernelArguments class
    returns an empty string. This is because dynamo0p3 currently does
    nothing with scalars when adding in OpenACC directives (which is
    where this method is used).

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    kern = sched.kernels()[0]
    kern_args = kern.arguments
    assert kern_args.scalars == []


# (1/1) Method data_on_device
def test_dynaccenterdatadirective_dataondevice():
    '''Test that the data_on_device method in the DynACCEnterDataDirective
    class returns None. This is because dynamo0p3 currently does not
    make use of this option.

    '''
    directive = DynACCEnterDataDirective()
    assert directive.data_on_device(None) is None

# Class DynKernelArguments end


def test_dyninvoke_runtime(tmpdir, monkeypatch):
    '''Test that run-time checks are added to the PSy-layer via dyninvoke
    in the expected way (correct location and correct code).

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected1 = (
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      USE mesh_mod, ONLY: mesh_type\n")
    assert expected1 in generated_code
    expected2 = (
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      IF (f1%which_function_space() /= W1) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'f1' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        "ed in the kernel metadata 'w1'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f2%which_function_space() /= W2) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'f2' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        "ed in the kernel metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (m1%which_function_space() /= W2) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'm1' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        "ed in the kernel metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (m2%which_function_space() /= W3) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'm2' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        "ed in the kernel metadata 'w3'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (f1_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', field 'f1' is on a read-only function space but is modi"
        "fied by kernel 'testkern_code'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      !\n"
        "      ! Initialise number of layers\n")
    assert expected2 in generated_code


def test_dynruntimechecks_anyspace(tmpdir, monkeypatch):
    '''Test that run-time checks are not added for fields where the kernel
    metadata specifies anyspace.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH, "11_any_space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected1 = (
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      USE mesh_mod, ONLY: mesh_type")
    assert expected1 in generated_code
    expected2 = (
        "      c_proxy(3) = c(3)%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      IF (c(1)%which_function_space() /= W0) THEN\n"
        "        CALL log_event(\"In alg 'any_space_example' invoke 'invoke_0"
        "_testkern_any_space_1_type', the field 'c' is passed to kernel 'test"
        "kern_any_space_1_code' but its function space is not compatible with"
        " the function space specified in the kernel metadata 'w0'.\", LOG_LE"
        "VEL_ERROR)\n"
        "      END IF\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (a_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'any_space_example' invoke 'invoke_0"
        "_testkern_any_space_1_type', field 'a' is on a read-only function sp"
        "ace but is modified by kernel 'testkern_any_space_1_code'.\", LOG_LE"
        "VEL_ERROR)\n"
        "      END IF\n"
        "      !\n"
        "      ! Initialise number of layers\n")
    assert expected2 in generated_code


def test_dynruntimechecks_vector(tmpdir, monkeypatch):
    ''' Test that run-time checks work for vector fields. '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    expected1 = (
        "      USE testkern_coord_w0_2_mod, ONLY: testkern_coord_w0_2_code\n"
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      USE mesh_mod, ONLY: mesh_type\n")
    assert expected1 in generated_code
    expected2 = (
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      IF (chi(1)%which_function_space() /= W0) THEN\n"
        "        CALL log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', the field 'chi' is passed to kernel 'testkern"
        "_coord_w0_2_code' but its function space is not compatible with the "
        "function space specified in the kernel metadata 'w0'.\", "
        "LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f1%which_function_space() /= W0) THEN\n"
        "        CALL log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', the field 'f1' is passed to kernel 'testkern_"
        "coord_w0_2_code' but its function space is not compatible with the "
        "function space specified in the kernel metadata 'w0'.\", "
        "LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (chi_proxy(1)%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', field 'chi' is on a read-only function space "
        "but is modified by kernel 'testkern_coord_w0_2_code'.\", "
        "LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f1_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', field 'f1' is on a read-only function space "
        "but is modified by kernel 'testkern_coord_w0_2_code'.\", "
        "LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      !\n"
        "      ! Initialise number of layers\n")
    assert expected2 in generated_code


def test_dynruntimechecks_multikern(tmpdir, monkeypatch):
    ''' Test that run-time checks work when there are multiple kernels and
    at least one field is specified as being on a given function space
    more than once. In this case we want to avoid checking the same
    thing twice.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected1 = (
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      USE mesh_mod, ONLY: mesh_type\n")
    assert expected1 in generated_code
    expected2 = (
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      IF (f1%which_function_space() /= W1) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'f1' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w1'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f2%which_function_space() /= W2) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'f2' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (m1%which_function_space() /= W2) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm1' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (m2%which_function_space() /= W3) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm2' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w3'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f3%which_function_space() /= W2) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'f3' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (m2%which_function_space() /= W2) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm2' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (m1%which_function_space() /= W3) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm1' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w3'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (f1_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'multi_invoke' invoke 'invoke_0', fi"
        "eld 'f1' is on a read-only function space but is modified by kernel "
        "'testkern_code'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      !\n"
        "      ! Initialise number of layers\n")
    assert expected2 in generated_code


def test_dynruntimechecks_builtins(tmpdir, monkeypatch):
    '''Test that run-time checks work when there are builtins.'''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.1_X_plus_Y_builtin.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected_code1 = (
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      TYPE(field_type), intent(in) :: f3, f1, f2\n")
    assert expected_code1 in generated_code
    expected_code2 = (
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (f3_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke' invoke 'invoke_0', f"
        "ield 'f3' is on a read-only function space but is modified by kernel"
        " 'x_plus_y'.\", LOG_LEVEL_ERROR)\n"        "      END IF\n"
        "      !\n"
        "      ! Call kernels and communication routines\n")
    assert expected_code2 in generated_code


def test_dynruntimechecks_anydiscontinuous(tmpdir, monkeypatch):
    '''Test that run-time checks work when we have checks for a field
    function space being consistent with an any_discontinuous_*
    function space.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "11.4_any_discontinuous_space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected1 = (
        "      USE testkern_any_discontinuous_space_op_1_mod, ONLY: testkern_"
        "any_discontinuous_space_op_1_code\n"
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      USE mesh_mod, ONLY: mesh_type\n")
    assert expected1 in generated_code
    expected2 = (
        "      op4_proxy = op4%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      IF (f1(1)%which_function_space() /= W3 .and. f1(1)%which_funct"
        "ion_space() /= WTHETA .and. f1(1)%which_function_space() /= W2V .and"
        ". f1(1)%which_function_space() /= W2VTRACE .and. f1(1)%which_funct"
        "ion_space() /= W2BROKEN) THEN\n"
        "        CALL log_event(\"In alg 'any_discontinuous_space_op_example_"
        "1' invoke 'invoke_0_testkern_any_discontinuous_space_op_1_type', the"
        " field 'f1' is passed to kernel 'testkern_any_discontinuous_space_op"
        "_1_code' but its function space is not compatible with the function "
        "space specified in the kernel metadata 'any_discontinuous_space_1'."
        "\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f2%which_function_space() /= W3 .and. f2%which_function_sp"
        "ace() /= WTHETA .and. f2%which_function_space() /= W2V .and. f2%whic"
        "h_function_space() /= W2VTRACE .and. f2%which_function_space() /= "
        "W2BROKEN) THEN\n"
        "        CALL log_event(\"In alg 'any_discontinuous_space_op_example_"
        "1' invoke 'invoke_0_testkern_any_discontinuous_space_op_1_type', the"
        " field 'f2' is passed to kernel 'testkern_any_discontinuous_space_op"
        "_1_code' but its function space is not compatible with the function "
        "space specified in the kernel metadata 'any_discontinuous_space_2'."
        "\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (f2_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'any_discontinuous_space_op_example_"
        "1' invoke 'invoke_0_testkern_any_discontinuous_space_op_1_type', fie"
        "ld 'f2' is on a read-only function space but is modified by kernel '"
        "testkern_any_discontinuous_space_op_1_code'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      !\n"
        "      ! Initialise number of layers\n")
    assert expected2 in generated_code


def test_dynruntimechecks_anyw2(tmpdir, monkeypatch):
    '''Test that run-time checks work when we have checks for a field
    function space being consistent with an anyw2 function
    space.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_run_time_checks", True)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "21.1_single_invoke_multi_anyw2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected1 = (
        "      USE testkern_multi_anyw2_mod, ONLY: testkern_multi_anyw2_code\n"
        "      USE log_mod, ONLY: log_event, LOG_LEVEL_ERROR\n"
        "      USE fs_continuity_mod\n"
        "      USE mesh_mod, ONLY: mesh_type\n")
    assert expected1 in generated_code
    expected2 = (
        "      f3_proxy = f3%get_proxy()\n"
        "      !\n"
        "      ! Perform run-time checks\n"
        "      !\n"
        "      ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "      IF (f1%which_function_space() /= W2 .and. f1%which_function_sp"
        "ace() /= W2H .and. f1%which_function_space() /= W2V .and. f1%which_f"
        "unction_space() /= W2BROKEN) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', the field 'f1' is passed to ker"
        "nel 'testkern_multi_anyw2_code' but its function space is not compat"
        "ible with the function space specified in the kernel metadata 'any_w"
        "2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f2%which_function_space() /= W2 .and. f2%which_function_sp"
        "ace() /= W2H .and. f2%which_function_space() /= W2V .and. f2%which_f"
        "unction_space() /= W2BROKEN) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', the field 'f2' is passed to ker"
        "nel 'testkern_multi_anyw2_code' but its function space is not compat"
        "ible with the function space specified in the kernel metadata 'any_w"
        "2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      IF (f3%which_function_space() /= W2 .and. f3%which_function_sp"
        "ace() /= W2H .and. f3%which_function_space() /= W2V .and. f3%which_f"
        "unction_space() /= W2BROKEN) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', the field 'f3' is passed to ker"
        "nel 'testkern_multi_anyw2_code' but its function space is not compat"
        "ible with the function space specified in the kernel metadata 'any_w"
        "2'.\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      ! Check that read-only fields are not modified\n"
        "      IF (f1_proxy%vspace%is_readonly()) THEN\n"
        "        CALL log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', field 'f1' is on a read-only fu"
        "nction space but is modified by kernel 'testkern_multi_anyw2_code'."
        "\", LOG_LEVEL_ERROR)\n"
        "      END IF\n"
        "      !\n"
        "      ! Initialise number of layers\n")
    assert expected2 in generated_code


def test_read_only_fields_hex(tmpdir):
    '''Test that halo exchange code is produced for read-only fields.'''

    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "24.1_read_fs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    expected = (
        "      IF (f2_proxy(1)%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy(1)%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy(2)%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy(2)%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy(3)%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy(3)%halo_exchange(depth=1)\n"
        "      END IF\n")
    assert expected in generated_code
