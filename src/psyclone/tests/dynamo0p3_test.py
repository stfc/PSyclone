# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' This module tests the Dynamo 0.3 API using pytest. '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

# imports
import os
import pytest
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, DynLoop, \
    FunctionSpace, VALID_STENCIL_TYPES, DynGlobalSum, HaloReadAccess
from psyclone.transformations import LoopFuseTrans
from psyclone.gen_kernel_stub import generate
import fparser
from fparser import api as fpapi
import utils

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


def test_get_op_wrong_name():  # pylint: disable=invalid-name
    ''' Tests that the get_operator_name() utility raises an error
    if passed the name of something that is not a valid operator '''
    from psyclone.dynamo0p3 import get_fs_operator_name
    with pytest.raises(GenerationError) as err:
        get_fs_operator_name("not_an_op", FunctionSpace("w3", None))
    assert "Unsupported name 'not_an_op' found" in str(err)


def test_get_op_orientation_name():  # pylint: disable=invalid-name
    ''' Test that get_operator_name() works for the orientation operator '''
    from psyclone.dynamo0p3 import get_fs_operator_name
    name = get_fs_operator_name("gh_orientation", FunctionSpace("w3", None))
    assert name == "orientation_w3"


CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                 &
          (/ arg_type(gh_real, gh_read),         &
             arg_type(gh_field,gh_write,w1),        &
             arg_type(gh_field,gh_read, w2),        &
             arg_type(gh_operator,gh_read, w2, w2), &
             arg_type(gh_field,gh_read, w3),        &
             arg_type(gh_integer, gh_read)          &
           /)
     type(func_type), dimension(3) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w2, gh_diff_basis),          &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure() :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a,b,c,d)
  end subroutine testkern_qr_code
end module testkern_qr
'''

# functions


def test_arg_descriptor_wrong_type():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata is not of type arg_type. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_read, w2)",
                        "arg_typ(gh_field,gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "each meta_arg entry must be of type 'arg_type'" \
        in str(excinfo.value)


def test_arg_descriptor_vector_str():  # pylint: disable=invalid-name
    ''' Test the str method of an argument descriptor containing a vector '''
    fparser.logging.disable('CRITICAL')
    # Change the meta-data so that the second argument is a vector
    code = CODE.replace("gh_field,gh_write,w1", "gh_field*3,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w1'")
    assert expected in dkm_str


def test_ad_scalar_type_too_few_args():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has fewer than 2 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_real, gh_read)",
                        "arg_type(gh_real)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have at least 2 args' \
        in str(excinfo.value)


def test_ad_scalar_type_too_many_args():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has more than 2 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_real, gh_read)",
                        "arg_type(gh_real, gh_read, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have 2 arguments if' \
        in str(excinfo.value)


def test_ad_scalar_type_no_write():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies GH_WRITE '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_real, gh_read)",
                        "arg_type(gh_real, gh_write)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("scalar arguments must be read-only (gh_read) or a reduction "
            "(['gh_sum']) but found 'gh_write'" in str(excinfo.value))


def test_ad_scalar_type_no_inc():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies GH_INC '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_real, gh_read)",
                        "arg_type(gh_real, gh_inc)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("scalar arguments must be read-only (gh_read) or a reduction "
            "(['gh_sum']) but found 'gh_inc'" in str(excinfo.value))


def test_ad_field_type_too_few_args():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata for a field has fewer than 3 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_write)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have at least 3 arguments' \
        in str(excinfo.value)


def test_ad_fld_type_too_many_args():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the argument descriptor
    metadata has more than 4 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_write,w1,w1,w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "each meta_arg entry must have at most 4 arguments if its first" \
        in str(excinfo.value)


def test_ad_fld_type_1st_arg():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the 1st argument is
    invalid'''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_hedge,gh_write,w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'the 1st argument of a meta_arg entry should be a valid ' \
        'argument type' in str(excinfo.value)


def test_ad_op_type_too_few_args():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the operator descriptor
    metadata has fewer than 4 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must have 4 arguments' in str(excinfo.value)


def test_ad_op_type_too_many_args():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the operator descriptor
    metadata has more than 4 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, w2, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must have 4 arguments' in str(excinfo.value)


def test_ad_op_type_wrong_3rd_arg():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the 3rd entry in the operator
    descriptor metadata is invalid. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, woops, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("dynamo0.3 API the 3rd argument of a meta_arg entry must be "
            "a valid function space name" in str(excinfo.value))


def test_ad_op_type_1st_arg_not_space():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when the operator descriptor
    metadata contains something that is not a valid space. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, wbroke, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must be a valid function space' in \
        str(excinfo.value)


def test_ad_invalid_type():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when an invalid descriptor type
    name is provided as the first argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_operator", "gh_operato", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '1st argument of a meta_arg entry should be a valid argument type' \
        in str(excinfo.value)


def test_ad_invalid_access_type():  # pylint: disable=invalid-name
    ''' Tests that an error is raised when an invalid access
    name is provided as the second argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_read", "gh_ead", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '2nd argument of a meta_arg entry' in str(excinfo.value)


def test_arg_descriptor_invalid_fs1():
    ''' Tests that an error is raised when an invalid function space
    name is provided as the third argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_read, w3", "gh_field,gh_read, w4", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '3rd argument of a meta_arg entry' in str(excinfo.value)


def test_arg_descriptor_invalid_fs2():
    ''' Tests that an error is raised when an invalid function space
    name is provided as the third argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w2, w2", "w2, w4", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '4th argument of a meta_arg entry' in str(excinfo.value)


def test_invalid_vector_operator():
    ''' Tests that an error is raised when a vector does not use "*"
    as it's operator. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_write,w1", "gh_field+3,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "must use '*' as the separator" in str(excinfo.value)


def test_invalid_vector_value_type():
    ''' Tests that an error is raised when a vector value is not a valid
    integer '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_write,w1", "gh_field*n,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'vector notation expects the format (field*n)' in str(excinfo.value)


def test_invalid_vector_value_range():
    ''' Tests that an error is raised when a vector value is not a valid
    value (<2) '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_write,w1", "gh_field*1,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'must contain a valid integer vector size' in str(excinfo.value)

# Testing that an error is raised when a vector value is not provided is
# not required here as it causes a parse error in the generic code.


def test_fs_descriptor_wrong_type():
    ''' Tests that an error is raised when the function space descriptor
    metadata is not of type func_type. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("func_type(w2", "funced_up_type(w2", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "each meta_func entry must be of type 'func_type'" in \
        str(excinfo.value)


def test_fs_descriptor_too_few_args():
    ''' Tests that an error is raised when there are two few arguments in
    the function space descriptor metadata (must be at least 2). '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w1, gh_basis", "w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_func entry must have at least 2 args' in str(excinfo.value)


def test_fs_desc_invalid_fs_type():
    ''' Tests that an error is raised when an invalid function space name
    is provided as the first argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w3, gh_basis", "w4, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '1st argument of a meta_func entry should be a valid function ' + \
        'space name' in str(excinfo.value)


def test_fs_desc_replicated_fs_type():
    ''' Tests that an error is raised when a function space name
    is replicated. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w3, gh_basis", "w1, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'function spaces specified in meta_funcs must be unique' \
        in str(excinfo.value)


def test_fs_desc_invalid_op_type():
    ''' Tests that an error is raised when an invalid function space
    operator name is provided as an argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w2, gh_diff_basis", "w2, gh_dif_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '2nd argument and all subsequent arguments of a meta_func ' + \
        'entry should be one of' in str(excinfo.value)


def test_fs_desc_replicated_op_type():
    ''' Tests that an error is raised when a function space
    operator name is replicated as an argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w3, gh_basis, gh_diff_basis",
                        "w3, gh_basis, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'error to specify an operator name more than once' \
        in str(excinfo.value)


def test_fsdesc_fs_not_in_argdesc():
    ''' Tests that an error is raised when a function space
    name is provided that has not been used in the arg descriptor. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w3, gh_basis", "w0, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'function spaces specified in meta_funcs must exist in ' + \
        'meta_args' in str(excinfo)


def test_missing_shape_both():  # pylint: disable=invalid-name
    ''' Check that we raise the correct error if a kernel requiring
    quadrature/evaluator fails to specify the shape of the evaluator '''
    fparser.logging.disable('CRITICAL')
    # Remove the line specifying the shape of the evaluator
    code = CODE.replace(
        "     integer, parameter :: gh_shape = gh_quadrature_XYoZ\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("must also supply the shape of that evaluator by setting "
            "'gh_shape' in the kernel meta-data but this is missing "
            "for kernel 'testkern_qr_type'" in str(excinfo))


def test_missing_shape_basis_only():  # pylint: disable=invalid-name
    ''' Check that we raise the correct error if a kernel specifying
    that it needs gh_basis fails to specify the shape of the evaluator '''
    fparser.logging.disable('CRITICAL')
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
        "     integer, parameter :: gh_shape = gh_quadrature_XYoZ\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("must also supply the shape of that evaluator by setting "
            "'gh_shape' in the kernel meta-data but this is missing "
            "for kernel 'testkern_qr_type'" in str(excinfo))


def test_missing_eval_shape_diff_basis_only():  # pylint: disable=invalid-name
    ''' Check that we raise the correct error if a kernel specifying
    that it needs gh_diff_basis fails to specify the shape of the evaluator '''
    fparser.logging.disable('CRITICAL')
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
        "     integer, parameter :: gh_shape = gh_quadrature_XYoZ\n",
        "", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("must also supply the shape of that evaluator by setting "
            "'gh_shape' in the kernel meta-data but this is missing "
            "for kernel 'testkern_qr_type'" in str(excinfo))


def test_invalid_shape():
    ''' Check that we raise the correct error if a kernel requiring
    quadrature/evaluator specifies an unrecognised shape for the evaluator '''
    fparser.logging.disable('CRITICAL')
    # Specify an invalid shape for the evaluator
    code = CODE.replace(
        "gh_shape = gh_quadrature_XYoZ",
        "gh_shape = quadrature_wrong", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    print str(excinfo)
    assert ("request a valid gh_shape (one of ['gh_quadrature_xyoz', "
            "'gh_evaluator']) but got 'quadrature_wrong' for kernel "
            "'testkern_qr_type'" in str(excinfo))


def test_unecessary_shape():
    ''' Check that we raise the correct error if a kernel meta-data specifies
    an evaluator shape but does not require quadrature or an evaluator '''
    fparser.logging.disable('CRITICAL')
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
    print str(excinfo)
    assert ("Kernel 'testkern_qr_type' specifies a gh_shape "
            "(gh_quadrature_xyoz) but does not need an evaluator because no "
            "basis or differential basis functions are required"
            in str(excinfo))


def test_field(tmpdir, f90, f90flags):
    ''' Tests that a call with a set of fields, no basis functions and
    no distributed memory, produces correct code.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)

    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    generated_code = psy.gen
    output = (
        "  MODULE single_invoke_psy\n"
        "    USE constants_mod, ONLY: r_def\n"
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type, "
        "columnwise_operator_type, columnwise_operator_proxy_type\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern, ONLY: testkern_code\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
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
        "      END DO \n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_type\n"
        "  END MODULE single_invoke_psy")
    print output
    print generated_code
    assert str(generated_code).find(output) != -1


def test_field_deref():
    ''' Tests that a call with a set of fields (some obtained by
    de-referencing derived types) and no basis functions produces
    correct code.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.13_single_invoke_field_deref.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:

        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        output = (
            "    SUBROUTINE invoke_0_testkern_type(a, f1, est_f2, m1, "
            "est_m2)\n"
            "      USE testkern, ONLY: testkern_code\n")
        assert output in generated_code
        if dist_mem:
            output = "      USE mesh_mod, ONLY: mesh_type\n"
            assert output in generated_code
        output = (
            "      REAL(KIND=r_def), intent(in) :: a\n"
            "      TYPE(field_type), intent(inout) :: f1\n"
            "      TYPE(field_type), intent(in) :: est_f2, m1, est_m2\n"
            "      INTEGER cell\n"
            "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
            "undf_w3\n")
        assert output in generated_code
        if dist_mem:
            output = "      TYPE(mesh_type), pointer :: mesh => null()\n"
            assert output in generated_code
        output = (
            "      INTEGER nlayers\n"
            "      TYPE(field_proxy_type) f1_proxy, est_f2_proxy, m1_proxy, "
            "est_m2_proxy\n"
            "      INTEGER, pointer :: map_w2(:,:) => null(), "
            "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
                "      mesh => f1%get_mesh()\n"
            )
            assert output in generated_code
        output = (
            "      !\n"
            "      ! Look-up dofmaps for each function space\n"
            "      !\n"
            "      map_w2 => est_f2_proxy%vspace%get_whole_dofmap()\n"
            "      map_w3 => est_m2_proxy%vspace%get_whole_dofmap()\n"
            "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
            "      !\n"
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
                "      IF (est_f2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL est_f2_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL m1_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (est_m2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL est_m2_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
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
            "      END DO \n")
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


def test_field_fs():
    ''' Tests that a call with a set of fields making use of all
    function spaces and no basis functions produces correct code.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.5_single_invoke_fs.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output = (
        "  MODULE single_invoke_fs_psy\n"
        "    USE constants_mod, ONLY: r_def\n"
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type, "
        "columnwise_operator_type, columnwise_operator_proxy_type\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_type(f1, f2, m1, m2, f3, f4, "
        "m3, m4)\n"
        "      USE testkern_fs, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(inout) :: f1, f3\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2, f4, m3, m4\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3, "
        "ndf_wtheta, undf_wtheta, ndf_w2h, undf_w2h, ndf_w2v, undf_w2v, "
        "ndf_any_w2, undf_any_w2\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy, "
        "f3_proxy, f4_proxy, m3_proxy, m4_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_wtheta(:,:) => null(), "
        "map_w1(:,:) => null(), map_any_w2(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2h(:,:) => null()\n"
        "      !\n"
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
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
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
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = m4_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, f3_proxy%data, f4_proxy%data, "
        "m3_proxy%data, m4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, map_w2h(:,cell), "
        "ndf_w2v, undf_w2v, map_w2v(:,cell), ndf_any_w2, undf_any_w2, "
        "map_any_w2(:,cell))\n"
        "      END DO \n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_type\n"
        "  END MODULE single_invoke_fs_psy")
    print str(generated_code)
    print output
    assert str(generated_code).find(output) != -1


def test_real_scalar():
    ''' tests that we generate correct code when a kernel takes a single,
    real scalar argument (plus fields)'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
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
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data,"
        " m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code


def test_int_scalar():
    ''' tests that we generate correct code when a kernel takes a single,
    integer scalar argument (plus fields) '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(f1, iflag, f2, m1, m2)\n"
        "      USE testkern_one_int_scalar, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      INTEGER, intent(in) :: iflag\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
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
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, iflag, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n")
    assert expected in generated_code


def test_two_real_scalars():
    ''' tests that we generate correct code when a kernel has two real,
    scalar arguments '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2, b)\n"
        "      USE testkern_two_real_scalars, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a, b\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
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
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, b, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code


def test_two_int_scalars():
    ''' tests that we generate correct code when a kernel has two integer,
    scalar arguments '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.6_single_invoke_2_int_scalars.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0(iflag, f1, f2, m1, m2, istep)\n"
        "      USE testkern_two_int_scalars, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      INTEGER, intent(in) :: iflag, istep\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
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
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, iflag, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, istep, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code
    # Check that we pass iflag by value in the second kernel call
    expected = (
        "        CALL testkern_code(nlayers, 1_i_def, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, istep, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
        "undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code


def test_two_scalars():
    ''' tests that we generate correct code when a kernel has two scalar
    arguments, one real and one integer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2, istep)\n"
        "      USE testkern_two_scalars, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      INTEGER, intent(in) :: istep\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
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
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
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
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data,"
        " m1_proxy%data, m2_proxy%data, istep, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n")
    assert expected in generated_code


def test_no_vector_scalar():
    ''' Tests that we raise an error when kernel meta-data erroneously
    specifies a vector scalar '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_real, gh_read)",
                        "arg_type(gh_real*3, gh_read)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'vector notation is not supported for scalar arguments' in \
        str(excinfo.value)


def test_vector_field():
    ''' tests that a vector field is declared correctly in the PSy
    layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print str(generated_code)
    assert str(generated_code).find("SUBROUTINE invoke_0_testkern_chi_"
                                    "type(f1, chi, f2)") != -1
    assert str(generated_code).find("TYPE(field_type), intent(inout)"
                                    " :: f1, chi(3)") != -1
    assert "TYPE(field_type), intent(in) :: f2" in str(generated_code)


def test_vector_field_2():
    ''' Tests that a vector field is indexed correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print generated_code
    # all references to chi_proxy should be chi_proxy(1)
    assert str(generated_code).find("chi_proxy%") == -1
    assert str(generated_code).count("chi_proxy(1)%vspace") == 4
    # use each chi field individually in the kernel
    assert str(generated_code).find("chi_proxy(1)%data, chi_proxy(2)%data,"
                                    " chi_proxy(3)%data") != -1


def test_vector_field_deref():
    ''' tests that a vector field is declared correctly in the PSy
    layer when it is obtained by de-referencing a derived type in the
    Algorithm layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "8.1_vector_field_deref.f90"),
                           api="dynamo0.3")
    for dist_mem in [True, False]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = psy.gen
        assert str(generated_code).find("SUBROUTINE invoke_0_testkern_chi_"
                                        "type(f1, box_chi, f2)") != -1
        assert str(generated_code).find("TYPE(field_type), intent(inout)"
                                        " :: f1, box_chi(3)") != -1
        assert "TYPE(field_type), intent(in) :: f2" in str(generated_code)


def test_orientation():
    ''' tests that orientation information is created correctly in
    the PSy '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "9_orientation.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print str(generated_code)
    assert str(generated_code).find("INTEGER, pointer :: orientation_w2(:)"
                                    " => null()") != -1
    assert str(generated_code).find("orientation_w2 => f2_proxy%vspace%"
                                    "get_cell_orientation(cell)") != -1


def test_operator():
    ''' tests that an operator is implemented correctly in the PSy
    layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert generated_code.find("SUBROUTINE invoke_0_testkern_operator"
                               "_type(mm_w0, chi, a, qr)") != -1
    assert generated_code.find("TYPE(operator_type), intent(inout) ::"
                               " mm_w0") != -1
    assert generated_code.find("TYPE(operator_proxy_type) mm_w0_"
                               "proxy") != -1
    assert generated_code.find("mm_w0_proxy = mm_w0%get_proxy()") != -1
    assert ("CALL testkern_operator_code(cell, nlayers, mm_w0_proxy%ncell_3d, "
            "mm_w0_proxy%local_stencil, chi_proxy(1)%data, chi_proxy(2)%data, "
            "chi_proxy(3)%data, a, ndf_w0, undf_w0, map_w0(:,cell), "
            "basis_w0_qr, diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, "
            "weights_z_qr)") in generated_code


def test_operator_different_spaces(tmpdir, f90, f90flags):
    '''tests that an operator with different to and from spaces is
    implemented correctly in the PSy layer'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.3_operator_different_spaces.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code

    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    decl_output = (
        "    SUBROUTINE invoke_0_assemble_weak_derivative_w3_w2_kernel_type"
        "(mapping, chi, qr)\n"
        "      USE assemble_weak_derivative_w3_w2_kernel_mod, ONLY: "
        "assemble_weak_derivative_w3_w2_kernel_code\n"
        "      USE quadrature_xyoz_mod, ONLY: quadrature_xyoz_type, "
        "quadrature_xyoz_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(in) :: chi(3)\n"
        "      TYPE(operator_type), intent(inout) :: mapping\n"
        "      TYPE(quadrature_xyoz_type), intent(in) :: qr\n"
        "      INTEGER, pointer :: orientation_w2(:) => null()\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w3_qr(:,:,:,:), "
        "diff_basis_w0_qr(:,:,:,:), diff_basis_w2_qr(:,:,:,:)\n"
        "      INTEGER dim_w3, diff_dim_w0, diff_dim_w2\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr(:) => null(), "
        "weights_z_qr(:) => null()\n"
        "      INTEGER np_xy_qr, np_z_qr\n"
        "      INTEGER ndf_w3, ndf_w2, ndf_w0, undf_w0\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(operator_proxy_type) mapping_proxy\n"
        "      TYPE(field_proxy_type) chi_proxy(3)\n"
        "      TYPE(quadrature_xyoz_proxy_type) qr_proxy\n"
        "      INTEGER, pointer :: map_w0(:,:) => null()\n")
    assert decl_output in generated_code
    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      mapping_proxy = mapping%get_proxy()\n"
        "      chi_proxy(1) = chi(1)%get_proxy()\n"
        "      chi_proxy(2) = chi(2)%get_proxy()\n"
        "      chi_proxy(3) = chi(3)%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = mapping_proxy%fs_from%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => mapping%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w0 => chi_proxy(1)%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = mapping_proxy%fs_to%get_ndf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = mapping_proxy%fs_from%get_ndf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = chi_proxy(1)%vspace%get_ndf()\n"
        "      undf_w0 = chi_proxy(1)%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n"
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w3 = mapping_proxy%fs_to%get_dim_space()\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w0 = chi_proxy(1)%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w0_qr(diff_dim_w0, ndf_w0, np_xy_qr, "
        "np_z_qr))\n"
        "      diff_dim_w2 = mapping_proxy%fs_from%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(BASIS, mapping_proxy%fs_to, "
        "dim_w3, ndf_w3, basis_w3_qr)\n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(DIFF_BASIS, chi_proxy(1)%vspace, "
        "diff_dim_w0, ndf_w0, diff_basis_w0_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, mapping_proxy%fs_from, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (chi_proxy(1)%is_dirty(depth=1)) THEN\n"
        "        CALL chi_proxy(1)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (chi_proxy(2)%is_dirty(depth=1)) THEN\n"
        "        CALL chi_proxy(2)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (chi_proxy(3)%is_dirty(depth=1)) THEN\n"
        "        CALL chi_proxy(3)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        orientation_w2 => mapping_proxy%fs_from%get_cell_orientation("
        "cell)\n"
        "        !\n"
        "        CALL assemble_weak_derivative_w3_w2_kernel_code(cell, "
        "nlayers, mapping_proxy%ncell_3d, mapping_proxy%local_stencil, "
        "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, ndf_w3, "
        "basis_w3_qr, ndf_w2, diff_basis_w2_qr, orientation_w2, "
        "ndf_w0, undf_w0, map_w0(:,cell), diff_basis_w0_qr, "
        "np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO \n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (diff_basis_w2_qr, diff_basis_w0_qr, basis_w3_qr)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_assemble_weak_derivative_w3_w2_kernel_"
        "type")
    assert output in generated_code


def test_operator_nofield(tmpdir, f90, f90flags):
    ''' tests that an operator with no field on the same space is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_code_str = str(psy.gen)
    print gen_code_str

    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    assert gen_code_str.find("SUBROUTINE invoke_0_testkern_operator_"
                             "nofield_type(mm_w2, chi, qr)") != -1
    assert gen_code_str.find("TYPE(operator_type), intent(inout) :: "
                             "mm_w2") != -1
    assert gen_code_str.find("TYPE(operator_proxy_type) mm_w2_proxy") != -1
    assert gen_code_str.find("mm_w2_proxy = mm_w2%get_proxy()") != -1
    assert gen_code_str.find("undf_w2") == -1
    assert gen_code_str.find("map_w2") == -1
    assert ("CALL testkern_operator_nofield_code(cell, nlayers, "
            "mm_w2_proxy%ncell_3d, mm_w2_proxy%local_stencil, "
            "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, "
            "ndf_w2, basis_w2_qr, ndf_w0, undf_w0, "
            "map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen_code_str)


def test_operator_nofield_different_space(  # pylint: disable=invalid-name
        tmpdir, f90, f90flags):
    ''' tests that an operator with no field on different spaces is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.5_operator_no_field_different_"
                                        "space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen

    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    assert "mesh => my_mapping%get_mesh()" in gen
    assert "nlayers = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w3 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_to%get_ndf()" in gen
    # We compute operators redundantly (out to the L1 halo)
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
            "local_stencil, ndf_w2, ndf_w3)" in gen)


def test_operator_nofield_scalar():
    ''' tests that an operator with no field and a
    scalar argument is implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.6_operator_no_field_scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert "mesh => my_mapping%get_mesh()" in gen
    assert "nlayers = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
            "local_stencil, b, ndf_w2, basis_w2_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen)


def test_operator_nofield_scalar_deref(  # pylint: disable=invalid-name
        tmpdir, f90, f90flags):
    ''' Tests that an operator with no field and a
    scalar argument is implemented correctly in the PSy layer when both
    are obtained by dereferencing derived type objects '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "10.6.1_operator_no_field_scalar_deref.f90"),
        api="dynamo0.3")
    for dist_mem in [True, False]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        gen = str(psy.gen)
        print gen

        if utils.TEST_COMPILE:
            assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        if dist_mem:
            assert "mesh => opbox_my_mapping%get_mesh()" in gen
        assert "nlayers = opbox_my_mapping_proxy%fs_from%get_nlayers()" in gen
        assert "ndf_w2 = opbox_my_mapping_proxy%fs_from%get_ndf()" in gen
        assert ("qr_get_instance%compute_function(BASIS, "
                "opbox_my_mapping_proxy%fs_from, "
                "dim_w2, ndf_w2, basis_w2_qr_get_instance)" in gen)
        if dist_mem:
            assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
        else:
            assert (
                "DO cell=1,opbox_my_mapping_proxy%fs_from%get_ncell()" in gen)
        assert (
            "(cell, nlayers, opbox_my_mapping_proxy%ncell_3d, "
            "opbox_my_mapping_proxy%local_stencil, box_b, ndf_w2, "
            "basis_w2_qr_get_instance, np_xy_qr_get_instance, "
            "np_z_qr_get_instance, weights_xy_qr_get_instance,"
            " weights_z_qr_get_instance)" in gen)


def test_operator_orientation(tmpdir, f90, f90flags):
    ''' tests that an operator requiring orientation information is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.2_operator_orient.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_str = str(psy.gen)
    print gen_str

    if utils.TEST_COMPILE:
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    assert gen_str.find("SUBROUTINE invoke_0_testkern_operator"
                        "_orient_type(mm_w1, chi, qr)") != -1
    assert gen_str.find("TYPE(operator_type), intent(inout) ::"
                        " mm_w1") != -1
    assert gen_str.find("TYPE(operator_proxy_type) mm_w1_"
                        "proxy") != -1
    assert gen_str.find("mm_w1_proxy = mm_w1%get_proxy()") != -1
    assert gen_str.find(
        "orientation_w1 => mm_w1_proxy%fs_from%get_cell_orientation"
        "(cell)") != -1
    assert ("CALL testkern_operator_orient_code(cell, nlayers, "
            "mm_w1_proxy%ncell_3d, mm_w1_proxy%local_stencil, "
            "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, ndf_w1, "
            "basis_w1_qr, orientation_w1, ndf_w0, undf_w0, map_w0(:,cell), "
            "diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, "
            "weights_z_qr)" in gen_str)


def test_op_orient_different_space(  # pylint: disable=invalid-name
        tmpdir, f90, f90flags):
    '''tests that an operator on different spaces requiring orientation
    information is implemented correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.4_operator_orient_different_"
                                        "space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_str = str(psy.gen)
    print gen_str

    if utils.TEST_COMPILE:
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    assert (
        "INTEGER, pointer :: orientation_w1(:) => null(), orientation_w2(:)"
        " => null()" in gen_str)
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen_str
    assert "ndf_w1 = my_mapping_proxy%fs_to%get_ndf()" in gen_str
    assert "dim_w1 = my_mapping_proxy%fs_to%get_dim_space()" in gen_str
    assert ("CALL qr%compute_function(BASIS, my_mapping_proxy%fs_to, "
            "dim_w1, ndf_w1, basis_w1_qr)" in gen_str)
    assert (
        "orientation_w2 => my_mapping_proxy%fs_from%get_cell_orientation("
        "cell)" in gen_str)
    assert (
        "orientation_w1 => my_mapping_proxy%fs_to%get_cell_orientation(cell)"
        in gen_str)
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, "
            "my_mapping_proxy%local_stencil, chi_proxy(1)%data, "
            "chi_proxy(2)%data, chi_proxy(3)%data, ndf_w1, basis_w1_qr, "
            "orientation_w1, ndf_w2, orientation_w2, ndf_w0, undf_w0, "
            "map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen_str)


def test_operator_deref(tmpdir, f90, f90flags):
    ''' Tests that we generate correct names for an operator in the PSy
    layer when obtained by de-referencing a derived type in the Algorithm
    layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10.8_operator_deref.f90"),
                           api="dynamo0.3")
    for dist_mem in [True, False]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        if utils.TEST_COMPILE:
            assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        assert generated_code.find("SUBROUTINE invoke_0_testkern_operator"
                                   "_type(mm_w0_op, chi, a, qr)") != -1
        assert generated_code.find("TYPE(operator_type), intent(inout) ::"
                                   " mm_w0_op") != -1
        assert generated_code.find("TYPE(operator_proxy_type) mm_w0_op_"
                                   "proxy") != -1
        assert (
            generated_code.find("mm_w0_op_proxy = mm_w0_op%get_proxy()") != -1)
        assert generated_code.find(
            "CALL testkern_operator_code(cell, nlayers, "
            "mm_w0_op_proxy%ncell_3d, mm_w0_op_proxy%local_stencil, "
            "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, a, "
            "ndf_w0, undf_w0, map_w0(:,cell), basis_w0_qr, "
            "diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, "
            "weights_z_qr)") != -1


def test_operator_no_dofmap_lookup():
    ''' Check that we use a field rather than an operator to look-up
    a dofmap, even when the operator precedes the field in the argument
    list. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.9_operator_first.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    # Check that we use the field and not the operator to look-up the dofmap
    assert "theta_proxy%vspace%get_whole_dofmap()" in gen_code
    assert gen_code.count("get_whole_dofmap") == 1


def test_operator_read_level1_halo():
    ''' Check that we raise an error if a kernel attempts to read from an
    operator beyond the level-1 halo '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.7_operator_read.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    # Modify the loop bound so that we attempt to read from the L2 halo
    # (of the operator)
    loop.set_upper_bound("cell_halo", index=2)
    # Attempt to generate the code
    with pytest.raises(GenerationError) as excinfo:
        _ = psy.gen
    assert ("Kernel 'testkern_operator_code' reads from an operator and "
            "therefore cannot be used for cells beyond the level 1 halo. "
            "However the containing loop goes out to level 2" in str(excinfo))


def test_any_space_1(tmpdir, f90, f90flags):
    ''' tests that any_space is implemented correctly in the PSy
    layer. Includes more than one type of any_space declaration
    and func_type basis functions on any_space. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    assert ("INTEGER, pointer :: map_w0(:,:) => null(), "
            "map_any_space_2_b(:,:) => null(), "
            "map_any_space_1_a(:,:) => null()\n" in generated_code)
    assert generated_code.find(
        "REAL(KIND=r_def), allocatable :: basis_any_space_1_a_qr(:,:,:,:), "
        "basis_any_space_2_b_qr(:,:,:,:)") != -1
    assert generated_code.find(
        "ALLOCATE (basis_any_space_1_a_qr(dim_any_space_1_a, "
        "ndf_any_space_1_a, np_xy_qr, np_z_qr))") != -1
    assert generated_code.find(
        "ALLOCATE (basis_any_space_2_b_qr(dim_any_space_2_b, "
        "ndf_any_space_2_b, np_xy_qr, np_z_qr))") != -1
    assert generated_code.find(
        "map_any_space_1_a => a_proxy%vspace%get_whole_dofmap()") != -1
    assert generated_code.find(
        "map_any_space_2_b => b_proxy%vspace%get_whole_dofmap()") != -1
    assert ("CALL testkern_any_space_1_code(nlayers, a_proxy%data, rdt, "
            "b_proxy%data, c_proxy(1)%data, c_proxy(2)%data, c_proxy(3)%data, "
            "ndf_any_space_1_a, undf_any_space_1_a, map_any_space_1_a(:,cell),"
            " basis_any_space_1_a_qr, ndf_any_space_2_b, undf_any_space_2_b, "
            "map_any_space_2_b(:,cell), basis_any_space_2_b_qr, ndf_w0, "
            "undf_w0, map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in generated_code)
    assert ("DEALLOCATE (basis_any_space_2_b_qr, diff_basis_w0_qr, "
            "basis_any_space_1_a_qr)" in generated_code)


def test_any_space_2():
    ''' tests that any_space is implemented correctly in the PSy
    layer. Includes multiple declarations of the same space, no
    func_type declarations and any_space used with an
    operator. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.1_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert "INTEGER, intent(in) :: istp" in generated_code
    assert generated_code.find(
        "INTEGER, pointer :: map_any_space_1_a(:,:) => null()") != -1
    assert generated_code.find(
        "INTEGER ndf_any_space_1_a, undf_any_space_1_a") != -1
    assert generated_code.find(
        "ndf_any_space_1_a = a_proxy%vspace%get_ndf()") != -1
    assert generated_code.find(
        "undf_any_space_1_a = a_proxy%vspace%get_undf()") != -1
    assert generated_code.find(
        "map_any_space_1_a => a_proxy%vspace%get_whole_dofmap()") != -1
    assert generated_code.find(
        "CALL testkern_any_space_2_code(cell, nlayers, a_proxy%data, b_pro"
        "xy%data, c_proxy%ncell_3d, c_proxy%local_stencil, istp, ndf_any_sp"
        "ace_1_a, undf_any_space_1_a, map_any_space_1_a(:,cell))") != -1


def test_op_any_space_different_space_1():  # pylint: disable=invalid-name
    ''' tests that any_space is implemented correctly in the PSy
    layer. Includes different spaces for an operator and no other
    fields.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.2_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert generated_code.find(
        "ndf_any_space_2_a = a_proxy%fs_from%get_ndf()") != -1
    assert generated_code.find(
        "ndf_any_space_1_a = a_proxy%fs_to%get_ndf()") != -1


def test_op_any_space_different_space_2(  # pylint: disable=invalid-name
        tmpdir, f90, f90flags):
    ''' tests that any_space is implemented correctly in the PSy
    layer in a more complicated example. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.3_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code

    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)
    assert "ndf_any_space_1_b = b_proxy%fs_to%get_ndf()" in generated_code
    assert "dim_any_space_1_b = b_proxy%fs_to%get_dim_space()" in \
        generated_code
    assert "ndf_any_space_2_b = b_proxy%fs_from%get_ndf()" in generated_code
    assert "ndf_any_space_3_c = c_proxy%fs_to%get_ndf()" in generated_code
    assert "ndf_any_space_4_d = d_proxy%fs_from%get_ndf()" in generated_code
    assert "undf_any_space_4_d = d_proxy%fs_from%get_undf()" in generated_code
    assert "dim_any_space_4_d = d_proxy%fs_from%get_dim_space()" in \
        generated_code
    assert "ndf_any_space_5_a = a_proxy%vspace%get_ndf()" in generated_code
    assert "undf_any_space_5_a = a_proxy%vspace%get_undf()" in generated_code
    assert "CALL qr%compute_function(BASIS, b_proxy%fs_to, " in generated_code
    assert "CALL qr%compute_function(BASIS, d_proxy%fs_from, " in \
        generated_code
    assert "CALL qr%compute_function(DIFF_BASIS, d_proxy%fs_from, " in \
        generated_code
    assert "map_any_space_5_a => a_proxy%vspace%get_whole_dofmap()" in \
        generated_code
    assert "map_any_space_4_d => f_proxy%vspace%get_whole_dofmap()" in \
        generated_code


def test_invoke_uniq_declns():
    ''' tests that we raise an error when Invoke.unique_declarations() is
    called for an invalid type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].unique_declarations("not_a_type")
    assert 'unique_declarations called with an invalid datatype' \
        in str(excinfo.value)


def test_invoke_uniq_declns_invalid_access():  # pylint: disable=invalid-name
    ''' tests that we raise an error when Invoke.unique_declarations() is
    called for an invalid access type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].unique_declarations("gh_field",
                                                       access="invalid_acc")
    assert 'unique_declarations called with an invalid access type' \
        in str(excinfo.value)


def test_invoke_uniq_proxy_declns():
    ''' tests that we raise an error when DynInvoke.unique_proxy_declarations()
    is called for an invalid type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].unique_proxy_declarations("not_a_type")
    assert 'unique_proxy_declarations called with an invalid datatype' \
        in str(excinfo.value)


def test_uniq_proxy_declns_invalid_access():  # pylint: disable=invalid-name
    ''' tests that we raise an error when DynInvoke.unique_proxy_declarations()
    is called for an invalid access type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].unique_proxy_declarations(
            "gh_field",
            access="invalid_acc")
    assert 'unique_proxy_declarations called with an invalid access type' \
        in str(excinfo.value)


def test_dyninvoke_first_access():
    ''' tests that we raise an error if DynInvoke.first_access(name) is
    called for an argument name that doesn't exist '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].first_access("not_an_arg")
    assert 'Failed to find any kernel argument with name' \
        in str(excinfo.value)


def test_dyninvoke_uniq_declns_inv_type():  # pylint: disable=invalid-name
    ''' tests that we raise an error when DynInvoke.unique_declns_by_intent()
    is called for an invalid argument type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].unique_declns_by_intent("gh_invalid")
    assert 'unique_declns_by_intent called with an invalid datatype' \
        in str(excinfo.value)


def test_dyninvoke_uniq_declns_intent_fields():  # pylint: disable=invalid-name
    ''' tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for gh_fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent("gh_field")
    assert args['inout'] == []
    assert args['out'] == ['f1']
    assert args['in'] == ['f2', 'm1', 'm2']


def test_dyninvoke_uniq_declns_intent_real():  # pylint: disable=invalid-name
    ''' tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for gh_real '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent("gh_real")
    assert args['inout'] == []
    assert args['out'] == []
    assert args['in'] == ['a']


def test_dyninvoke_uniq_declns_intent_int():  # pylint: disable=invalid-name
    ''' tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for gh_integer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent("gh_integer")
    assert args['inout'] == []
    assert args['out'] == []
    assert args['in'] == ['istep']


def test_dyninvoke_uniq_declns_intent_ops():  # pylint: disable=invalid-name
    ''' tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for operator arguments '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent("gh_operator")
    assert args['inout'] == []
    assert args['out'] == ['op']
    assert args['in'] == []


def test_dyninvoke_arg_for_fs():
    ''' tests that we raise an error when DynInvoke.arg_for_funcspace() is
    called for an un-used space '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].arg_for_funcspace(FunctionSpace("wtheta",
                                                                   None))
    assert "No argument found on 'wtheta' space" \
        in str(excinfo.value)


def test_kernel_specific():
    ''' Test that a call to enforce boundary conditions is *not* added
    following a call to the matrix_vector_kernel_type kernel. Boundary
    conditions are now explicity specified in the Algorithm as required. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "12_kernel_specific.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    output0 = "USE enforce_bc_kernel_mod, ONLY: enforce_bc_code"
    assert output0 not in generated_code
    output1 = "USE function_space_mod, ONLY: w1, w2, w2h, w2v\n"
    assert output1 not in generated_code
    output2 = "INTEGER fs"
    assert output2 not in generated_code
    output3 = "INTEGER, pointer :: boundary_dofs(:,:) => null()"
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
        "ndf_any_space_1_f1, undf_any_space_1_f1, map_any_space_1_f1(:,cell), "
        "boundary_dofs)")
    assert output6 not in generated_code


def test_multi_kernel_specific():
    '''Test that a call to enforce boundary conditions is *not* added following
    multiple calls to the matrix_vector_kernel_type kernel. Boundary conditions
    must now be explicitly specified as part of the Algorithm. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.3_multi_kernel_specific.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code

    # Output must not contain any bc-related code
    output0 = "USE enforce_bc_kernel_mod, ONLY: enforce_bc_code"
    assert generated_code.count(output0) == 0
    output1 = "USE function_space_mod, ONLY: w1, w2, w2h, w2v, any_w2\n"
    assert generated_code.count(output1) == 0

    # first loop
    output1 = "INTEGER fs\n"
    assert output1 not in generated_code
    output2 = "INTEGER, pointer :: boundary_dofs(:,:) => null()"
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
        "ndf_any_space_1_f1, undf_any_space_1_f1, map_any_space_1_f1(:,cell), "
        "boundary_dofs)")
    assert output5 not in generated_code

    # second loop
    output6 = "INTEGER fs_1\n"
    assert output6 not in generated_code
    output7 = "INTEGER, pointer :: boundary_dofs_1(:,:) => null()"
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
        "ndf_any_space_1_f1, undf_any_space_1_f1, map_any_space_1_f1(:,cell), "
        "boundary_dofs_1)")
    assert output10 not in generated_code


def test_field_bc_kernel():
    '''tests that a kernel with a particular name is recognised as a
    boundary condition kernel and that appopriate code is added to
    support this. This code is required as the dynamo0.3 api does not
    know about boundary conditions but this kernel requires them. This
    "hack" is only supported to get PSyclone to generate correct code
    for the current implementation of dynamo. Future API's will not
    support any hacks. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.2_enforce_bc_kernel.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output1 = "INTEGER, pointer :: boundary_dofs(:,:) => null()"
    assert str(generated_code).find(output1) != -1
    output2 = "boundary_dofs => a_proxy%vspace%get_boundary_dofs()"
    assert str(generated_code).find(output2) != -1
    output3 = (
        "CALL enforce_bc_code(nlayers, a_proxy%data, ndf_any_space_1_a, "
        "undf_any_space_1_a, map_any_space_1_a(:,cell), boundary_dofs)")
    assert str(generated_code).find(output3) != -1


def test_bc_kernel_field_only(monkeypatch):
    '''tests that the recognised boundary-condition kernel is rejected
    if it has an operator as argument instead of a field.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.2_enforce_bc_kernel.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        if dist_mem:
            idx = 1
        else:
            idx = 0
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[idx]
        call = loop.children[0]
        arg = call.arguments.args[0]
        # Monkeypatch the argument object so that it thinks it is an
        # operator rather than a field
        monkeypatch.setattr(arg, "_type", value="gh_operator")
        # We have to monkey-patch the arg.ref_name() function too as
        # otherwise the first monkey-patch causes it to break. Since
        # it is a function we have to patch it with a temporary
        # function which we create using lambda.
        monkeypatch.setattr(arg, "ref_name",
                            lambda function_space=None: "vspace")
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Expected a gh_field from which to look-up boundary dofs "
                "for kernel enforce_bc_code but got gh_operator"
                in str(excinfo))


def test_operator_bc_kernel():
    ''' Tests that a kernel with a particular name is recognised as a
    kernel that applies boundary conditions to operators and that
    appropriate code is added to support this. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    output1 = "INTEGER, pointer :: boundary_dofs(:,:) => null()"
    assert output1 in generated_code
    output2 = "boundary_dofs => op_a_proxy%fs_to%get_boundary_dofs()"
    assert output2 in generated_code
    output3 = (
        "CALL enforce_operator_bc_code(cell, nlayers, op_a_proxy%ncell_3d, "
        "op_a_proxy%local_stencil, ndf_any_space_1_op_a, "
        "ndf_any_space_2_op_a, boundary_dofs)")
    assert output3 in generated_code


def test_operator_bc_kernel_fld_err(monkeypatch):
    ''' test that we reject the recognised operator boundary conditions
    kernel if its argument is not an operator '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[0]
        call = loop.children[0]
        arg = call.arguments.args[0]
        # Monkeypatch the argument object so that it thinks it is a
        # field rather than an operator
        monkeypatch.setattr(arg, "_type", value="gh_field")
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Expected a LMA operator from which to look-up boundary dofs "
                "but kernel enforce_operator_bc_code has argument gh_field") \
            in str(excinfo)


def test_operator_bc_kernel_multi_args_err():  # pylint: disable=invalid-name
    ''' test that we reject the recognised operator boundary conditions
    kernel if it has more than one argument '''
    import copy
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[0]
        call = loop.children[0]
        arg = call.arguments.args[0]
        # Make the list of arguments invalid by duplicating (a copy of)
        # this argument. We take a copy because otherwise, when we change
        # the type of arg 1 below, we change it for both.
        call.arguments.args.append(copy.copy(arg))
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Kernel enforce_operator_bc_code has 2 arguments when it "
                "should only have 1 (an LMA operator)") in str(excinfo)
        # And again but make the second argument a field this time
        call.arguments.args[1]._type = "gh_field"
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Kernel enforce_operator_bc_code has 2 arguments when it "
                "should only have 1 (an LMA operator)") in str(excinfo)


def test_operator_bc_kernel_wrong_access_err():  # pylint: disable=invalid-name
    ''' test that we reject the recognised operator boundary conditions
    kernel if its operator argument has the wrong access type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[0]
        call = loop.children[0]
        arg = call.arguments.args[0]
        print dir(arg)
        print type(arg)
        arg._access = "gh_read"
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert (
            "applies boundary conditions to an operator. However its operator "
            "argument has access gh_read rather than gh_inc") in str(excinfo)


def test_multikernel_invoke_1():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke. We test the parts of the code that
    are incorrect at the time of writing '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    # check that argument names are not replicated
    output1 = "SUBROUTINE invoke_0(a, f1, f2, m1, m2)"
    assert generated_code.find(output1) != -1
    # check that only one proxy initialisation is produced
    output2 = "f1_proxy = f1%get_proxy()"
    assert generated_code.count(output2) == 1
    # check that we only initialise dofmaps once
    output3 = "map_w2 => f2_proxy%vspace%get_whole_dofmap()"
    assert generated_code.count(output3) == 1


def test_multikernel_invoke_qr():
    ''' Test that correct code is produced when there are multiple
    kernels with (the same) QR within an invoke. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.1_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # simple check that two kernel calls exist
    assert str(generated_code).count("CALL testkern_qr_code") == 2


def test_mkern_invoke_vec_fields():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with vector fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.2_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # 1st test for duplication of name vector-field declaration
    output1 = "TYPE(field_type), intent(inout) :: f1, chi(3), chi(3)"
    assert str(generated_code).find(output1) == -1
    # 2nd test for duplication of name vector-field declaration
    output2 = "TYPE(field_proxy_type) f1_proxy, chi_proxy(3), chi_proxy(3)"
    assert str(generated_code).find(output2) == -1


def test_multikern_invoke_orient():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with orientation '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.3_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # 1st test for duplication of name vector-field declaration
    output1 = "TYPE(field_type), intent(in) :: f2, f3(3), f3(3)"
    assert str(generated_code).find(output1) == -1
    # 2nd test for duplication of name vector-field declaration
    output2 = (
        "TYPE(field_proxy_type) f1_proxy, f2_proxy, f3_proxy(3), f3_proxy(3)")
    assert str(generated_code).find(output2) == -1


def test_multikern_invoke_oper():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with operators '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # 1st test for duplication of name vector-field declaration
    output1 = "TYPE(field_type), intent(in) :: f1(3), f1(3)"
    assert str(generated_code).find(output1) == -1
    # 2nd test for duplication of name vector-field declaration
    output2 = "TYPE(field_proxy_type) f1_proxy(3), f1_proxy(3)"
    assert str(generated_code).find(output2) == -1


def test_2kern_invoke_any_space():
    ''' Test correct code is generated when there are just two
    kernels within an invoke with kernel fields declared as
    any_space. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.1_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert ("INTEGER, pointer :: map_any_space_1_f1(:,:) => null(), "
            "map_any_space_1_f2(:,:) => null()\n"
            in gen)
    assert "map_any_space_1_f1 => f1_proxy%vspace%get_whole_dofmap()\n" in gen
    assert "map_any_space_1_f2 => f2_proxy%vspace%get_whole_dofmap()\n" in gen
    assert (
        "        CALL testkern_any_space_2_code(cell, nlayers, f1_proxy%data,"
        " f2_proxy%data, op_proxy%ncell_3d, op_proxy%local_stencil, scalar, "
        "ndf_any_space_1_f1, undf_any_space_1_f1, "
        "map_any_space_1_f1(:,cell))\n" in gen)
    assert "map_any_space_1_f2 => f2_proxy%vspace%get_whole_dofmap()\n"
    assert (
        "        CALL testkern_any_space_2_code(cell, nlayers, f2_proxy%data,"
        " f1_proxy%data, op_proxy%ncell_3d, op_proxy%local_stencil, scalar, "
        "ndf_any_space_1_f2, undf_any_space_1_f2, "
        "map_any_space_1_f2(:,cell))\n"
        in gen)


def test_multikern_invoke_any_space(tmpdir, f90, f90flags):
    ''' Test that we generate correct code when there are multiple
    kernels within an invoke with kernel fields declared as
    any_space.  '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)
    assert ("INTEGER, pointer :: map_any_space_1_f1(:,:) => null(), "
            "map_any_space_2_f1(:,:) => null(), "
            "map_any_space_2_f2(:,:) => null(), "
            "map_any_space_1_f2(:,:) => null(), map_w0(:,:) => null()" in gen)
    assert (
        "REAL(KIND=r_def), allocatable :: basis_any_space_1_f1_qr(:,:,:,:), "
        "basis_any_space_2_f2_qr(:,:,:,:), basis_any_space_1_f2_qr(:,:,:,:), "
        "basis_any_space_2_f1_qr(:,:,:,:), diff_basis_w0_qr(:,:,:,:)"in gen)
    assert "ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()" in gen
    assert "ndf_any_space_2_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert "ndf_w0 = f3_proxy(1)%vspace%get_ndf()" in gen
    assert "ndf_any_space_1_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert ("CALL qr%compute_function(BASIS, f2_proxy%vspace, "
            "dim_any_space_1_f2, ndf_any_space_1_f2, "
            "basis_any_space_1_f2_qr)" in gen)
    assert (
        "      map_any_space_1_f1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_space_2_f1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_space_2_f2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_space_1_f2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => f3_proxy(1)%vspace%get_whole_dofmap()" in gen)
    assert ("CALL testkern_any_space_1_code(nlayers, f1_proxy%data, rdt, "
            "f2_proxy%data, f3_proxy(1)%data, f3_proxy(2)%data, "
            "f3_proxy(3)%data, ndf_any_space_1_f1, undf_any_space_1_f1, "
            "map_any_space_1_f1(:,cell), basis_any_space_1_f1_qr, "
            "ndf_any_space_2_f2, undf_any_space_2_f2, "
            "map_any_space_2_f2(:,cell), basis_any_space_2_f2_qr, ndf_w0, "
            "undf_w0, map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr" in gen)


def test_mkern_invoke_multiple_any_spaces(  # pylint: disable=invalid-name
        tmpdir, f90, f90flags):
    ''' Test that we generate correct code when there are multiple
    kernels within an invoke with kernel fields declared as
    any_space.  '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.2_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)
    assert "ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()" in gen
    assert ("CALL qr%compute_function(BASIS, f1_proxy%vspace, "
            "dim_any_space_1_f1, ndf_any_space_1_f1, "
            "basis_any_space_1_f1_qr)" in gen)
    assert "ndf_any_space_2_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert ("CALL qr%compute_function(BASIS, f2_proxy%vspace, "
            "dim_any_space_2_f2, ndf_any_space_2_f2, "
            "basis_any_space_2_f2_qr)" in gen)
    assert "ndf_any_space_1_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert "ndf_any_space_1_op = op_proxy%fs_to%get_ndf()" in gen
    assert "ndf_any_space_5_f2 = f2_proxy%vspace%get_ndf()" in gen
    assert "ndf_any_space_1_op2 = op2_proxy%fs_to%get_ndf()" in gen
    assert "ndf_any_space_3_op3 = op3_proxy%fs_to%get_ndf()" in gen
    assert gen.count("ndf_any_space_4_op4 = op4_proxy%fs_from%get_ndf()") == 1
    assert "ndf_any_space_3_op5" not in gen
    assert "ndf_any_space_4_f1" not in gen
    # testkern_any_space_1_type requires GH_BASIS on ANY_SPACE_1 and 2 and
    # DIFF_BASIS on w0
    # f1 is on ANY_SPACE_1 and f2 is on ANY_SPACE_2. f3 is on W0.
    assert ("CALL qr%compute_function(BASIS, f1_proxy%vspace, "
            "dim_any_space_1_f1, ndf_any_space_1_f1, "
            "basis_any_space_1_f1_qr)" in gen)
    assert ("CALL qr%compute_function(BASIS, f2_proxy%vspace, "
            "dim_any_space_2_f2, ndf_any_space_2_f2, "
            "basis_any_space_2_f2_qr)" in gen)
    # testkern_any_space_4_type needs GH_BASIS on ANY_SPACE_1 which is the
    # to-space of op2
    assert ("CALL qr%compute_function(BASIS, op2_proxy%fs_to, "
            "dim_any_space_1_op2, ndf_any_space_1_op2, "
            "basis_any_space_1_op2_qr)" in gen)
    # Need GH_BASIS and DIFF_BASIS on ANY_SPACE_4 which is to/from-space
    # of op4
    assert ("CALL qr%compute_function(BASIS, op4_proxy%fs_from, "
            "dim_any_space_4_op4, ndf_any_space_4_op4, "
            "basis_any_space_4_op4_qr)" in gen)
    assert ("CALL qr%compute_function(DIFF_BASIS, op4_proxy%fs_from, "
            "diff_dim_any_space_4_op4, ndf_any_space_4_op4, "
            "diff_basis_any_space_4_op4_qr)" in gen)


@pytest.mark.xfail(reason="bug : loop fuse replicates maps in loops")
def test_loopfuse():
    ''' Tests whether loop fuse actually fuses and whether
    multiple maps are produced or not. Multiple maps are not an
    error but it would be nicer if there were only one '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
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
        if line.find("DO cell") != -1:
            do_idx = idx
        if line.find("CALL testkern_code(") != -1:
            kern_idxs.append(idx)
        if line.find("END DO") != -1:
            enddo_idx = idx
    # two kernel calls
    assert len(kern_idxs) == 2
    # both kernel calls are within the loop
    for kern_id in kern_idxs:
        assert kern_id > do_idx and kern_id < enddo_idx


def test_named_psy_routine():
    ''' Check that we generate a subroutine with the expected name
    if an invoke is named '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "1.0.1_single_named_invoke.f90"),
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        gen_code = str(psy.gen)
        # Name should be all lower-case and with spaces replaced by underscores
        assert "SUBROUTINE invoke_important_invoke" in gen_code

# tests for dynamo0.3 stub generator


def test_stub_non_existant_filename():
    ''' fail if the file does not exist '''
    with pytest.raises(IOError) as excinfo:
        generate("non_existant_file.f90", api="dynamo0.3")
    assert "file 'non_existant_file.f90' not found" in str(excinfo.value)


def test_stub_invalid_api():
    ''' fail if the specified api is not supported '''
    with pytest.raises(GenerationError) as excinfo:
        generate(os.path.join(BASE_PATH, "ru_kernel_mod.f90"), api="dynamo0.1")
    assert "Unsupported API 'dynamo0.1' specified" in str(excinfo.value)


def test_stub_file_content_not_fortran():  # pylint: disable=invalid-name
    ''' fail if the kernel file does not contain fortran '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "dynamo0p3_test.py"), api="dynamo0.3")
    assert 'the file does not contain a module. Is it a Kernel file?' \
        in str(excinfo.value)


def test_stub_file_fortran_invalid():
    ''' fail if the fortran in the kernel is not valid '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_invalid_fortran.F90"),
                 api="dynamo0.3")
    assert 'invalid Fortran' in str(excinfo.value)


def test_file_fortran_not_kernel():
    ''' fail if file is valid fortran but is not a kernel file '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                 api="dynamo0.3")
    assert 'file does not contain a module. Is it a Kernel file?' \
        in str(excinfo.value)


def test_module_name_too_short():
    ''' fail if length of kernel module name is too short '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_short_name.F90"),
                 api="dynamo0.3")
    assert "too short to have '_mod' as an extension" in str(excinfo.value)


def test_module_name_convention():
    ''' fail if kernel module name does not have _mod at end '''
    with pytest.raises(ParseError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern.F90"), api="dynamo0.3")
    assert "does not have '_mod' as an extension" in str(excinfo.value)


def test_kernel_datatype_not_found():
    ''' fail if kernel datatype is not found '''
    with pytest.raises(RuntimeError) as excinfo:
        generate(os.path.join(BASE_PATH, "testkern_no_datatype.F90"),
                 api="dynamo0.3")
    assert 'Kernel type testkern_type does not exist' in str(excinfo.value)

SIMPLE = (
    "  MODULE simple_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE simple_code(nlayers, field_1_w1, ndf_w1, undf_w1,"
    " map_w1)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w1) ::"
    " field_1_w1\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
    "    END SUBROUTINE simple_code\n"
    "  END MODULE simple_mod")


def test_stub_generate_working():
    ''' check that the stub generate produces the expected output '''
    result = generate(os.path.join(BASE_PATH, "simple.f90"),
                      api="dynamo0.3")
    print SIMPLE
    print result
    assert str(result).find(SIMPLE) != -1


def test_stub_generate_working_noapi():  # pylint: disable=invalid-name
    ''' check that the stub generate produces the expected output when
    we use the default api (which should be dynamo0.3)'''
    result = generate(os.path.join(BASE_PATH, "simple.f90"))
    print result
    assert str(result).find(SIMPLE) != -1

SIMPLE_WITH_SCALARS = (
    "  MODULE simple_with_scalars_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE simple_with_scalars_code(nlayers, rscalar_1, field_2_w1, "
    "iscalar_3, ndf_w1, undf_w1, map_w1)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      REAL(KIND=r_def), intent(in) :: rscalar_1\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w1) ::"
    " field_2_w1\n"
    "      INTEGER, intent(in) :: iscalar_3\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
    "    END SUBROUTINE simple_with_scalars_code\n"
    "  END MODULE simple_with_scalars_mod")


def test_stub_generate_with_scalars():
    ''' check that the stub generate produces the expected output when
    the kernel has scalar arguments '''
    result = generate(os.path.join(BASE_PATH, "simple_with_scalars.f90"),
                      api="dynamo0.3")
    print result
    assert str(result).find(SIMPLE_WITH_SCALARS) != -1


SCALAR_SUMS = (
    "  MODULE testkern_multiple_scalar_sums_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE testkern_multiple_scalar_sums_code(nlayers, rscalar_1, "
    "iscalar_2, field_3_w3, rscalar_4, iscalar_5, ndf_w3, undf_w3, map_w3)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      REAL(KIND=r_def), intent(inout) :: rscalar_1\n"
    "      INTEGER, intent(inout) :: iscalar_2\n"
    "      INTEGER, intent(in) :: ndf_w3\n"
    "      INTEGER, intent(in) :: undf_w3\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w3) :: field_3_w3\n"
    "      REAL(KIND=r_def), intent(inout) :: rscalar_4\n"
    "      INTEGER, intent(inout) :: iscalar_5\n"
    "      INTEGER, intent(in), dimension(ndf_w3) :: map_w3\n"
    "    END SUBROUTINE testkern_multiple_scalar_sums_code\n"
    "  END MODULE testkern_multiple_scalar_sums_mod")


def test_stub_generate_with_scalar_sums():  # pylint: disable=invalid-name
    '''check that the stub generator raises an exception when a kernel has
    a reduction (since these are not permitted for user-supplied kernels)'''
    with pytest.raises(ParseError) as err:
        _ = generate(
            os.path.join(BASE_PATH, "simple_with_reduction.f90"),
            api="dynamo0.3")
    assert (
        "user-supplied Dynamo 0.3 kernel must not write/update a scalar "
        "argument but kernel simple_with_reduction_type has gh_real with "
        "gh_sum access" in str(err))


# fields : intent
INTENT = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(3) =    &
          (/ arg_type(gh_field,gh_write,w1), &
             arg_type(gh_field,gh_inc, w1), &
             arg_type(gh_field,gh_read, w1)  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_load_meta_wrong_type():
    ''' Test that the load_meta function raises an appropriate error
    if the meta-data contains an un-recognised type '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    # Break the meta-data
    metadata.arg_descriptors[0]._type = "gh_hedge"
    with pytest.raises(GenerationError) as excinfo:
        kernel.load_meta(metadata)
    assert "load_meta expected one of '['gh_field'," in str(excinfo.value)


def test_intent():
    ''' test that field intent is generated correctly for kernel stubs '''
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w1, field_2_w1, "
        "field_3_w1, ndf_w1, undf_w1, map_w1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_1_w1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w1) :: "
        "field_3_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

# fields : spaces
SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(7) =               &
          (/ arg_type(gh_field,gh_write, w0),     &
             arg_type(gh_field,gh_write, w1),     &
             arg_type(gh_field,gh_write, w2),     &
             arg_type(gh_field,gh_write, w3),     &
             arg_type(gh_field,gh_write, wtheta), &
             arg_type(gh_field,gh_write, w2h),    &
             arg_type(gh_field,gh_write, w2v)     &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_spaces():
    ''' test that field spaces are handled correctly for kernel stubs '''
    ast = fpapi.parse(SPACES, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w0, field_2_w1, "
        "field_3_w2, field_4_w3, field_5_wtheta, field_6_w2h, field_7_w2v, "
        "ndf_w0, undf_w0, map_w0, ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, "
        "map_w2, ndf_w3, undf_w3, map_w3, ndf_wtheta, undf_wtheta, "
        "map_wtheta, ndf_w2h, undf_w2h, map_w2h, ndf_w2v, undf_w2v, "
        "map_w2v)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in) :: undf_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      INTEGER, intent(in) :: undf_w3\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in) :: undf_wtheta\n"
        "      INTEGER, intent(in) :: ndf_w2h\n"
        "      INTEGER, intent(in) :: undf_w2h\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
        "      INTEGER, intent(in) :: undf_w2v\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w3) :: "
        "field_4_w3\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_wtheta) :: "
        "field_5_wtheta\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2h) :: "
        "field_6_w2h\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2v) :: "
        "field_7_w2v\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER, intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      INTEGER, intent(in), dimension(ndf_w2h) :: map_w2h\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

# fields : vectors
VECTORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field*3,gh_write, w0) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_vectors():
    ''' test that field vectors are handled correctly for kernel stubs '''
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, ndf_w0, undf_w0, map_w0)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v3\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1


def test_arg_descriptor_vec_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected when we have a vector quantity '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w0'")
    print result
    assert expected_output in result


# operators : spaces and intent
OPERATORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(5) =    &
          (/ arg_type(gh_operator,gh_write, w0, w0), &
             arg_type(gh_operator,gh_inc,   w1, w1), &
             arg_type(gh_operator,gh_read,  w2, w2), &
             arg_type(gh_operator,gh_write, w3, w3), &
             arg_type(gh_operator,gh_read, any_space_1, any_space_1)  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_operators():
    ''' test that operators are handled correctly for kernel stubs '''
    ast = fpapi.parse(OPERATORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, op_1_ncell_3d, op_1, "
        "op_2_ncell_3d, op_2, op_3_ncell_3d, op_3, op_4_ncell_3d, op_4, "
        "op_5_ncell_3d, op_5, ndf_w0, ndf_w1, ndf_w2, ndf_w3, "
        "ndf_any_space_1_op_5)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_op_5\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w0,ndf_w0,"
        "op_1_ncell_3d) :: op_1\n"
        "      INTEGER, intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      INTEGER, intent(in) :: op_3_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2,ndf_w2,"
        "op_3_ncell_3d) :: op_3\n"
        "      INTEGER, intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      INTEGER, intent(in) :: op_5_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_op_5,"
        "ndf_any_space_1_op_5,op_5_ncell_3d) :: op_5\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1


def test_arg_descriptor_op_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected when we have an operator '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(OPERATORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_operator'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space_to[2]='w0'\n"
        "  function_space_from[3]='w0'\n")
    print result
    assert expected_output in result


OPERATOR_DIFFERENT_SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_operator,gh_write, w0, w1) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_stub_operator_different_spaces():  # pylint: disable=invalid-name
    ''' test that the correct function spaces are provided in the
    correct order when generating a kernel stub with an operator on
    different spaces '''
    ast = fpapi.parse(OPERATOR_DIFFERENT_SPACES, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    result = str(kernel.gen_stub)
    assert "(cell, nlayers, op_1_ncell_3d, op_1, ndf_w0, ndf_w1)" in result
    assert "dimension(ndf_w0,ndf_w1,op_1_ncell_3d)" in result

# orientation : spaces

ORIENTATION_OUTPUT = (
    "    SUBROUTINE dummy_orientation_code(cell, nlayers, field_1_w0, "
    "op_2_ncell_3d, op_2, field_3_w2, op_4_ncell_3d, op_4, ndf_w0, "
    "undf_w0, map_w0, orientation_w0, ndf_w1, orientation_w1, ndf_w2, "
    "undf_w2, map_w2, orientation_w2, ndf_w3, orientation_w3)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: cell\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      INTEGER, intent(in) :: ndf_w0\n"
    "      INTEGER, intent(in) :: undf_w0\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in) :: ndf_w2\n"
    "      INTEGER, intent(in) :: undf_w2\n"
    "      INTEGER, intent(in) :: ndf_w3\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
    "field_1_w0\n"
    "      INTEGER, intent(in) :: op_2_ncell_3d\n"
    "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
    "op_2_ncell_3d) :: op_2\n"
    "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
    "field_3_w2\n"
    "      INTEGER, intent(in) :: op_4_ncell_3d\n"
    "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
    "op_4_ncell_3d) :: op_4\n"
    "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
    "      INTEGER, intent(in), dimension(ndf_w0) :: orientation_w0\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: orientation_w1\n"
    "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
    "      INTEGER, intent(in), dimension(ndf_w2) :: orientation_w2\n"
    "      INTEGER, intent(in), dimension(ndf_w3) :: orientation_w3\n"
    "    END SUBROUTINE dummy_orientation_code\n"
    "  END MODULE dummy_orientation_mod")


def test_orientation_stubs():
    ''' Test that orientation is handled correctly for kernel
    stubs '''
    # Read-in the meta-data from file (it's in a file because it's also
    # used when testing the genkernelstub script from the command
    # line).
    with open(os.path.join(BASE_PATH, "dummy_orientation_mod.f90"),
              "r") as myfile:
        orientation = myfile.read()

    ast = fpapi.parse(orientation, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    print str(generated_code)
    assert str(generated_code).find(ORIENTATION_OUTPUT) != -1


def test_enforce_bc_kernel_stub_gen():  # pylint: disable=invalid-name
    ''' Test that the enforce_bc_kernel boundary layer argument modification
    is handled correctly for kernel stubs'''
    ast = fpapi.parse(os.path.join(BASE_PATH, "enforce_bc_kernel_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE enforce_bc_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE enforce_bc_code(nlayers, field_1_any_space_1_field_1, "
        "ndf_any_space_1_field_1, undf_any_space_1_field_1, "
        "map_any_space_1_field_1, boundary_dofs)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: undf_any_space_1_field_1\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_any_space_1_field_1)"
        " :: field_1_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1) :: "
        "map_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1,2) :: "
        "boundary_dofs\n"
        "    END SUBROUTINE enforce_bc_code\n"
        "  END MODULE enforce_bc_mod")
    print str(generated_code)
    assert str(generated_code).find(output) != -1


def test_enforce_op_bc_kernel_stub_gen():  # pylint: disable=invalid-name
    ''' Test that the enforce_operator_bc_kernel boundary dofs argument
    modification is handled correctly for kernel stubs'''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "enforce_operator_bc_kernel_mod.F90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE enforce_operator_bc_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE enforce_operator_bc_code(cell, nlayers, op_1_ncell_3d,"
        " op_1, ndf_any_space_1_op_1, ndf_any_space_2_op_1, boundary_dofs)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_op_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_op_1\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension("
        "ndf_any_space_1_op_1,ndf_any_space_2_op_1,op_1_ncell_3d) :: op_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_op_1,2) :: "
        "boundary_dofs\n"
        "    END SUBROUTINE enforce_operator_bc_code\n"
        "  END MODULE enforce_operator_bc_mod")
    print generated_code
    assert output in generated_code

# note, we do not need a separate test for qr as it is implicitly
# tested for in the above examples.
# fields : intent

SUB_NAME = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field,gh_write,w1) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy
  end type dummy_type
contains
  subroutine dummy()
  end subroutine dummy
end module dummy_mod
'''


def test_sub_name():
    ''' test for expected behaviour when the kernel subroutine does
    not conform to the convention of having "_code" at the end of its
    name. In this case we append "_code to the name and _mod to the
    kernel name.'''
    ast = fpapi.parse(SUB_NAME, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w1, "
        "ndf_w1, undf_w1, map_w1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_1_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1


def test_kernel_stub_usage():
    ''' Check that the kernel-stub generator prints a usage message
    if no arguments are supplied '''
    from subprocess import Popen, STDOUT, PIPE

    usage_msg = (
        "usage: genkernelstub [-h] [-o OUTFILE] [-api API] [-l] filename\n"
        "genkernelstub: error: too few arguments")

    # We use the Popen constructor here rather than check_output because
    # the latter is only available in Python 2.7 onwards.
    out = Popen(['genkernelstub'],
                stdout=PIPE,
                stderr=STDOUT).communicate()[0]
    assert usage_msg in out


def test_kernel_stub_gen_cmd_line():
    ''' Check that we can call the kernel-stub generator from the
    command line '''
    from subprocess import Popen, PIPE
    # We use the Popen constructor here rather than check_output because
    # the latter is only available in Python 2.7 onwards.
    out = Popen(["genkernelstub",
                 os.path.join(BASE_PATH, "dummy_orientation_mod.f90")],
                stdout=PIPE).communicate()[0]

    print "Output was: ", out
    assert ORIENTATION_OUTPUT in out


def test_stub_stencil_extent():
    ''' Check that correct stub code is produced when there is a stencil
    access '''
    ast = fpapi.parse(os.path.join(BASE_PATH, "testkern_stencil_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    print generated_code
    result1 = (
        "    SUBROUTINE testkern_stencil_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_map, field_3_w2, "
        "field_4_w3, ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, map_w2, "
        "ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = "INTEGER, intent(in) :: field_2_stencil_size"
    assert result2 in generated_code
    assert (
        "INTEGER, intent(in), dimension(ndf_w2,field_2_stencil_size) "
        ":: field_2_stencil_map" in generated_code)


def test_stub_stencil_direction():
    '''Check that correct stub code is produced when there is a stencil
    access which requires a direction argument '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_xory1d_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    print generated_code
    result1 = (
        "    SUBROUTINE testkern_stencil_xory1d_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_direction, "
        "field_2_stencil_map, field_3_w2, field_4_w3, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER, intent(in) :: field_2_stencil_size\n"
        "      INTEGER, intent(in) :: field_2_direction\n"
        "      INTEGER, intent(in), dimension(ndf_w2,field_2_stencil_size) :: "
        "field_2_stencil_map")
    assert result2 in generated_code


def test_stub_stencil_vector():
    '''Check that correct stub code is produced when there is a stencil
    access which is a vector '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_vector_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    print generated_code
    result1 = (
        "    SUBROUTINE testkern_stencil_vector_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, field_2_w3_v1, field_2_w3_v2, "
        "field_2_w3_v3, field_2_w3_v4, field_2_stencil_size, "
        "field_2_stencil_map, ndf_w0, undf_w0, map_w0, ndf_w3, undf_w3, "
        "map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER, intent(in) :: field_2_stencil_size\n"
        "      INTEGER, intent(in), dimension(ndf_w3,field_2_stencil_size) "
        ":: field_2_stencil_map")
    assert result2 in generated_code


def test_stub_stencil_multi():
    '''Check that correct stub code is produced when there are multiple
    stencils'''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_multi_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    print generated_code
    result1 = (
        "    SUBROUTINE testkern_stencil_multi_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_map, field_3_w2, "
        "field_3_stencil_size, field_3_direction, field_3_stencil_map, "
        "field_4_w3, field_4_stencil_size, field_4_stencil_map, ndf_w1, "
        "undf_w1, map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER, intent(in) :: field_2_stencil_size\n"
        "      INTEGER, intent(in), dimension(ndf_w2,field_2_stencil_size) :: "
        "field_2_stencil_map\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      INTEGER, intent(in) :: field_3_stencil_size\n"
        "      INTEGER, intent(in) :: field_3_direction\n"
        "      INTEGER, intent(in), dimension(ndf_w2,field_3_stencil_size) :: "
        "field_3_stencil_map\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: "
        "field_4_w3\n"
        "      INTEGER, intent(in) :: field_4_stencil_size\n"
        "      INTEGER, intent(in), dimension(ndf_w3,field_4_stencil_size) :: "
        "field_4_stencil_map")

    assert result2 in generated_code

STENCIL_CODE = '''
module stencil_mod
  type, extends(kernel_type) :: stencil_type
     type(arg_type), meta_args(2) =          &
          (/ arg_type(gh_field,gh_write,w1), &
             arg_type(gh_field,gh_read, w2, stencil(cross)) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => stencil_code
  end type stencil_type
contains
  subroutine stencil_code()
  end subroutine stencil_code
end module stencil_mod
'''


def test_stencil_metadata():
    ''' Check that we can parse Kernels with stencil metadata '''
    ast = fpapi.parse(STENCIL_CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    stencil_descriptor_0 = metadata.arg_descriptors[0]
    assert stencil_descriptor_0.stencil is None
    stencil_descriptor_1 = metadata.arg_descriptors[1]
    assert stencil_descriptor_1.stencil['type'] == 'cross'
    # stencil extent is not provided in the above metadata
    assert stencil_descriptor_1.stencil['extent'] is None


def test_field_metadata_too_many_arguments():  # pylint: disable=invalid-name
    '''Check that we raise an exception if more than 4 arguments are
    provided in the metadata for a gh_field arg_type.'''
    result = STENCIL_CODE.replace(
        "gh_field,gh_read, w2, stencil(cross)",
        "gh_field,gh_read, w2, stencil(cross), w1", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "each meta_arg entry must have at most 4 arguments" \
        in str(excinfo.value)


def test_invalid_stencil_form_1():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by being a literal integer or
    just "stencil" '''
    result = STENCIL_CODE.replace("stencil(cross)", "1", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
        in str(excinfo.value)
    assert "Unrecognised meta-data entry" in str(excinfo.value)
    result = STENCIL_CODE.replace("stencil(cross)", "stencil", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
        in str(excinfo.value)
    assert "Expecting format stencil(<type>[,<extent>]) but found stencil" \
        in str(excinfo.value)


def test_invalid_stencil_form_2():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by having an invalid name'''
    result = STENCIL_CODE.replace("stencil(cross)", "stenci(cross)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
        in str(excinfo.value)


def test_invalid_stencil_form_3():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by not having brackets'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
        in str(excinfo.value)


def test_invalid_stencil_form_4():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by containing no values in
    the brackets '''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil()", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "but found stencil()" in str(excinfo.value)


def test_invalid_stencil_form_5():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by containing no values in
    the brackets, with a separator '''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(,)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "kernel metadata has an invalid format" \
        in str(excinfo.value)


def test_invalid_stencil_form_6():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>[,<extent>]) format by containing more than two
    values in in the brackets '''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(cross,1,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be either a valid stencil specification" \
        in str(excinfo.value)
    assert "there must be at most two arguments inside the brackets" \
        in str(excinfo.value)


def test_invalid_stencil_first_arg_1():  # pylint: disable=invalid-name
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>[,<extent>]) is not valid and is an integer'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "not one of the valid types" in str(excinfo.value)
    assert "is a literal" in str(excinfo.value)


def test_invalid_stencil_first_arg_2():  # pylint: disable=invalid-name
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>[,<extent>]) is not valid and is a name'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(cros)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "not one of the valid types" in str(excinfo.value)


def test_invalid_stencil_first_arg_3():  # pylint: disable=invalid-name
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>[,<extent>]) is not valid and has brackets'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d(xx))", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "the specified <type>" in str(excinfo.value)
    assert "includes brackets" in str(excinfo.value)


def test_invalid_stencil_second_arg_1():  # pylint: disable=invalid-name
    '''Check that we raise an exception if the value of the stencil extent in
    stencil(<type>[,<extent>]) is not an integer'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,x1d)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "the specified <extent>" in str(excinfo.value)
    assert "is not an integer" in str(excinfo.value)


def test_invalid_stencil_second_arg_2():  # pylint: disable=invalid-name
    '''Check that we raise an exception if the value of the stencil extent in
    stencil(<type>[,<extent>]) is less than 1'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,0)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "the specified <extent>" in str(excinfo.value)
    assert "is less than 1" in str(excinfo.value)


def test_unsupported_second_argument():  # pylint: disable=invalid-name
    '''Check that we raise an exception if stencil extent is specified, as
    we do not currently support it'''
    result = STENCIL_CODE.replace("stencil(cross)", "stencil(x1d,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "Kernels with fixed stencil extents are not currently supported" \
        in str(excinfo.value)


def test_valid_stencil_types():
    ''' Check that we successfully parse all valid stencil types '''
    for stencil_type in VALID_STENCIL_TYPES:
        result = STENCIL_CODE.replace("stencil(cross)",
                                      "stencil("+stencil_type+")", 1)
        ast = fpapi.parse(result, ignore_comments=False)
        _ = DynKernMetadata(ast)


def test_arg_descriptor_funcs_method_error():  # pylint: disable=invalid-name
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when function_spaces is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._type = "gh_fire_starter"
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_spaces
    assert 'Internal error, DynArgDescriptor03:function_spaces(), should ' \
        'not get to here' in str(excinfo.value)


def test_DynKernelArgument_intent_invalid():  # pylint: disable=invalid-name
    '''Tests that an error is raised in DynKernelArgument when an invalid
    intent value is found. Tests with and without distributed memory '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        if dist_mem:
            idx = 3
        else:
            idx = 0
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        loop = schedule.children[idx]
        call = loop.children[0]
        arg = call.arguments.args[0]
        arg._access = "invalid"
        with pytest.raises(GenerationError) as excinfo:
            _ = arg.intent
        assert "Expecting argument access to be one of 'gh_read," in \
            str(excinfo.value)


def test_arg_ref_name_method_error1():
    ''' Tests that an internal error is raised in DynKernelArgument
    when ref_name() is called with a function space that is not
    associated with this field'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    first_argument = first_kernel.arguments.args[1]
    with pytest.raises(GenerationError) as excinfo:
        # the argument is a field and is on "w1"
        _ = first_argument.ref_name(FunctionSpace("w3", None))
    assert 'not one of the function spaces associated with this argument' \
        in str(excinfo.value)


def test_arg_ref_name_method_error2():
    ''' Tests that an internal error is raised in DynKernelArgument
    when ref_name() is called when the argument type is not one of
    gh_field or gh_operator'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    first_argument = first_kernel.arguments.args[1]
    first_argument._type = "gh_funky_instigator"
    with pytest.raises(GenerationError) as excinfo:
        _ = first_argument.ref_name()
    assert 'ref_name: Error, unsupported arg type' in str(excinfo)


def test_arg_intent_error():
    ''' Tests that an internal error is raised in DynKernelArgument
    when intent() is called and the argument access property is not one of
    gh_{read,write,inc} '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    first_argument = first_kernel.arguments.args[0]
    # Mess with the internal state of this argument object
    first_argument._access = "gh_not_an_intent"
    with pytest.raises(GenerationError) as excinfo:
        _ = first_argument.intent()
    assert ("Expecting argument access to be one of 'gh_read, gh_write, "
            "gh_inc' or one of ['gh_sum'], but found 'gh_not_an_intent'" in
            str(excinfo))


def test_no_arg_on_space(monkeypatch):
    ''' Tests that DynKernelArguments.get_arg_on_space[,_name] raise
    the appropriate error when there is no kernel argument on the
    supplied space. '''
    from psyclone.psyGen import FieldNotFoundError
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    kernel_args = first_kernel.arguments
    # Test getting the argument by the meta-data name for the function space
    arg = kernel_args.get_arg_on_space_name("w2")
    assert arg.name == "f2"
    with pytest.raises(FieldNotFoundError) as excinfo:
        _ = kernel_args.get_arg_on_space_name("not_a_space")
    assert ("there is no field or operator with function space not_a_space" in
            str(excinfo))
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
            "name = 'not_a_space_name')" in str(excinfo))


def test_arg_descriptor_func_method_error():  # pylint: disable=invalid-name
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when function_space is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._type = "gh_fire_starter"
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_space
    assert 'Internal error, DynArgDescriptor03:function_space(), should ' \
        'not get to here' in str(excinfo.value)


def test_arg_descriptor_fld_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected for a field argument'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[1]
    result = str(field_descriptor)
    print result
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w1'")
    assert expected_output in result


def test_arg_descriptor_scalar_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected for a scalar argument'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    print result
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_real'\n"
        "  access_descriptor[1]='gh_read'\n")
    assert expected_output in result


def test_arg_descriptor_str_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when __str__ is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._type = "gh_fire_starter"
    with pytest.raises(ParseError) as excinfo:
        _ = str(field_descriptor)
    assert 'Internal error in DynArgDescriptor03.__str__' \
        in str(excinfo.value)


def test_arg_descriptor_repr():
    ''' Tests that the repr method for DynArgDescriptor03 works as
    expected '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    result = repr(field_descriptor)
    print result
    assert 'DynArgDescriptor03(arg_type(gh_real, gh_read))' \
        in result


def test_arg_desc_func_space_tofrom_err():  # pylint: disable=invalid-name
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when function_space_to or function_space_from is called and the
    internal type is not gh_operator.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_space_to
    assert ("function_space_to only makes sense for one of ['gh_operator', "
            "'gh_columnwise_operator']") in str(excinfo.value)
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_space_from
    assert ("function_space_from only makes sense for one of ['gh_operator', "
            "'gh_columnwise_operator']") in str(excinfo.value)


def test_mangle_no_space_error():
    ''' Tests that an error is raised in mangle_fs_name()
    when none of the provided kernel arguments are on the
    specified space '''
    from psyclone.dynamo0p3 import mangle_fs_name
    from psyclone.psyGen import FieldNotFoundError
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.2_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    with pytest.raises(FieldNotFoundError) as excinfo:
        _ = mangle_fs_name(first_kernel.arguments.args, "any_space_7")
    assert "No kernel argument found for function space 'any_space_7'" \
        in str(excinfo.value)


def test_mangle_function_space():
    ''' Tests that we correctly mangle the function space name '''
    from psyclone.dynamo0p3 import mangle_fs_name
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5.2_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    name = mangle_fs_name(first_kernel.arguments.args, "any_space_2")
    assert name == "any_space_2_f2"


def test_no_mangle_specified_function_space():  # pylint: disable=invalid-name
    ''' Test that we do not name-mangle a function space that is not
    any_space '''
    from psyclone.dynamo0p3 import mangle_fs_name
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    name = mangle_fs_name(first_kernel.arguments.args, "w2")
    assert name == "w2"


def test_fsdescriptors_get_descriptor():  # pylint: disable=invalid-name
    ''' Test that FSDescriptors.get_descriptor() raises the expected error
    when passed a function space for which there is no corresponding kernel
    argument '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    fspace = FunctionSpace("w0", None)
    with pytest.raises(GenerationError) as excinfo:
        first_kernel.fs_descriptors.get_descriptor(fspace)
    assert "there is no descriptor for function space w0" in str(excinfo.value)


def test_arg_descriptor_init_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when an invalid type is provided. However this error never gets
    tripped due to an earlier test so we need to force the error by
    changing the internal state.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    # extract an arg_type object that we can use to create a
    # DynArgDescriptor03 object
    arg_type = field_descriptor._arg_type
    # Now try to trip the error by making the initial test think
    # that GH_INVALID is actually valid
    from psyclone.dynamo0p3 import VALID_ARG_TYPE_NAMES, DynArgDescriptor03
    keep = []
    keep.extend(VALID_ARG_TYPE_NAMES)
    VALID_ARG_TYPE_NAMES.append("GH_INVALID")
    arg_type.args[0].name = "GH_INVALID"
    with pytest.raises(ParseError) as excinfo:
        _ = DynArgDescriptor03(arg_type)
    assert 'Internal error in DynArgDescriptor03.__init__' \
        in str(excinfo.value)
    VALID_ARG_TYPE_NAMES = keep


def test_func_descriptor_repr():
    ''' Tests the __repr__ output of a func_descriptor '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    func_descriptor = metadata.func_descriptors[0]
    func_str = repr(func_descriptor)
    assert "DynFuncDescriptor03(func_type(w1, gh_basis))" in func_str


def test_func_descriptor_str():
    ''' Tests the __str__ output of a func_descriptor '''
    fparser.logging.disable('CRITICAL')
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
    passed an invalid function space '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    with pytest.raises(GenerationError) as err:
        _ = first_invoke.arg_for_funcspace(FunctionSpace("waah", "waah"))
    assert "No argument found on 'waah' space" in str(err)


def test_dist_memory_true():
    ''' test that the distributed memory flag is on by default '''
    import psyclone.config
    assert psyclone.config.DISTRIBUTED_MEMORY


def test_halo_dirty_1():
    ''' check halo_dirty call is added correctly with a simple example '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "     END DO \n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n")
    assert expected in generated_code


def test_halo_dirty_2():
    ''' check halo_dirty calls only for write and inc (not for read) '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.1_halo_writers.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "      END DO \n"
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


def test_halo_dirty_3():
    ''' check halo_dirty calls with multiple kernel calls '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print generated_code
    assert str(generated_code).count("CALL f1_proxy%set_dirty()") == 2


def test_halo_dirty_4():
    ''' check halo_dirty calls with field vectors '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "      END DO \n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL chi_proxy(1)%set_dirty()\n"
        "      CALL chi_proxy(2)%set_dirty()\n"
        "      CALL chi_proxy(3)%set_dirty()\n"
        "      CALL f1_proxy%set_dirty()\n")
    assert expected in generated_code


def test_halo_dirty_5():
    ''' check no halo_dirty calls for operators '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert "set_dirty()" not in generated_code
    assert "! Set halos dirty/clean" not in generated_code


def test_no_halo_dirty():
    '''check that no halo_dirty code is produced if distributed_memory is
    set to False'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert "set_dirty()" not in generated_code
    assert "! Set halos dirty/clean" not in generated_code


def test_halo_exchange():
    ''' test that a halo_exchange call is added for a loop with a
    stencil operation '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.2_halo_readers.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    output1 = (
        "     IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
        "      END IF \n"
        "      !\n")
    print output1
    assert output1 in generated_code
    output2 = ("      DO cell=1,mesh%get_last_halo_cell(1)\n")
    print output2
    assert output2 in generated_code


def test_halo_exchange_inc():
    '''test that appropriate halo exchange calls are added if we have a
    gh_inc operation and that the loop bounds included computation in
    the l1 halo '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    output1 = (
        "      IF (a_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL a_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (b_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL b_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (d_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL d_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (e_proxy(1)%is_dirty(depth=1)) THEN\n"
        "        CALL e_proxy(1)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (e_proxy(2)%is_dirty(depth=1)) THEN\n"
        "        CALL e_proxy(2)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (e_proxy(3)%is_dirty(depth=1)) THEN\n"
        "        CALL e_proxy(3)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    output2 = (
        "      IF (f_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert output1 in result
    assert output2 in result
    assert result.count("halo_exchange") == 7


def test_no_halo_exchange_for_operator():  # pylint: disable=invalid-name
    ''' Test that no halo exchange is generated before a kernel that reads
    from an operator '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.7_operator_read.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    # This kernel reads from an operator and a scalar and these
    # do not require halos to be updated.
    assert "halo_exchange" not in result


def test_no_set_dirty_for_operator():
    ''' Test that we do not call set_dirty for an operator that is written
    by a kernel. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.6_operator_no_field_scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    # This kernel only writes to an operator and since operators are
    # cell-local this does not require us to call the is_dirty() method.
    assert "is_dirty" not in result


def test_halo_exchange_different_spaces():  # pylint: disable=invalid-name
    '''test that all of our different function spaces with a stencil
    access result in halo calls including any_space'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.3_halo_readers_all_fs.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert result.count("halo_exchange") == 9


def test_halo_exchange_vectors_1():
    ''' test that halo exchange produces correct code for vector
    fields. Test a field with gh_inc '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4.1_halo_vector.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert result.count("halo_exchange(") == 3
    for idx in range(1, 4):
        assert "f1_proxy("+str(idx)+")%halo_exchange(depth=1)" in result
    expected = ("      IF (f1_proxy(3)%is_dirty(depth=1)) THEN\n"
                "        CALL f1_proxy(3)%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert expected in result


def test_halo_exchange_vectors():
    ''' test that halo exchange produces correct code for vector
    fields. Test both a field with a stencil and a field with gh_inc '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert result.count("halo_exchange(") == 7
    for idx in range(1, 4):
        assert "f1_proxy("+str(idx)+")%halo_exchange(depth=1)" in result
        assert "f2_proxy("+str(idx)+")%halo_exchange(depth=f2_extent+1)" \
            in result
    expected = ("      IF (f2_proxy(4)%is_dirty(depth=f2_extent+1)) THEN\n"
                "        CALL f2_proxy(4)%halo_exchange(depth=f2_extent+1)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert expected in result


def test_halo_exchange_depths():
    ''' test that halo exchange includes the correct halo
    depth with gh_write '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.5_halo_depth.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    expected = ("      IF (f2_proxy%is_dirty(depth=extent)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=extent)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=extent)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=extent)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f4_proxy%is_dirty(depth=extent)) THEN\n"
                "        CALL f4_proxy%halo_exchange(depth=extent)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_edge_cell()\n")
    assert expected in result


def test_halo_exchange_depths_gh_inc():  # pylint: disable=invalid-name
    ''' test that halo exchange includes the correct halo depth when
    we have a gh_inc as this increases the required depth by 1 (as
    redundant computation is performed in the l1 halo) '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.6_halo_depth_2.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    expected = ("      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f1_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=f3_extent+1)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=f3_extent+1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f4_proxy%is_dirty(depth=f4_extent+1)) THEN\n"
                "        CALL f4_proxy%halo_exchange(depth=f4_extent+1)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert expected in result


def test_stencil_read_only():
    '''test that an error is raised if a field with a stencil is not
    accessed as gh_read'''
    fparser.logging.disable('CRITICAL')
    code = STENCIL_CODE.replace("gh_read, w2, stencil(cross)",
                                "gh_write, w2, stencil(cross)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name="stencil_type")
    assert "a stencil must be read only" in str(excinfo.value)


def test_w3_and_inc_error():
    '''test that an error is raised if w3 and gh_inc are provided for the
    same field in the metadata '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_read, w3)",
                        "arg_type(gh_field,gh_inc, w3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name="testkern_qr_type")
    assert (
        "It does not make sense for a quantity on a discontinuous space "
        "(w3) to have a 'gh_inc' access" in str(excinfo.value))


def test_halo_exchange_view(capsys):
    ''' test that the halo exchange view method returns what we expect '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.2_halo_readers.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.get('invoke_0_testkern_stencil_type').schedule
    schedule.view()
    result, _ = capsys.readouterr()

    # Ensure we test for text containing the correct (colour) control codes
    sched = colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])
    loop = colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])
    call = colored("KernCall", SCHEDULE_COLOUR_MAP["KernCall"])
    exch = colored("HaloExchange", SCHEDULE_COLOUR_MAP["HaloExchange"])

    expected = (
        sched + "[invoke='invoke_0_testkern_stencil_type' dm=True]\n"
        "    " + exch + "[field='f2', type='region', depth=f2_extent+1, "
        "check_dirty=True]\n"
        "    " + exch + "[field='f3', type='region', depth=1, "
        "check_dirty=True]\n"
        "    " + exch + "[field='f4', type='region', depth=1, "
        "check_dirty=True]\n"
        "    " + loop + "[type='',field_space='w1',it_space='cells', "
        "upper_bound='cell_halo(1)']\n"
        "        " + call + " testkern_stencil_code(f1,f2,f3,f4) "
        "[module_inline=False]")
    print expected
    print result
    assert expected in result


def test_no_mesh_mod():
    '''test that we do not add a mesh module to the PSy layer if one is
    not required. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    result = str(psy.gen)
    print result
    assert "USE mesh_mod, ONLY: mesh_type" not in result
    assert "TYPE(mesh_type), pointer :: mesh => null()" not in result
    assert "mesh => a%get_mesh()" not in result


def test_mesh_mod():
    '''test that a mesh module is added to the PSy layer and a mesh object
    is created when required. One is required when we determine loop
    bounds for distributed memory '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert "USE mesh_mod, ONLY: mesh_type" in result
    assert "TYPE(mesh_type), pointer :: mesh => null()" in result
    output = ("      !\n"
              "      ! Create a mesh object\n"
              "      !\n"
              "      mesh => a%get_mesh()\n")
    assert output in result

# when we add build tests we should test that we can we get the mesh
# object from an operator


def test_set_lower_bound_functions():
    '''test that we raise appropriate exceptions when the lower bound of
    a loop is set to invalid values '''
    my_loop = DynLoop()
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_lower_bound("invalid_loop_bounds_name")
    assert "lower bound loop name is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_lower_bound("inner", index=0)
    assert "specified index" in str(excinfo.value)
    assert "lower loop bound is invalid" in str(excinfo.value)


def test_set_upper_bound_functions():
    '''test that we raise appropriate exceptions when the upper bound of
    a loop is set to invalid values '''
    my_loop = DynLoop()
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("invalid_loop_bounds_name")
    assert "upper loop bound name is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("start")
    assert "'start' is not a valid upper bound" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("inner", index=0)
    assert "specified index" in str(excinfo.value)
    assert "upper loop bound is invalid" in str(excinfo.value)


def test_lower_bound_fortran_1():
    '''tests we raise an exception in the DynLoop:_lower_bound_fortran()
    method - first GenerationError'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    my_loop.set_lower_bound("inner", index=1)
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop._lower_bound_fortran()
    assert ("lower bound must be 'start' if we are sequential" in
            str(excinfo.value))


def test_lower_bound_fortran_2(monkeypatch):
    '''tests we raise an exception in the DynLoop:_lower_bound_fortran()
    method - second GenerationError'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[3]
    # we can not use the standard set_lower_bound function as that
    # checks for valid input
    monkeypatch.setattr(my_loop, "_lower_bound_name", value="invalid")
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop._lower_bound_fortran()
    assert ("Unsupported lower bound name 'invalid' found" in
            str(excinfo.value))


def test_upper_bound_fortran_1():
    '''tests we raise an exception in the DynLoop:_upper_bound_fortran()
    method when 'cell_halo', 'dof_halo' or 'inner' are used'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    for option in ["cell_halo", "dof_halo", "inner"]:
        my_loop.set_upper_bound(option, index=1)
        with pytest.raises(GenerationError) as excinfo:
            _ = my_loop._upper_bound_fortran()
            assert (
                "'{0}' is not a valid loop upper bound for sequential/"
                "shared-memory code".format(option) in
                str(excinfo.value))


def test_upper_bound_fortran_2(monkeypatch):
    '''tests we raise an exception in the DynLoop:_upper_bound_fortran()
    method if an invalid value is provided'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="invalid")
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop._upper_bound_fortran()
    assert (
        "Unsupported upper bound name 'invalid' found" in str(excinfo.value))


def test_intent_multi_kern():
    ''' Test that we correctly generate argument declarations when the
    same fields are passed to different kernels with different intents '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        output = str(psy.gen)
        print output
        assert "TYPE(field_type), intent(inout) :: g, f\n" in output
        assert "TYPE(field_type), intent(inout) :: b, h\n" in output
        assert "TYPE(field_type), intent(in) :: c, d, a, e(3)\n" in output
        assert "TYPE(quadrature_xyoz_type), intent(in) :: qr\n" in output


def test_field_gh_sum_invalid():
    ''' Tests that an error is raised when a field is specified with
    access type gh_sum '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_read, w2)",
                        "arg_type(gh_field, gh_sum, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "reduction access 'gh_sum' is only valid with a scalar argument" \
        in str(excinfo.value)
    assert "but 'gh_field' was found" in str(excinfo.value)


def test_operator_gh_sum_invalid():
    ''' Tests that an error is raised when an operator is specified with
    access type gh_sum '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator, gh_sum, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "reduction access 'gh_sum' is only valid with a scalar argument" \
        in str(excinfo.value)
    assert "but 'gh_operator' was found" in str(excinfo.value)


def test_derived_type_arg():
    ''' Test that we generate a suitable name for a dummy variable
    in the PSy layer when its value in the algorithm layer is
    obtained from the component of a derived type or from a type-bound
    procedure call. '''
    for dist_mem in [True, False]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "1.6.2_single_invoke_1_int_from_derived_type.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        gen = str(psy.gen)
        print gen
        # Check the four integer variables are named and declared correctly
        expected = (
            "    SUBROUTINE invoke_0(f1, my_obj_iflag, f2, m1, m2, "
            "my_obj_get_flag, my_obj_get_flag_1, my_obj_get_flag_2)\n")
        assert expected in gen
        expected = (
            "      INTEGER, intent(in) :: my_obj_iflag, my_obj_get_flag, "
            "my_obj_get_flag_1, my_obj_get_flag_2\n")
        assert expected in gen
        # Check that they are still named correctly when passed to the
        # kernels
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, my_obj_iflag, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, my_obj_get_flag, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, my_obj_get_flag_1, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, my_obj_get_flag_2, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)


def test_multiple_derived_type_args():
    ''' Test that we generate correct code when kernel arguments are
    supplied from the algorithm layer as different components of the
    same derived type object '''
    for dist_mem in [True, False]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "1.6.3_single_invoke_multiple_derived_types.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        gen = str(psy.gen)
        print gen
        # Check the four integer variables are named and declared correctly
        expected = (
            "    SUBROUTINE invoke_0(f1, obj_a_iflag, f2, m1, m2, "
            "obj_b_iflag, obj_a_obj_b, obj_b_obj_a)\n")
        assert expected in gen
        expected = (
            "      INTEGER, intent(in) :: obj_a_iflag, obj_b_iflag, "
            "obj_a_obj_b, obj_b_obj_a\n")
        assert expected in gen
        # Check that they are still named correctly when passed to the
        # kernels
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, obj_a_iflag, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, obj_b_iflag, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, obj_a_obj_b, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)
        assert (
            "CALL testkern_code(nlayers, f1_proxy%data, obj_b_obj_a, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))" in gen)


def test_single_stencil_extent():
    '''test a single stencil access with an extent value passed from the
    algorithm layer is treated correctly in the PSy layer. Test both
    sequential and distributed memory '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "SUBROUTINE invoke_0_testkern_stencil_type(f1, f2, f3, f4, "
            "f2_extent)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = ("      INTEGER, intent(in) :: f2_extent\n")
        assert output3 in result
        output4 = (
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output4 in result
        output5 = (
            "      !\n"
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,f2_extent)\n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      !\n")
        assert output5 in result
        output6 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data,"
            " f2_proxy%data, f2_stencil_size, f2_stencil_dofmap(:,:,cell),"
            " f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output6 in result


def test_single_stencil_xory1d():
    '''test a single stencil access with an extent and direction value
    passed from the algorithm layer is treated correctly in the PSy
    layer. Test both sequential and distributed memory '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.3_single_stencil_xory1d.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0_testkern_stencil_xory1d_type(f1, f2, f3, "
            "f4, f2_extent, f2_direction)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
            "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = (
            "      INTEGER, intent(in) :: f2_extent\n"
            "      INTEGER, intent(in) :: f2_direction\n")
        assert output3 in result
        output4 = (
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output4 in result
        output5 = (
            "      !\n"
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      IF (f2_direction .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,f2_extent)\n"
            "      END IF \n"
            "      IF (f2_direction .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,f2_extent)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      !\n")
        assert output5 in result
        output6 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size, f2_direction, "
            "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output6 in result


def test_single_stencil_literal():
    '''test extent value is used correctly from the algorithm layer when
    it is a literal value so is not passed by argument'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.4_single_stencil_literal.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = ("    SUBROUTINE invoke_0_testkern_stencil_type(f1, f2, "
                   "f3, f4)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = (
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output3 in result
        output4 = (
            "      !\n"
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,1)\n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      !\n")
        assert output4 in result
        if dist_mem:
            output5 = (
                "      IF (f2_proxy%is_dirty(depth=2)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=2)\n"
                "      END IF \n")
            assert output5 in result
        output6 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size, f2_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output6 in result


def test_stencil_region_unsupported():
    '''Check that we raise an exception if the value of the stencil type
    in stencil(<type>[,<extent>]) is region. This is not a parse error
    as region is a valid value, it is just that the LFRic
    infrastructure does not yet support it. '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.12_single_stencil_region.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        with pytest.raises(GenerationError) as excinfo:
            _ = str(psy.gen)
        assert "Unsupported stencil type 'region' supplied" in \
            str(excinfo.value)


def test_single_stencil_xory1d_literal():  # pylint: disable=invalid-name
    '''test extent value is used correctly from the algorithm layer when
    it is a literal value so is not passed by argument'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.5_single_stencil_xory1d_literal.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = ("    SUBROUTINE invoke_0_testkern_stencil_xory1d_type("
                   "f1, f2, f3, f4)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
            "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = (
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output3 in result
        output4 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      IF (x_direction .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,2)\n"
            "      END IF \n"
            "      IF (x_direction .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,2)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      !\n")
        assert output4 in result
        if dist_mem:
            output5 = (
                "      IF (f2_proxy%is_dirty(depth=3)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=3)\n"
                "      END IF \n")
            assert output5 in result
        output6 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size, x_direction, "
            "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output6 in result


def test_single_stencil_xory1d_literal_mixed():  # pylint: disable=invalid-name
    '''test extent value is used correctly from the algorithm layer when
    it is a literal value so is not passed by argument and the case of the
    literal is specified in mixed case'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.5.1_single_stencil_xory1d_literal.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = ("    SUBROUTINE invoke_0_testkern_stencil_xory1d_type("
                   "f1, f2, f3, f4)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
            "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = (
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output3 in result
        output4 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      IF (x_direction .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,2)\n"
            "      END IF \n"
            "      IF (x_direction .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,2)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      !\n")
        assert output4 in result
        if dist_mem:
            output5 = (
                "      IF (f2_proxy%is_dirty(depth=3)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=3)\n"
                "      END IF \n")
            assert output5 in result
        output6 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size, x_direction, "
            "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output6 in result


def test_multiple_stencils():
    '''test for correct output when there is more than one stencil in a
    kernel'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.7_multiple_stencils.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0_testkern_stencil_multi_type(f1, f2, f3, "
            "f4, f2_extent, f3_extent, f3_direction)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
            "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
            "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = (
            "      INTEGER, intent(in) :: f2_extent, f3_extent\n"
            "      INTEGER, intent(in) :: f3_direction\n")
        assert output3 in result
        output4 = (
            "      INTEGER f4_stencil_size\n"
            "      INTEGER, pointer :: f4_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
            "null()\n"
            "      INTEGER f3_stencil_size\n"
            "      INTEGER, pointer :: f3_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
            "null()\n"
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output4 in result
        output5 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,f2_extent)\n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      IF (f3_direction .eq. x_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,f3_extent)\n"
            "      END IF \n"
            "      IF (f3_direction .eq. y_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,f3_extent)\n"
            "      END IF \n"
            "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
            "      f3_stencil_size = f3_stencil_map%get_size()\n"
            "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,1)\n"
            "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
            "      f4_stencil_size = f4_stencil_map%get_size()\n"
            "      !\n")
        assert output5 in result
        if dist_mem:
            output6 = (
                "      IF (f2_proxy%is_dirty(depth=f2_extent+1)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=f2_extent+1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=f3_extent+1)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=f3_extent+1)\n"
                "      END IF \n")
            assert output6 in result
        output7 = (
            "        CALL testkern_stencil_multi_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size, f2_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f3_stencil_size, f3_direction, "
            "f3_stencil_dofmap(:,:,cell), f4_proxy%data, f4_stencil_size, "
            "f4_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output7 in result


def test_multiple_stencil_same_name():
    '''test the case when there is more than one stencil in a kernel with
    the same name for extent'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.8_multiple_stencils_same_name.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0_testkern_stencil_multi_type(f1, f2, f3, "
            "f4, extent, f3_direction)")
        assert output1 in result
        output2 = (
            "      INTEGER, intent(in) :: extent\n"
            "      INTEGER, intent(in) :: f3_direction\n")
        assert output2 in result
        output3 = (
            "      INTEGER f4_stencil_size\n"
            "      INTEGER, pointer :: f4_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
            "null()\n"
            "      INTEGER f3_stencil_size\n"
            "      INTEGER, pointer :: f3_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
            "null()\n"
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output3 in result
        output4 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      IF (f3_direction .eq. x_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,extent)\n"
            "      END IF \n"
            "      IF (f3_direction .eq. y_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,extent)\n"
            "      END IF \n"
            "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
            "      f3_stencil_size = f3_stencil_map%get_size()\n"
            "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,extent)\n"
            "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
            "      f4_stencil_size = f4_stencil_map%get_size()\n"
            "      !\n")
        assert output4 in result
        output5 = (
            "        CALL testkern_stencil_multi_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size, f2_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f3_stencil_size, f3_direction, "
            "f3_stencil_dofmap(:,:,cell), f4_proxy%data, f4_stencil_size, "
            "f4_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output5 in result


def test_multi_stencil_same_name_direction():  # pylint: disable=invalid-name
    '''test the case where there is more than one stencil in a kernel with
    the same name for direction'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.9_multiple_stencils_same_name.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "SUBROUTINE invoke_0_testkern_stencil_multi_2_type(f1, f2, f3, "
            "f4, extent, direction)")
        assert output1 in result
        output2 = (
            "      INTEGER, intent(in) :: extent\n"
            "      INTEGER, intent(in) :: direction\n")
        assert output2 in result
        output3 = (
            "      INTEGER f4_stencil_size\n"
            "      INTEGER, pointer :: f4_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
            "null()\n"
            "      INTEGER f3_stencil_size\n"
            "      INTEGER, pointer :: f3_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
            "null()\n"
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
            "null()\n")
        assert output3 in result
        output4 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      IF (direction .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,extent)\n"
            "      END IF \n"
            "      IF (direction .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,extent)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      IF (direction .eq. x_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,extent)\n"
            "      END IF \n"
            "      IF (direction .eq. y_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,extent)\n"
            "      END IF \n"
            "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
            "      f3_stencil_size = f3_stencil_map%get_size()\n"
            "      IF (direction .eq. x_direction) THEN\n"
            "        f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,extent)\n"
            "      END IF \n"
            "      IF (direction .eq. y_direction) THEN\n"
            "        f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,extent)\n"
            "      END IF \n"
            "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
            "      f4_stencil_size = f4_stencil_map%get_size()\n"
            "      !\n")
        assert output4 in result
        output5 = (
            "     CALL testkern_stencil_multi_2_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size, direction, "
            "f2_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f3_stencil_size, direction, "
            "f3_stencil_dofmap(:,:,cell), "
            "f4_proxy%data, f4_stencil_size, direction, "
            "f4_stencil_dofmap(:,:,cell), "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output5 in result


def test_multi_kerns_stencils_diff_fields():  # pylint: disable=invalid-name
    '''Test the case where we have multiple kernels with stencils and
    different fields for each. We also test extent names by having both
    shared and individual names.'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.20_multiple_kernels_stencils.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0(f1, f2a, f3, f4, f2b, f2c, f2a_extent, "
            "extent)")
        assert output1 in result
        output2 = (
            "      USE testkern_stencil_mod, ONLY: testkern_stencil_code\n"
            "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n")
        assert output2 in result
        output3 = (
            "      INTEGER, intent(in) :: f2a_extent, extent\n")
        assert output3 in result
        output4 = (
            "      INTEGER f2b_stencil_size\n"
            "      INTEGER, pointer :: f2b_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2b_stencil_map "
            "=> null()\n"
            "      INTEGER f2a_stencil_size\n"
            "      INTEGER, pointer :: f2a_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2a_stencil_map "
            "=> null()\n")
        assert output4 in result
        output5 = (
            "      !\n"
            "      f2a_stencil_map => f2a_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,f2a_extent)\n"
            "      f2a_stencil_dofmap => f2a_stencil_map%get_whole_dofmap()\n"
            "      f2a_stencil_size = f2a_stencil_map%get_size()\n"
            "      f2b_stencil_map => f2b_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f2b_stencil_dofmap => f2b_stencil_map%get_whole_dofmap()\n"
            "      f2b_stencil_size = f2b_stencil_map%get_size()\n"
            "      !\n")
        assert output5 in result
        output6 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2a_proxy%data, f2a_stencil_size, f2a_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output6 in result
        output7 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2b_proxy%data, f2b_stencil_size, f2b_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output7 in result
        output8 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2c_proxy%data, f2b_stencil_size, f2b_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output8 in result


def test_extent_name_clash():
    '''Test we can deal with name clashes for stencils. We have a single
    kernel with argument names passed from the algorithm layer that
    would clash with stencil-name, stencil-dofmap and stencil-size
    variables.'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.13_single_stencil.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0(f2_stencil_map, f2, f2_stencil_dofmap, "
            "stencil_cross_1, f3_stencil_map, f3, f3_stencil_dofmap, "
            "f2_extent, f3_stencil_size)")
        assert output1 in result
        output2 = (
            "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
            "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type")
        assert output2 in result
        output3 = (
            "      INTEGER, intent(in) :: f2_extent, f3_stencil_size\n"
            "      TYPE(field_type), intent(inout) :: f2_stencil_map, "
            "f3_stencil_map\n"
            "      TYPE(field_type), intent(in) :: f2, f2_stencil_dofmap, "
            "stencil_cross_1, f3, f3_stencil_dofmap\n")
        assert output3 in result
        output4 = (
            "      INTEGER f3_stencil_size_1\n"
            "      INTEGER, pointer :: f3_stencil_dofmap_1(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map_1 => "
            "null()\n"
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap_1(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map_1 => "
            "null()\n")
        assert output4 in result
        output5 = (
            "      TYPE(field_proxy_type) f2_stencil_map_proxy, f2_proxy, "
            "f2_stencil_dofmap_proxy, stencil_cross_1_proxy, "
            "f3_stencil_map_proxy, f3_proxy, f3_stencil_dofmap_proxy\n")
        assert output5 in result
        output6 = (
            "      stencil_cross_1_proxy = stencil_cross_1%get_proxy()")
        assert output6 in result
        output7 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,f2_extent)\n"
            "      f2_stencil_dofmap_1 => "
            "f2_stencil_map_1%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map_1%get_size()\n"
            "      f3_stencil_map_1 => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,f3_stencil_size)\n"
            "      f3_stencil_dofmap_1 => "
            "f3_stencil_map_1%get_whole_dofmap()\n"
            "      f3_stencil_size_1 = f3_stencil_map_1%get_size()\n"
            "      !\n")
        assert output7 in result
        output8 = (
            "        CALL testkern_stencil_code(nlayers, "
            "f2_stencil_map_proxy%data, f2_proxy%data, f2_stencil_size, "
            "f2_stencil_dofmap_1(:,:,cell), f2_stencil_dofmap_proxy%data, "
            "stencil_cross_1_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output8 in result
        output9 = (
            "        CALL testkern_stencil_code(nlayers, "
            "f3_stencil_map_proxy%data, f3_proxy%data, f3_stencil_size_1, "
            "f3_stencil_dofmap_1(:,:,cell), f3_stencil_dofmap_proxy%data, "
            "stencil_cross_1_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert output9 in result


def test_two_stencils_same_field():
    '''Test two Kernels within an invoke, with the same field having a
    stencil access in each kernel. f2_w2 is the field we care
    about. '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "19.14_two_stencils_same_field.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0(f1_w1, f2_w2, f3_w2, f4_w3, f1_w3, "
            "f2_extent, extent)")
        assert output1 in result
        output2 = (
            "      INTEGER f2_w2_stencil_size_1\n"
            "      INTEGER, pointer :: f2_w2_stencil_dofmap_1(:,:,:) => "
            "null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_w2_stencil_map_1 "
            "=> null()")
        assert output2 in result
        output3 = (
            "      INTEGER f2_w2_stencil_size\n"
            "      INTEGER, pointer :: f2_w2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_w2_stencil_map "
            "=> null()")
        assert output3 in result
        output4 = (
            "      f2_w2_stencil_map => f2_w2_proxy%vspace%get_stencil_dofmap"
            "(STENCIL_CROSS,f2_extent)\n"
            "      f2_w2_stencil_dofmap => "
            "f2_w2_stencil_map%get_whole_dofmap()\n"
            "      f2_w2_stencil_size = f2_w2_stencil_map%get_size()\n")
        assert output4 in result
        output5 = (
            "      f2_w2_stencil_map_1 => "
            "f2_w2_proxy%vspace%get_stencil_dofmap(STENCIL_CROSS,extent)\n"
            "      f2_w2_stencil_dofmap_1 => "
            "f2_w2_stencil_map_1%get_whole_dofmap()\n"
            "      f2_w2_stencil_size_1 = f2_w2_stencil_map_1%get_size()\n")
        assert output5 in result
        output6 = (
            "        CALL testkern_stencil_code(nlayers, f1_w1_proxy%data, "
            "f2_w2_proxy%data, f2_w2_stencil_size, "
            "f2_w2_stencil_dofmap(:,:,cell), "
            "f3_w2_proxy%data, f4_w3_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))")
        assert output6 in result
        output7 = (
            "        CALL testkern_stencil_depth_code(nlayers, "
            "f1_w3_proxy%data, f1_w1_proxy%data, f1_w1_stencil_size, "
            "f1_w1_stencil_dofmap(:,:,cell), f2_w2_proxy%data, "
            "f2_w2_stencil_size_1, "
            "f2_w2_stencil_dofmap_1(:,:,cell), f4_w3_proxy%data, "
            "f4_w3_stencil_size, "
            "f4_w3_stencil_dofmap(:,:,cell), ndf_w3, undf_w3, map_w3(:,cell), "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell))")
        assert output7 in result


def test_stencils_same_field_literal_extent():  # pylint: disable=invalid-name
    '''Test three Kernels within an invoke, with the same field having a
    stencil access in each kernel and the extent being passed as a
    literal value. Extent is the same in two kernels and different in
    the third. '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.15_stencils_same_field_literal_extent.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "      INTEGER f2_stencil_size_1\n"
            "      INTEGER, pointer :: f2_stencil_dofmap_1(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map_1 "
            "=> null()\n"
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map "
            "=> null()")
        assert output1 in result
        output2 = (
            "      !\n"
            "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,1)\n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,2)\n"
            "      f2_stencil_dofmap_1 => "
            "f2_stencil_map_1%get_whole_dofmap()\n"
            "      f2_stencil_size_1 = f2_stencil_map_1%get_size()\n"
            "      !")
        assert output2 in result
        output3 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size, f2_stencil_dofmap(:,:,cell), "
            "f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert result.count(output3) == 2
        output4 = (
            "        CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size_1, f2_stencil_dofmap_1(:,:,cell), "
            "f3_proxy%data, f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))")
        assert result.count(output4) == 1

        if dist_mem:
            assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
            assert "CALL f2_proxy%halo_exchange(depth=3)" in result
            assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f3_proxy%halo_exchange(depth=1)" in result
            assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencils_same_field_literal_direct():  # pylint: disable=invalid-name
    '''Test three Kernels within an invoke, with the same field having a
    stencil access in each kernel and the direction being passed as a
    literal value. In two kernels the direction value is the same and
    in the third it is different. '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.16_stencils_same_field_literal_direction.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "      INTEGER f2_stencil_size_1\n"
            "      INTEGER, pointer :: f2_stencil_dofmap_1(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map_1 "
            "=> null()\n"
            "      INTEGER f2_stencil_size\n"
            "      INTEGER, pointer :: f2_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map "
            "=> null()")
        assert output1 in result
        output2 = (
            "      !\n"
            "      IF (x_direction .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,2)\n"
            "      END IF \n"
            "      IF (x_direction .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,2)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      IF (y_direction .eq. x_direction) THEN\n"
            "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,2)\n"
            "      END IF \n"
            "      IF (y_direction .eq. y_direction) THEN\n"
            "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,2)\n"
            "      END IF \n"
            "      f2_stencil_dofmap_1 => "
            "f2_stencil_map_1%get_whole_dofmap()\n"
            "      f2_stencil_size_1 = f2_stencil_map_1%get_size()\n"
            "      !")
        assert output2 in result
        output3 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size, x_direction, "
            "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert result.count(output3) == 2
        output4 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size_1, y_direction, "
            "f2_stencil_dofmap_1(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert result.count(output4) == 1

        if dist_mem:
            assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
            assert "CALL f2_proxy%halo_exchange(depth=3)" in result
            assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f3_proxy%halo_exchange(depth=1)" in result
            assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencil_extent_specified():
    '''the function stencil_unique_str() raises an error if a stencil
    with an extent provided in the metadata is passed in. This is because
    this is not currently supported. This test checks that the appropriate
    error is raised. '''
    # load an example with an argument that has stencil metadata
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    # access the argument with stencil metadata
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[3].children[0]
    stencil_arg = kernel.arguments.args[1]
    # artificially add an extent to the stencil metadata info
    stencil_arg.descriptor.stencil['extent'] = 1
    from psyclone.dynamo0p3 import stencil_unique_str
    with pytest.raises(GenerationError) as err:
        stencil_unique_str(stencil_arg, "")
    assert ("found a stencil with an extent specified in the metadata. "
            "This is not coded for." in str(err))


def test_haloexchange_unknown_halo_depth():  # pylint: disable=invalid-name
    '''If a stencil extent is provided in the kernel metadata then the
    value is stored in an instance of the DynHaloExchange class. This test
    checks that the value is stored as expected (although stencil extents
    in metadata are not currently supported in PSyclone).'''
    # load an example with an argument that has stencil metadata
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    # access the argument with stencil metadata
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[3].children[0]
    stencil_arg = kernel.arguments.args[1]
    # artificially add an extent to the stencil metadata info
    stencil_arg.descriptor.stencil['extent'] = 10
    halo_exchange = schedule.children[0]
    assert halo_exchange._compute_halo_depth() == '11'


def test_haloexchange_correct_parent():  # pylint: disable=invalid-name
    '''Test that a dynamo haloexchange has the correct parent once it has
    been added to a schedule.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    for child in schedule.children:
        assert child.parent == schedule


def test_one_kern_multi_field_same_stencil():  # pylint: disable=invalid-name
    '''This test checks for the case where we have the same stencil used
    by more than one field in a kernel'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.17_single_kernel_multi_field_same_stencil.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "    SUBROUTINE invoke_0_testkern_multi_field_same_stencil_type("
            "f0, f1, f2, f3, f4, extent, direction)")
        assert output1 in result
        output2 = (
            "      INTEGER, intent(in) :: extent\n"
            "      INTEGER, intent(in) :: direction\n")
        assert output2 in result
        output3 = (
            "      INTEGER f3_stencil_size\n"
            "      INTEGER, pointer :: f3_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
            "null()\n"
            "      INTEGER f1_stencil_size\n"
            "      INTEGER, pointer :: f1_stencil_dofmap(:,:,:) => null()\n"
            "      TYPE(stencil_dofmap_type), pointer :: f1_stencil_map => "
            "null()\n")
        assert output3 in result
        output4 = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f1_stencil_map => f1_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f1_stencil_dofmap => f1_stencil_map%get_whole_dofmap()\n"
            "      f1_stencil_size = f1_stencil_map%get_size()\n"
            "      IF (direction .eq. x_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,extent)\n"
            "      END IF \n"
            "      IF (direction .eq. y_direction) THEN\n"
            "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,extent)\n"
            "      END IF \n"
            "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
            "      f3_stencil_size = f3_stencil_map%get_size()\n"
            "      !\n")
        assert output4 in result
        output5 = (
            "        CALL testkern_multi_field_same_stencil_code(nlayers, "
            "f0_proxy%data, f1_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), f2_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), f3_proxy%data, f3_stencil_size, "
            "direction, f3_stencil_dofmap(:,:,cell), f4_proxy%data, "
            "f3_stencil_size, direction, f3_stencil_dofmap(:,:,cell), "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell))")
        assert output5 in result


def test_single_kernel_any_space_stencil():  # pylint: disable=invalid-name
    '''This is a test for stencils and any_space within a single kernel
    and between kernels. We test when any_space is the same and when
    it is different within kernels and between kernels for the case of
    different fields. When it is the same we should have the same
    stencil dofmap (as all other stencil information is the same) and
    when it is different we should have a different stencil dofmap (as
    we do not know whether they are on the same space). '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.18_anyspace_stencil_1.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "      f1_stencil_map => f1_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f1_stencil_dofmap => f1_stencil_map%get_whole_dofmap()\n"
            "      f1_stencil_size = f1_stencil_map%get_size()\n"
            "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
            "      f4_stencil_size = f4_stencil_map%get_size()\n"
            "      f5_stencil_map => f5_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f5_stencil_dofmap => f5_stencil_map%get_whole_dofmap()\n"
            "      f5_stencil_size = f5_stencil_map%get_size()\n"
            "      !\n")
        assert output1 in result
        # use the same stencil dofmap
        output2 = (
            "        CALL testkern_same_anyspace_stencil_code(nlayers, "
            "f0_proxy%data, f1_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), f2_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_any_space_1_f1, undf_any_space_1_f1, "
            "map_any_space_1_f1(:,cell))")
        assert output2 in result
        output3 = (
            "        CALL testkern_different_anyspace_stencil_code(nlayers, "
            "f3_proxy%data, f4_proxy%data, f4_stencil_size, "
            "f4_stencil_dofmap(:,:,cell), f5_proxy%data, f5_stencil_size, "
            "f5_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_any_space_1_f4, undf_any_space_1_f4, "
            "map_any_space_1_f4(:,cell), ndf_any_space_2_f5, "
            "undf_any_space_2_f5, map_any_space_2_f5(:,cell))")
        # use a different stencil dofmap
        assert output3 in result


@pytest.mark.xfail(reason="stencils and any_space produces too many dofmaps")
def test_multi_kernel_any_space_stencil_1():  # pylint: disable=invalid-name
    '''This is a test for stencils and any_space with two kernels. We test
    when any_space is the same and when it is different for the same
    field. In our example we should have a single dofmap. However, at
    the moment we produce two. This is valid but not optimal. It is
    not a big deal at the moment as the Met Office do not plan to use
    any_space but it should be able to be fixed when we get dependence
    analysis within invokes working. Therefore making it xfail for the
    moment. '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.19_anyspace_stencil_2.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = (
            "      f1_stencil_map => f1_proxy%vspace%get_stencil_dofmap("
            "STENCIL_CROSS,extent)\n"
            "      f1_stencil_dofmap => f1_stencil_map%get_whole_dofmap()\n"
            "      f1_stencil_size = f1_stencil_map%get_size()\n"
            "      !\n")
        assert output1 in result
        output2 = (
            "        CALL testkern_same_anyspace_stencil_code(nlayers, "
            "f0_proxy%data, f1_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), f2_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_any_space_1_f1, undf_any_space_1_f1, map_any_space_1_f1)")
        assert output2 in result
        output3 = (
            "        CALL testkern_different_anyspace_stencil_code(nlayers, "
            "f3_proxy%data, f1_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), f2_proxy%data, f1_stencil_size, "
            "f1_stencil_dofmap(:,:,cell), ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_any_space_1_f1, undf_any_space_1_f1, map_any_space_1_f1, "
            "ndf_any_space_2_f2, undf_any_space_2_f2, map_any_space_2_f2)")
        assert output3 in result


def test_stencil_args_unique_1():
    '''This test checks that stencil extent and direction arguments do not
    clash with internal names generated in the PSy-layer. f2_stencil_size
    and nlayers are chosen as the names that would clash.'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.21_stencil_names_clash.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        # we use f2_stencil_size for extent and nlayers for direction
        # as arguments
        output1 = ("    SUBROUTINE invoke_0_testkern_stencil_xory1d_type(f1, "
                   "f2, f3, f4, f2_stencil_size, nlayers)")
        assert output1 in result
        output2 = ("      INTEGER, intent(in) :: f2_stencil_size\n"
                   "      INTEGER, intent(in) :: nlayers")
        assert output2 in result
        output3 = "      INTEGER f2_stencil_size_1"
        assert output3 in result
        # therefore the local variable is now declared as nlayers_1"
        output4 = "      INTEGER nlayers_1"
        assert output4 in result
        output5 = "      nlayers_1 = f1_proxy%vspace%get_nlayers()"
        assert output5 in result
        output6 = (
            "      IF (nlayers .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,f2_stencil_size)\n"
            "      END IF \n"
            "      IF (nlayers .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,f2_stencil_size)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size_1 = f2_stencil_map%get_size()")
        assert output6 in result
        output7 = (
            "        CALL testkern_stencil_xory1d_code(nlayers_1, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size_1, nlayers, "
            "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output7 in result


def test_stencil_args_unique_2():
    '''This test checks that stencil extent and direction arguments are
    unique within the generated PSy-layer when they are accessed as
    indexed arrays, with the same array name, from the algorithm
    layer.'''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.22_stencil_names_indexed.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        output1 = ("    SUBROUTINE invoke_0(f1, f2, f3, f4, f2_info, "
                   "f2_info_2, f2_info_1, f2_info_3)")
        assert output1 in result
        output2 = (
            "      INTEGER, intent(in) :: f2_info, f2_info_2\n"
            "      INTEGER, intent(in) :: f2_info_1, f2_info_3")
        assert output2 in result
        output3 = (
            "      IF (f2_info_1 .eq. x_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,f2_info)\n"
            "      END IF \n"
            "      IF (f2_info_1 .eq. y_direction) THEN\n"
            "        f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,f2_info)\n"
            "      END IF \n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      IF (f2_info_3 .eq. x_direction) THEN\n"
            "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DX,f2_info_2)\n"
            "      END IF \n"
            "      IF (f2_info_3 .eq. y_direction) THEN\n"
            "        f2_stencil_map_1 => f2_proxy%vspace%get_stencil_dofmap("
            "STENCIL_1DY,f2_info_2)\n"
            "      END IF ")
        assert output3 in result
        output4 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size, f2_info_1, "
            "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output4 in result
        output5 = (
            "        CALL testkern_stencil_xory1d_code(nlayers, "
            "f1_proxy%data, f2_proxy%data, f2_stencil_size_1, f2_info_3, "
            "f2_stencil_dofmap_1(:,:,cell), f3_proxy%data, f4_proxy%data, "
            "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
            "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))")
        assert output5 in result
        if dist_mem:
            assert (
                "IF (f2_proxy%is_dirty(depth=max(f2_info+1,"
                "f2_info_2+1))) THEN" in result)
            assert (
                "CALL f2_proxy%halo_exchange(depth=max(f2_info+1,"
                "f2_info_2+1))" in result)
            assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f3_proxy%halo_exchange(depth=1)" in result
            assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_stencil_args_unique_3():
    '''This test checks that stencil extent and direction arguments are
    unique within the generated PSy-layer when they are dereferenced,
    with the same type/class name, from the algorithm layer. '''
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "19.23_stencil_names_deref.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        result = str(psy.gen)
        print result
        assert (
            "      INTEGER, intent(in) :: my_info_f2_info, my_info_f2_info_2\n"
            "      INTEGER, intent(in) :: my_info_f2_info_1, "
            "my_info_f2_info_3\n"
            in result)
        assert (
            "f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap(STENCIL_1DX,"
            "my_info_f2_info)" in result)
        if dist_mem:
            assert (
                "IF (f2_proxy%is_dirty(depth=max(my_info_f2_info+1,"
                "my_info_f2_info_2+1))) THEN" in result)
            assert (
                "CALL f2_proxy%halo_exchange(depth=max(my_info_f2_info+1,"
                "my_info_f2_info_2+1))" in result)
            assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f3_proxy%halo_exchange(depth=1)" in result
            assert "IF (f4_proxy%is_dirty(depth=1)) THEN" in result
            assert "CALL f4_proxy%halo_exchange(depth=1)" in result


def test_dynloop_load_unexpected_func_space():  # pylint: disable=invalid-name
    '''The load function of an instance of the dynloop class raises an
    error if an unexpexted function space is found. This test makes
    sure this error works correctly. It's a little tricky to raise
    this error as it is unreachable. However, we can sabotage an
    earlier function to make it return an invalid value. '''
    # first create a working instance of the DynLoop class
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    # now get access to the DynLoop class, the associated kernel class
    # and the associated field.
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]
    kernel = loop.children[0]
    field = kernel.arguments.iteration_space_arg()
    # break the fields function space
    field._function_spaces[0]._orig_name = "broken"
    # create a function which always returns the broken field

    def broken_func():
        ''' returns the above field no matter what '''
        return field
    # replace the iteration_space_arg method with our broke
    # function. This is required as iteration_space_arg currently
    # never returns a field with an invalid function space.
    kernel.arguments.iteration_space_arg = broken_func
    # We can now raise the exception.
    with pytest.raises(GenerationError) as err:
        loop.load(kernel)
    assert ("Generation Error: Unexpected function space found. Expecting "
            "one of ['w3', 'w0', 'w1', 'w2', 'wtheta', 'w2h', 'w2v', "
            "'any_w2'] but found 'broken'" in str(err))


def test_dynkernargs_unexpect_stencil_extent():  # pylint: disable=invalid-name
    '''This test checks that we raise an error in DynKernelArguments if
    metadata is provided with an extent value. This is a litle tricky to
    raise as the parser does not not allow this to happen. We therefore
    modify the results from the parser to raise the error.'''
    # parse some valid code with a stencil
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api="dynamo0.3")
    # find the parsed code's call class
    call = invoke_info.calls.values()[0].kcalls[0]
    # add an extent to the stencil metadata
    kernel_metadata = call.ktype
    kernel_metadata._arg_descriptors[1].stencil['extent'] = 2
    # remove the extra argument (as the extent value no longer needs
    # to be passed so an associated error will be raised)
    del call.args[2]
    # finally call our object to raise the error
    from psyclone.dynamo0p3 import DynKernelArguments
    with pytest.raises(GenerationError) as err:
        _ = DynKernelArguments(call, None)
    assert "extent metadata not yet supported" in str(err)


def test_unsupported_halo_read_access():  # pylint: disable=invalid-name
    '''This test checks that we raise an error if the halo_read_access
    method finds an upper bound other than halo or ncells. The
    particular issue at the moment is that if inner is specified we do
    not know whether the stencil accesses the halo or not. However,
    this limitation is not going to affect anyone until we add in loop
    iteration space splitting transformations.
    '''
    # create a valid loop with a stencil access
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    # get access to the DynLoop object
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]
    # access to the argument that has a stencil access in the kernel
    kernel = loop.children[0]
    stencil_arg = kernel.arguments.args[1]
    loop.set_upper_bound("inner", 1)
    # call our method
    with pytest.raises(GenerationError) as err:
        _ = loop._halo_read_access(stencil_arg)
    assert ("Loop bounds other than cell_halo and ncells are currently "
            "unsupported for kernels with stencil accesses. Found "
            "'inner'." in str(err))


def test_dynglobalsum_unsupported_scalar():  # pylint: disable=invalid-name
    '''Check that an instance of the DynGlobalSum class raises an
    exception if an unsupported scalar type is provided when
    dm=True '''
    # get an instance of an integer scalar
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]
    kernel = loop.children[0]
    argument = kernel.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        _ = DynGlobalSum(argument)
    assert "DynGlobalSum currently only supports '['gh_real']'" in str(err)


def test_dynglobalsum_nodm_error():
    '''Check that an instance of the DynGlobalSum class raises an
    exception if it is instantiated when dm=False'''
    # get an instance of a real scalar
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.children[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as err:
        _ = DynGlobalSum(argument)
    assert ("It makes no sense to create a DynGlobalSum object when "
            "dm=False") in str(err)


def test_no_updated_args():
    ''' Check that we raise the expected exception when we encounter a
    kernel that does not write to any of its arguments '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_read,w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A Dynamo 0.3 kernel must have at least one argument that is "
            "updated (written to) but found none for kernel "
            "testkern_qr_type" in str(excinfo))


def test_scalars_only_invalid():
    ''' Check that we raise the expected exception if we encounter a
    kernel that only has (read-only) scalar arguments '''
    fparser.logging.disable('CRITICAL')
    code = '''
module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), meta_args(2) =                 &
          (/ arg_type(gh_real, gh_read),            &
             arg_type(gh_integer, gh_read)          &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains
  subroutine testkern_code(a,b)
  end subroutine testkern_code
end module testkern
'''
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A Dynamo 0.3 kernel must have at least one argument that is "
            "updated (written to) but found none for kernel "
            "testkern_type" in str(excinfo))


def test_multiple_updated_field_args():  # pylint: disable=invalid-name
    ''' Check that we successfully parse a kernel that writes to more
    than one of its field arguments '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_read, w2)",
                        "arg_type(gh_field,gh_write, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    count = 0
    for descriptor in metadata.arg_descriptors:
        if descriptor.type == "gh_field" and descriptor.access != "gh_read":
            count += 1
    assert count == 2


def test_multiple_updated_op_args():
    ''' Check that we successfully parse the metadata for a kernel that
    writes to more than one of its field and operator arguments '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_write, w1, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    count = 0
    for descriptor in metadata.arg_descriptors:
        if ((descriptor.type == "gh_field" or
             descriptor.type == "gh_operator") and
                descriptor.access != "gh_read"):
            count += 1
    assert count == 2


def test_multiple_updated_scalar_args():  # pylint: disable=invalid-name
    ''' Check that we raise the expected exception when we encounter a
    kernel that writes to more than one of its field and scalar arguments '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_real, gh_read)",
                        "arg_type(gh_real, gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A user-supplied Dynamo 0.3 kernel must not write/update a scalar "
            "argument but kernel testkern_qr_type has" in
            str(excinfo))


def test_itn_space_write_w3_w1():  # pylint: disable=invalid-name
    ''' Check that generated loop over cells in the psy layer has the correct
    upper bound when a kernel writes to two fields, the first on a
    discontinuous space and the second on a continuous space. The resulting
    loop (when dm=True) must include the L1 halo because of the second
    field argument which is continuous '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.1_single_invoke_write_multi_fs.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        if dist_mem:
            output = (
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
            assert output in generated_code
        else:
            output = (
                "      ! Call our kernels\n"
                "      !\n"
                "      DO cell=1,m2_proxy%vspace%get_ncell()\n")
            assert output in generated_code


def test_itn_space_fld_and_op_writers():  # pylint: disable=invalid-name
    ''' Check that generated loop over cells in the psy layer has the
    correct upper bound when a kernel writes to both an operator and a
    field, the latter on a discontinuous space and first in the list
    of args. (Loop must include L1 halo because we're writing to an
    operator.) '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.2_single_invoke_write_fld_op.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        if dist_mem:
            output = (
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
            assert output in generated_code
        else:
            output = (
                "      ! Call our kernels\n"
                "      !\n"
                "      DO cell=1,op1_proxy%fs_from%get_ncell()\n")
            assert output in generated_code


def test_itn_space_any_w3():
    ''' Check generated loop over cells has correct upper bound when
    a kernel writes to fields on any-space and W3 '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.3_single_invoke_write_anyspace_w3.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        if dist_mem:
            output = (
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
            assert output in generated_code
        else:
            output = (
                "      ! Call our kernels\n"
                "      !\n"
                "      DO cell=1,f1_proxy%vspace%get_ncell()\n")
            assert output in generated_code


def test_itn_space_any_w1():
    ''' Check generated loop over cells has correct upper bound when
    a kernel writes to fields on any-space and W1 '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.4_single_invoke_write_anyspace_w1.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        if dist_mem:
            output = (
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
            assert output in generated_code
        else:
            # Loop upper bound should use f2 as that field is *definitely*
            # on a continuous space (as opposed to the one on any_space
            # that might be).
            output = (
                "      ! Call our kernels\n"
                "      !\n"
                "      DO cell=1,f2_proxy%vspace%get_ncell()\n")
            assert output in generated_code


def test_unexpected_type_error():
    '''Check that we raise an exception if an unexpected datatype is found
    when running the ArgOrdering generate method. As it is abstract we use
    the KernCallArgList sub class'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "1.0.1_single_named_invoke.f90"),
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        if distmem:
            index = 3
        else:
            index = 0
        loop = schedule.children[index]
        kernel = loop.children[0]
        # sabotage one of the arguments to make it have an invalid type.
        kernel.arguments.args[0]._type = "invalid"
        # Now call KernCallArgList to raise an exception
        from psyclone.dynamo0p3 import KernCallArgList
        create_arg_list = KernCallArgList(kernel)
        with pytest.raises(GenerationError) as excinfo:
            create_arg_list.generate()
        assert (
            "Unexpected arg type found in dynamo0p3.py:ArgOrdering:"
            "generate()") in str(excinfo.value)


def test_argordering_exceptions():
    '''Check that we raise an exception if the abstract methods are called
    in an instance of the ArgOrdering class '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "1.0.1_single_named_invoke.f90"),
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        if distmem:
            index = 3
        else:
            index = 0
        loop = schedule.children[index]
        kernel = loop.children[0]
        from psyclone.dynamo0p3 import ArgOrdering
        create_arg_list = ArgOrdering(kernel)
        for method in [create_arg_list.cell_position,
                       create_arg_list.mesh_height,
                       create_arg_list.mesh_ncell2d,
                       create_arg_list.quad_rule]:
            with pytest.raises(NotImplementedError):
                method()
        for method in [create_arg_list.field_vector,
                       create_arg_list.field,
                       create_arg_list.stencil_unknown_extent,
                       create_arg_list.stencil_unknown_direction,
                       create_arg_list.stencil,
                       create_arg_list.operator,
                       create_arg_list.scalar,
                       create_arg_list.fs_common,
                       create_arg_list.fs_compulsory_field,
                       create_arg_list.basis,
                       create_arg_list.diff_basis,
                       create_arg_list.orientation,
                       create_arg_list.field_bcs_kernel,
                       create_arg_list.operator_bcs_kernel,
                       create_arg_list.banded_dofmap,
                       create_arg_list.indirection_dofmap,
                       create_arg_list.cma_operator]:
            with pytest.raises(NotImplementedError):
                method(None)


def test_kernel_args_has_op():
    ''' Check that we raise an exception if the arg. type supplied to
    DynKernelArguments.has_operator() is not a valid operator '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api="dynamo0.3")
    # find the parsed code's call class
    call = invoke_info.calls.values()[0].kcalls[0]
    from psyclone.dynamo0p3 import DynKernelArguments
    dka = DynKernelArguments(call, None)
    with pytest.raises(GenerationError) as excinfo:
        _ = dka.has_operator(op_type="gh_field")
    assert "op_type must be a valid operator type" in str(excinfo)


def test_kernel_stub_invalid_scalar_argument():  # pylint: disable=invalid-name
    '''Check that we raise an exception if an unexpected datatype is found
    when using the KernStubArgList scalar method'''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # sabotage the scalar argument to make it have an invalid type.
    arg = kernel.arguments.args[1]
    arg._type = "invalid"
    # create a temporary module to add code into
    from psyclone.f2pygen import ModuleGen
    module = ModuleGen("module_name")
    # Now call KernStubArgList to raise an exception
    from psyclone.dynamo0p3 import KernStubArgList
    create_arg_list = KernStubArgList(kernel, module)
    with pytest.raises(GenerationError) as excinfo:
        create_arg_list.scalar(arg)
    assert (
        "Internal error: expected arg type to be one of '['gh_real', "
        "'gh_integer']' but got 'invalid'") in str(excinfo.value)


def test_kernel_stub_ind_dofmap_errors():  # pylint: disable=invalid-name
    '''Check that we raise the expected exceptions if the wrong arguments
    are supplied to KernelStubArgList.indirection_dofmap() '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # create a temporary module to add code into
    from psyclone.f2pygen import ModuleGen
    module = ModuleGen("module_name")
    # Now call KernStubArgList to raise an exception
    from psyclone.dynamo0p3 import KernStubArgList
    create_arg_list = KernStubArgList(kernel, module)
    # First call it without an argument object
    with pytest.raises(GenerationError) as excinfo:
        create_arg_list.indirection_dofmap("w3")
    assert "no CMA operator supplied" in str(excinfo)
    # Second, call it with an argument object but one that is not
    # an operator
    with pytest.raises(GenerationError) as excinfo:
        create_arg_list.indirection_dofmap("w3", kernel.arguments.args[1])
    assert ("a CMA operator (gh_columnwise_operator) must be supplied but "
            "got") in str(excinfo)


def test_kerncallarglist_arglist_error():  # pylint: disable=invalid-name
    '''Check that we raise an exception if we call the arglist method in
    kerncallarglist without first calling the generate method'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "1.0.1_single_named_invoke.f90"),
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        if distmem:
            index = 3
        else:
            index = 0
        loop = schedule.children[index]
        kernel = loop.children[0]
        from psyclone.dynamo0p3 import KernCallArgList
        create_arg_list = KernCallArgList(kernel)
        with pytest.raises(GenerationError) as excinfo:
            _ = create_arg_list.arglist
        assert (
            "Internal error. The argument list in KernCallArgList:arglist() "
            "is empty. Has the generate() method been "
            "called?") in str(excinfo.value)


def test_kernstubarglist_arglist_error():  # pylint: disable=invalid-name
    '''Check that we raise an exception if we call the arglist method in
    kernstubarglist without first calling the generate method'''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # create a temporary module to add code into
    from psyclone.f2pygen import ModuleGen
    module = ModuleGen("module_name")
    # Now call KernStubArgList to raise an exception
    from psyclone.dynamo0p3 import KernStubArgList
    create_arg_list = KernStubArgList(kernel, module)
    with pytest.raises(GenerationError) as excinfo:
        _ = create_arg_list.arglist
    assert (
        "Internal error. The argument list in KernStubArgList:arglist() is "
        "empty. Has the generate() method been "
        "called?") in str(excinfo.value)


def test_multi_anyw2():
    '''Check generated code works correctly when we have multiple any_w2
    fields. Particularly check that we only generate a single lookup.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.1_single_invoke_multi_anyw2.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        if dist_mem:
            output = (
                "      ! Look-up dofmaps for each function space\n"
                "      !\n"
                "      map_any_w2 => f1_proxy%vspace%get_whole_dofmap()\n"
                "      !\n"
                "      ! Initialise number of DoFs "
                "for any_w2\n"
                "      !\n"
                "      ndf_any_w2 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_w2 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n"
                "        !\n"
                "        CALL testkern_multi_anyw2_code(nlayers, "
                "f1_proxy%data, f2_proxy%data, f3_proxy%data, ndf_any_w2, "
                "undf_any_w2, map_any_w2(:,cell))\n"
                "      END DO \n"
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
                "      ! Initialise number of DoFs for "
                "any_w2\n"
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
                "      END DO ")
            assert output in generated_code


def test_anyw2_vectors():
    '''Check generated code works correctly when we have any_w2 field
    vectors'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.3_single_invoke_anyw2_vector.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        assert "f3_proxy(1) = f3(1)%get_proxy()" in generated_code
        assert "f3_proxy(2) = f3(2)%get_proxy()" in generated_code
        assert "f3_proxy(1)%data, f3_proxy(2)%data" in generated_code


def test_anyw2_operators():
    '''Check generated code works correctly when we have any_w2 fields
    with operators'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.4_single_invoke_anyw2_operator.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
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
            "      ! Compute basis arrays\n"
            "      !\n"
            "      CALL qr%compute_function(BASIS, mm_w2_proxy%fs_from, "
            "dim_any_w2, ndf_any_w2, basis_any_w2_qr)")
        assert output in generated_code


def test_anyw2_stencils():
    '''Check generated code works correctly when we have any_w2 fields
    with stencils'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.5_single_invoke_anyw2_stencil.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        output = (
            "      ! Initialise stencil dofmaps\n"
            "      !\n"
            "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap"
            "(STENCIL_CROSS,extent)\n"
            "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
            "      f2_stencil_size = f2_stencil_map%get_size()\n"
            "      !\n")
        assert output in generated_code


# test that stub generation works with any_w2 space.
def test_stub_generate_with_anyw2():
    '''check that the stub generate produces the expected output when we
    have any_w2 fields. In particular, check basis functions as these
    have specific sizes associated with the particular function space'''
    result = generate(os.path.join(BASE_PATH,
                                   "testkern_multi_anyw2_basis_mod.f90"),
                      api="dynamo0.3")
    print result
    expected_output = (
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_any_w2,"
        "np_xy,np_z) :: basis_any_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_any_w2,"
        "np_xy,np_z) :: diff_basis_any_w2")
    assert expected_output in str(result)


def test_no_halo_for_discontinous():
    '''Test that we do not create halo exchange calls when our loop only
    iterates over owned cells (e.g. it writes to a discontinuous
    field), we only read from a discontinous field and there are no
    stencil accesses'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3_only.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    result = str(psy.gen)
    print result
    assert "halo_exchange" not in result


def test_halo_for_discontinuous():
    '''Test that we create halo exchange call when our loop iterates over
    owned cells (e.g. it writes to a discontinuous field), we read
    from a continous field, there are no stencil accesses, but we do
    not know anything about the previous writer. As the previous
    writer may have been over dofs we could have dirty annexed dofs so
    need to add a halo exchange.'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    result = str(psy.gen)
    print result
    assert "IF (f1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL f1_proxy%halo_exchange(depth=1)" in result
    assert "IF (f2_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result


def test_halo_for_discontinuous_2():
    '''Test that we create halo exchange call when our loop iterates over
    owned cells (e.g. it writes to a discontinuous field), we read
    from a continous field, there are no stencil accesses, and the
    previous writer iterates over ndofs. We therefore have dirty
    annexed dofs so need to add a halo exchange. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.7_halo_annexed.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    result = str(psy.gen)
    print result
    assert "IF (f1_proxy%is_dirty(depth=1)) THEN" not in result
    assert "CALL f1_proxy%halo_exchange(depth=1)" in result
    assert "IF (f2_proxy%is_dirty(depth=1)) THEN" not in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result


def test_arg_discontinous():
    '''test that the discontinuous method in the dynamo argument class
    returns the correct values '''

    # 1 discontinuous field returns true
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3_only.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[0].children[0]
    field = kernel.arguments.args[0]
    assert field.space == 'w3'
    assert field.discontinuous

    # 2 any_space field returns false
    _, info = parse(os.path.join(BASE_PATH,
                                 "11_any_space.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[5].children[0]
    field = kernel.arguments.args[0]
    assert field.space == 'any_space_1'
    assert not field.discontinuous

    # 3 continuous field returns false
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.children[3].children[0]
    field = kernel.arguments.args[1]
    assert field.space == 'w1'
    assert not field.discontinuous


def test_halo_stencil_redundant_computation():  # pylint: disable=invalid-name
    '''If a loop contains a kernel with a stencil access and the loop
    computes redundantly into the halo then the value of the stencil
    in the associated halo exchange is returned as type region
    irrespective of the type of kernel stencil. This is because the
    redundant computation will be performed all points (equivalent
    to a full halo) and there is no support for mixing accesses at
    different levels. In this example the kernel stencil is cross.'''

    _, info = parse(os.path.join(BASE_PATH,
                                 "19.1_single_stencil.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    stencil_halo_exchange = schedule.children[0]
    assert stencil_halo_exchange._compute_stencil_type() == "region"


def test_halo_same_stencils_no_red_comp():  # pylint: disable=invalid-name
    '''If a halo has two or more different halo reads associated with it
    and the type of stencils are the same and the loops do not
    redundantly compute into the halo then the chosen stencil type for
    the halo exchange is the same as the kernel stencil type. In this
    case both are cross'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.8_halo_same_stencils.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    stencil_halo_exchange = schedule.children[1]
    assert stencil_halo_exchange._compute_stencil_type() == "cross"


def test_halo_different_stencils_no_red_comp():  # pylint: disable=invalid-name
    '''If a halo has two or more different halo reads associated with it
    and the type of stencils are different and the loops do not
    redundantly compute into the halo then the chosen stencil type is
    region. In this case, one is xory and the other is cross, We could
    try to be more clever here in the future as the actual minimum is
    cross!'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.9_halo_different_stencils.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    stencil_halo_exchange = schedule.children[1]
    assert stencil_halo_exchange._compute_stencil_type() == "region"


def test_comp_halo_intern_err(monkeypatch):  # pylint: disable=invalid-name
    '''Check that we raise an exception if the compute_halo_read_info method in
    dynhaloexchange does not find any read dependencies. This should
    never be the case. We use monkeypatch to force the exception to be
    raised'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    monkeypatch.setattr(field, "forward_read_dependencies", lambda: [])
    with pytest.raises(GenerationError) as excinfo:
        halo_exchange._compute_halo_read_info()
    assert ("Internal logic error. There should be at least one read "
            "dependence for a halo exchange") in str(excinfo.value)


def test_halo_exch_1_back_dep(monkeypatch):  # pylint: disable=invalid-name
    '''Check that an internal error is raised if a halo exchange returns
    with more than one write dependency. It should only ever be 0 or 1.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
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


def test_halo_ex_back_dep_no_call(monkeypatch):  # pylint: disable=invalid-name
    '''Check that an internal error is raised if a halo exchange
    write dependency is not a call.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.9_halo_different_stencils.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
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
            "call but found <type 'function'>") in str(excinfo.value)


def test_HaloReadAccess_input_field():  # pylint: disable=invalid-name
    '''The HaloReadAccess class expects a DynKernelArgument or equivalent
    object as input. If this is not the case an exception is raised. This
    test checks that this exception is raised correctly.'''
    with pytest.raises(GenerationError) as excinfo:
        _ = HaloReadAccess(None)
    assert (
        "Generation Error: HaloInfo class expects an argument of type "
        "DynArgument, or equivalent, on initialisation, but found, "
        "'<type 'NoneType'>'" in str(excinfo.value))


def test_HaloReadAccess_field_in_call():  # pylint: disable=invalid-name
    '''The field passed to HaloReadAccess should be within a kernel or
    builtin. If it is not then an exception is raised. This test
    checks that this exception is raised correctly'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    with pytest.raises(GenerationError) as excinfo:
        _ = HaloReadAccess(field)
    assert ("field 'f2' should be from a call but found "
            "<class 'psyclone.dynamo0p3.DynHaloExchange'>"
            in str(excinfo.value))


def test_HaloReadAccess_field_not_reader():  # pylint: disable=invalid-name
    '''The field passed to HaloReadAccess should be read within its associated
    kernel or builtin. If it is not then an exception is raised. This
    test checks that this exception is raised correctly

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke_w3_only.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.children[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as excinfo:
        _ = HaloReadAccess(argument)
    assert (
        "In HaloInfo class, field 'f1' should be one of ['gh_read', "
        "'gh_inc'], but found 'gh_write'" in str(excinfo.value))


def test_HaloRead_inv_loop_upper(monkeypatch):  # pylint: disable=invalid-name
    '''The upper bound of a loop in the compute_halo_read_info method within
    the HaloReadAccesss class should be recognised by the logic. If not an
    exception is raised and this test checks that this exception is
    raised correctly
    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field
    read_dependencies = field.forward_read_dependencies()
    read_dependency = read_dependencies[0]
    loop = read_dependency.call.parent
    monkeypatch.setattr(loop, "_upper_bound_name", "invalid")
    with pytest.raises(GenerationError) as excinfo:
        halo_exchange._compute_halo_read_info()
    assert ("Internal error in HaloReadAccess._compute_from_field. Found "
            "unexpected loop upper bound name 'invalid'") in str(excinfo.value)


def test_HaloReadAccess_discontinuous_field():  # pylint: disable=invalid-name
    '''When a discontinuous argument is read in a loop with an iteration
    space over 'ncells' then it only accesses local dofs. This test
    checks that HaloReadAccess works correctly in this situation'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3_only.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.children[0]
    arg = kernel.arguments.args[1]
    halo_access = HaloReadAccess(arg)
    assert not halo_access.max_depth
    assert halo_access.var_depth is None
    assert halo_access.literal_depth == 0
    assert halo_access.stencil_type is None


def test_loop_cont_read_inv_bound(monkeypatch):  # pylint: disable=invalid-name
    '''When a continuous argument is read it may access the halo. The
    logic for this is in _halo_read_access. If the loop type in this
    routine is not known then an exception is raised. This test checks
    that this exception is raised correctly'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke_w3.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]
    kernel = loop.children[0]
    f1_arg = kernel.arguments.args[1]
    #
    monkeypatch.setattr(loop, "_upper_bound_name", "invalid")
    with pytest.raises(GenerationError) as excinfo:
        _ = loop._halo_read_access(f1_arg)
    assert ("Internal error in _halo_read_access. It should not be "
            "possible to get to here. loop upper bound name is 'invalid' "
            "and arg 'f1' access is 'gh_read'.") in str(excinfo.value)


def test_new_halo_exch_vect_field(monkeypatch):  # pylint: disable=invalid-name
    '''if a field requires (or may require) a halo exchange before it is
    called and it has more than one backward write dependencies then it
    must be a vector (as a vector field requiring a halo exchange should
    have a halo exchange for each vector). The method
    create_halo_exchanges raises an exception if this is not the
    case. This test checks that the exception is raised correctly.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[7]
    kernel = loop.children[0]
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


def test_new_halo_exch_vect_deps(monkeypatch):  # pylint: disable=invalid-name
    '''if a field requires (or may require) a halo exchange before it is
    called and it has more than one backward write dependencies then
    it must be a vector (as a vector field requiring a halo exchange
    should have a halo exchange for each vector) and its vector size
    must equal the number of dependencies. The method
    create_halo_exchanges raises an exception if this is not the
    case. This test checks that the exception is raised correctly.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[7]
    kernel = loop.children[0]
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


def test_new_halo_exch_vect_deps2(monkeypatch):  # pylint: disable=invalid-name
    '''if a field requires (or may require) a halo exchange before it is
    called and it has more than one backward write dependencies then
    it must be a vector (as a vector field requiring a halo exchange
    should have a halo exchange for each component) and each dependency
    should be a halo exchange. The method create_halo_exchanges raises
    an exception if this is not the case. This test checks that the
    exception is raised correctly.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[7]
    kernel = loop.children[0]
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


def test_halo_req_no_read_deps(monkeypatch):  # pylint: disable=invalid-name
    '''If the required method in a halo exchange object does not find any
    read dependencies then there has been an internal error and an
    exception will be raised. This test checks that this exception is
    raised correctly.'''

    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    halo_exchange = schedule.children[0]
    field = halo_exchange.field

    monkeypatch.setattr(field, "_name", "unique")

    with pytest.raises(GenerationError) as excinfo:
        _, _ = halo_exchange.required()
    assert ("Internal logic error. There should be at least one read "
            "dependence for a halo exchange" in str(excinfo.value))


def test_no_halo_exchange_annex_dofs(  # pylint: disable=invalid-name
        tmpdir, f90, f90flags):
    '''If a kernel writes to a discontinuous field and also reads from a
    continuous field then that fields annexed dofs are read (but not
    the rest of its level1 halo). If the previous modification of this
    continuous field makes the annexed dofs valid then no halo
    exchange is required. This is the case when the previous loop
    iterates over cells as it computes into the l1 halo by default
    precisely in order to ensure that the annexed dofs are correct for
    subsequent reading (whilst the rest of the l1 halo ends up being
    dirty).

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.7.1_halo_annexed.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)
    assert "CALL f1_proxy%halo_exchange" in result
    assert "CALL f2_proxy%halo_exchange" not in result
