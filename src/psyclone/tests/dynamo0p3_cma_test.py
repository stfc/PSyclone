# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' This module tests the support for Column-Matrix-Assembly operators in
the Dynamo 0.3 API using pytest. '''

from __future__ import absolute_import
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.parse import ParseError, parse
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.gen_kernel_stub import generate
import utils

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

CMA_ASSEMBLE = '''
module testkern_cma
  type, extends(kernel_type) :: testkern_cma_type
     type(arg_type), meta_args(4) =                 &
          (/ arg_type(gh_operator,gh_read, any_space_1, any_space_2), &
             arg_type(gh_columnwise_operator,gh_write, any_space_1,   &
                      any_space_2),                                   &
             arg_type(gh_field,gh_read, any_space_1),                 &
             arg_type(gh_real, gh_read)                               &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_cma_code
  end type testkern_cma_type
contains
  subroutine testkern_cma_code(a,b,c,d)
  end subroutine testkern_cma_code
end module testkern_cma
'''


def test_cma_mdata_assembly():
    ''' Check that we can parse meta-data entries relating to Column-Matrix
    Assembly '''
    fparser.logging.disable('CRITICAL')
    code = CMA_ASSEMBLE
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space_to[2]='any_space_1'\n"
        "  function_space_from[3]='any_space_2'\n")
    print dkm_str
    assert expected in dkm_str
    assert dkm._cma_operation == "assembly"


def test_cma_mdata_assembly_missing_op():
    ''' Check that we raise the expected error if the supplied meta-data
    is assembling a gh_columnwise_operator but doesn't have a read-only
    gh_operator '''
    fparser.logging.disable('CRITICAL')
    # Remove  the (required) LMA operator
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_operator,gh_read, any_space_1, any_space_2),", "", 1)
    code = code.replace("meta_args(4) =", "meta_args(3) =", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type has a single column-wise operator "
            "argument but does not conform to the rules for an Assembly "
            "kernel because it does not have any read-only LMA operator "
            "arguments") in str(excinfo)


def test_cma_mdata_multi_writes():
    ''' Check that we raise the expected error if the supplied meta-data
    specifies more than one CMA operator that is written to '''
    fparser.logging.disable('CRITICAL')
    # Replace the field arg with another CMA operator that is written to
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field,gh_read, any_space_1)",
        "arg_type(gh_columnwise_operator,gh_write,any_space_1,any_space_2)",
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A Dynamo 0.3 kernel cannot update more than one CMA "
            "(column-wise) operator but kernel testkern_cma_type "
            "updates 2") in str(excinfo)
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field,gh_read, any_space_1)",
        "arg_type(gh_columnwise_operator,gh_write,any_space_1,any_space_2),&\n"
        "arg_type(gh_columnwise_operator,gh_write,any_space_1,any_space_2)",
        1)
    code = code.replace("meta_args(4) = ", "meta_args(5) = ", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A Dynamo 0.3 kernel cannot update more than one CMA "
            "(column-wise) operator but kernel testkern_cma_type "
            "updates 3") in str(excinfo)


def test_cma_mdata_mutable_op():
    ''' Check that we raise the expected error if the supplied meta-data
    is assembling a gh_columnwise_operator but doesn't have a read-only
    gh_operator '''
    fparser.logging.disable('CRITICAL')
    # Make the LMA operator gh_write instead of gh_read
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_operator,gh_read, any_space_1, any_space_2),",
        "arg_type(gh_operator,gh_write, any_space_1, any_space_2),", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type writes to a column-wise operator but "
            "also writes to ['gh_operator'] argument(s). This is not "
            "allowed.") in str(excinfo)


def test_cma_mdata_writes_lma_op():
    ''' Check that we raise the expected error if the supplied meta-data
    is assembling a gh_columnwise_operator but also writes to a
    gh_operator '''
    fparser.logging.disable('CRITICAL')
    # Add an additional LMA operator that has write access
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_operator,gh_read, any_space_1, any_space_2), &\n",
        "arg_type(gh_operator,gh_read, any_space_1, any_space_2), &\n"
        "arg_type(gh_operator,gh_write, any_space_1, any_space_2), &\n", 1)
    code = code.replace("meta_args(4)", "meta_args(5)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type writes to a column-wise operator but "
            "also writes to ['gh_operator'] argument(s). This is not "
            "allowed.") in str(excinfo)


def test_cma_mdata_assembly_diff_spaces():
    ''' Check that we successfully parse the supplied meta-data if it
    is assembling a gh_columnwise_operator but the to/from spaces don't
    match those of the supplied gh_operator '''
    fparser.logging.disable('CRITICAL')
    # Change the to space of the LMA operator
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_operator,gh_read, any_space_1, any_space_2),",
        "arg_type(gh_operator,gh_read, any_space_3, any_space_2),", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[0])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_operator'\n"
        "  access_descriptor[1]='gh_read'\n"
        "  function_space_to[2]='any_space_3'\n"
        "  function_space_from[3]='any_space_2'\n")
    assert expected in dkm_str
    assert dkm._cma_operation == "assembly"


def test_cma_mdata_asm_vector_error():
    ''' Check that we raise the expected error if a kernel assembling a
    CMA operator has any vector arguments '''
    fparser.logging.disable('CRITICAL')
    # Change the space of the field that is written
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field,gh_read, any_space_1)",
        "arg_type(gh_field*3,gh_read, any_space_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has a "
            "vector argument (gh_field*3). This is forbidden.") in str(excinfo)
    code = CMA_ASSEMBLE.replace(
        "gh_columnwise_operator,", "gh_columnwise_operator*2,", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has a "
            "vector argument (gh_columnwise_operator*2). This is forbidden") \
        in str(excinfo)


def test_cma_mdata_asm_stencil_error():
    ''' Check that we raise the expected error if a kernel assembling a
    CMA operator specifies a stencil access on a field'''
    fparser.logging.disable('CRITICAL')
    # Change the space of the field that is written
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field,gh_read, any_space_1)",
        "arg_type(gh_field,gh_read,any_space_1,stencil(x1d))", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has an "
            "argument with a stencil access (x1d). This is forbidden.") \
        in str(excinfo)


CMA_APPLY = '''
module testkern_cma_apply
  type, extends(kernel_type) :: testkern_cma_type
  type(arg_type) :: meta_args(3) = (/                                      &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                        &
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2),                        &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2) &
       /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_cma_code
  end type testkern_cma_type
contains
  subroutine testkern_cma_code(a,b,c,d)
  end subroutine testkern_cma_code
end module testkern_cma_apply
'''


def test_cma_mdata_apply():
    ''' Check that we can parse meta-data entries relating to the
    application of Column-Matrix operators '''
    fparser.logging.disable('CRITICAL')
    code = CMA_APPLY
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'\n"
        "  access_descriptor[1]='gh_read'\n"
        "  function_space[2]='any_space_2'\n")
    print dkm_str
    assert expected in dkm_str
    dkm_str = str(dkm.arg_descriptors[2])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  access_descriptor[1]='gh_read'\n"
        "  function_space_to[2]='any_space_1'\n"
        "  function_space_from[3]='any_space_2'\n")
    assert expected in dkm_str
    assert dkm._cma_operation == "apply"


def test_cma_mdata_apply_too_many_ops():
    ''' Check that we raise the expected error if there are too-many
    CMA operators '''
    fparser.logging.disable('CRITICAL')
    # Add an additional read-only CMA operator to the argument list
    code = CMA_APPLY.replace(
        "ANY_SPACE_2),                        &\n",
        "ANY_SPACE_2),                        &\n"
        "       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2), &\n", 1)
    code = code.replace("meta_args(3)", "meta_args(4)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("a kernel that applies a CMA operator must only have one such "
            "operator in its list of arguments but found 2") in str(excinfo)


def test_cma_mdata_apply_too_many_flds():
    ''' Check that we raise the expected error if there are too-many
    field args to a kernel that applies a CMA operator '''
    fparser.logging.disable('CRITICAL')
    # Add an additional read-only field to the argument list
    code = CMA_APPLY.replace(
        "ANY_SPACE_2),                        &\n",
        "ANY_SPACE_2),                        &\n"
        "       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2), &\n", 1)
    code = code.replace("meta_args(3)", "meta_args(4)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("a kernel that applies a CMA operator must have 3 arguments "
            "(the operator and two fields) but kernel testkern_cma_type "
            "has 4") in str(excinfo)


def test_cma_mdata_apply_no_read_fld():
    ''' Check that we raise the expected error if there is no read-only
    field arg to a kernel that applies a CMA operator '''
    fparser.logging.disable('CRITICAL')
    # Make the read-only field gh_write instead
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2), ",
        "arg_type(GH_FIELD,    GH_WRITE, ANY_SPACE_2), ", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("has a read-only CMA operator. In order to apply it the kernel "
            "must have one read-only field argument") in str(excinfo)


def test_cma_mdata_apply_no_write_fld():
    ''' Check that we raise the expected error if there is no written
    field arg to a kernel that applies a CMA operator '''
    fparser.logging.disable('CRITICAL')
    # Turn the written field into an operator instead
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1), ",
        "arg_type(GH_OPERATOR, GH_WRITE, ANY_SPACE_1, ANY_SPACE_1), ", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("has a read-only CMA operator. In order to apply it the kernel "
            "must write to one field argument") in str(excinfo)


def test_cma_mdata_apply_wrong_spaces():
    ''' Check that we raise the expected error if the function spaces of the
    read and write fields do not match the from and to function spaces of the
    CMA operator '''
    fparser.logging.disable('CRITICAL')
    # Change the space of the field that is written
    code = CMA_APPLY.replace("arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1)",
                             "arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("applies a CMA operator but the function space of the field "
            "argument it writes to (any_space_3) does not match the 'to' "
            "space of the operator (any_space_1)") in str(excinfo)
    # Change the space of the field that is read
    code = CMA_APPLY.replace("arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2)",
                             "arg_type(GH_FIELD,    GH_READ, ANY_SPACE_3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("applies a CMA operator but the function space of the field "
            "argument it reads from (any_space_3) does not match the 'from' "
            "space of the operator (any_space_2)") in str(excinfo)


def test_cma_mdata_apply_vector_error():
    ''' Check that we raise the expected error if the meta-data for a kernel
    that applies a CMA operator contains a vector argument '''
    fparser.logging.disable('CRITICAL')
    code = CMA_APPLY.replace("arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1)",
                             "arg_type(GH_FIELD*3,  GH_INC,  ANY_SPACE_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has a "
            "vector argument (gh_field*3). This is forbidden.") in str(excinfo)
    code = CMA_APPLY.replace("GH_COLUMNWISE_OPERATOR,",
                             "GH_COLUMNWISE_OPERATOR*4,", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has a "
            "vector argument (gh_columnwise_operator*4). This is "
            "forbidden.") in str(excinfo)


def test_cma_mdata_apply_fld_stencil_error():
    ''' Check that we raise the expected error if the meta-data for a kernel
    that applies a CMA operator contains a field argument with a stencil
    access '''
    fparser.logging.disable('CRITICAL')
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2)",
        "arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2, STENCIL(X1D))", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has an "
            "argument with a stencil access (x1d). This is "
            "forbidden.") in str(excinfo)


CMA_MATRIX = '''
module testkern_cma_matrix_matrix
  type, extends(kernel_type) :: testkern_cma_type
  type(arg_type) :: meta_args(4) = (/                                      &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2),&
       arg_type(GH_REAL,                GH_READ),                          &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2),&
       arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE,ANY_SPACE_1, ANY_SPACE_2) &
       /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_cma_code
  end type testkern_cma_type
contains
  subroutine testkern_cma_code(a,b,c,d)
  end subroutine testkern_cma_code
end module testkern_cma_matrix_matrix
'''


def test_cma_mdata_matrix_prod():
    ''' Check that we can parse meta-data entries relating to a kernel
    that performs a product of two CMA operators '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[2])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  access_descriptor[1]='gh_read'\n"
        "  function_space_to[2]='any_space_1'\n"
        "  function_space_from[3]='any_space_2'\n")
    print dkm_str
    assert expected in dkm_str
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_too_few_args():
    ''' Check that we raise the expected error when there are too few
    arguments specified in meta-data '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2),&\n", "", 2)
    code = code.replace("meta_args(4)", "meta_args(2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("has a single column-wise operator argument but does not conform "
            "to the rules for an Assembly kernel because it does not have "
            "any read-only LMA operator arguments") in str(excinfo)


def test_cma_mdata_matrix_field_arg():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    reads from a field argument. Adding an argument that is not a CMA
    operator or scalar means that PSyclone attempts to identify this as an
    assembly kernel. '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2)", "arg_type(GH_FIELD, GH_READ, ANY_SPACE_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A column-wise matrix-matrix kernel must have only column-wise "
            "operators and scalars as arguments but kernel testkern_cma_type "
            "has: ['gh_field', ") in str(excinfo)


def test_cma_mdata_matrix_no_scalar_arg():
    ''' Check that we successfully parse meta-data for a matrix-matrix kernel
    that has no scalar arguments. '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_REAL,                GH_READ)",
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2)",
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = DynKernMetadata(ast, name=name)
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_2_scalar_args():
    ''' Check that we successfully parse meta-data for a matrix-matrix kernel
    that has 2 scalar arguments. '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2)",
        "arg_type(GH_REAL,                GH_READ)",
        1)
    print code
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = DynKernMetadata(ast, name=name)
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_2_writes():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    writes to more than one CMA operator '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2),&\n",
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2),&\n"
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE, ANY_SPACE_1, "
        "ANY_SPACE_2),&\n", 1)
    code = code.replace("meta_args(4)", "meta_args(5)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A Dynamo 0.3 kernel cannot update more than one CMA "
            "(column-wise) operator but kernel testkern_cma_type "
            "updates 2") in str(excinfo)


def test_cma_mdata_stencil_invalid():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    specifies a stencil '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE,ANY_SPACE_1, ANY_SPACE_2)",
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE,ANY_SPACE_1, "
        "stencil(cross))", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 4th argument of a meta_arg entry must be a valid function "
            "space") in str(excinfo)
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE,ANY_SPACE_1, ANY_SPACE_2)",
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE,ANY_SPACE_1, ANY_SPACE_2, "
        "stencil(cross))", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("each meta_arg entry must have 4 arguments if its first argument "
            "is gh_operator or gh_columnwise_operator") in str(excinfo)


def test_cma_mdata_matrix_vector_error():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    contains a vector argument '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_WRITE,ANY_SPACE_1, ANY_SPACE_2)",
        "arg_type(GH_COLUMNWISE_OPERATOR*3,GH_WRITE,ANY_SPACE_1,ANY_SPACE_2)",
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type takes a CMA operator but has a vector "
            "argument (gh_columnwise_operator*3)") in str(excinfo)


def test_cma_asm_cbanded_dofmap_error():
    ''' Check that we raise expected internal error if DynInvokeDofmaps
    encounters an assembly kernel that has more than one CMA op argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.0_cma_assembly.f90"),
        distributed_memory=True,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    calls = invoke.schedule.calls()
    # We must go in and make the internal state inconsistent in order
    # to trigger the error. So, we set the type of all the arguments
    # in the kernel cal to be CMA operators...
    for arg in calls[0].arguments.args:
        arg._type = 'gh_columnwise_operator'
    with pytest.raises(GenerationError) as excinfo:
        invoke.dofmaps.__init__(invoke.schedule)
    assert ("Internal error: there should only be one CMA operator argument "
            "for a CMA assembly kernel but found 2") in str(excinfo)


def test_cma_asm():
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.0_cma_assembly.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert ("USE operator_mod, ONLY: operator_type, operator_proxy_type, "
                "columnwise_operator_type, columnwise_operator_proxy_type") \
            in code
        assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
        assert "TYPE(columnwise_operator_type), intent(inout) :: cma_op1" \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "INTEGER ncell_2d" in code
        assert ("INTEGER, pointer :: cbanded_map_any_space_1_lma_op1(:,:) => "
                "null(), cbanded_map_any_space_2_lma_op1(:,:) => null()") \
            in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        assert ("CALL columnwise_op_asm_kernel_code(cell, nlayers, ncell_2d, "
                "lma_op1_proxy%ncell_3d, lma_op1_proxy%local_stencil, "
                "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, "
                "cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
                "cma_op1_gamma_m, cma_op1_gamma_p, ndf_any_space_1_lma_op1, "
                "cbanded_map_any_space_1_lma_op1, ndf_any_space_2_lma_op1, "
                "cbanded_map_any_space_2_lma_op1)") in code


def test_cma_asm_field():
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator with a field as argument '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.3_cma_assembly_field.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert ("USE operator_mod, ONLY: operator_type, operator_proxy_type, "
                "columnwise_operator_type, columnwise_operator_proxy_type") \
            in code
        assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
        assert "TYPE(columnwise_operator_type), intent(inout) :: cma_op1" \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert ("INTEGER, pointer :: cbanded_map_any_space_1_afield(:,:) => "
                "null(), cbanded_map_any_space_2_lma_op1(:,:) => null()") \
            in code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        expected = ("CALL columnwise_op_asm_field_kernel_code(cell, "
                    "nlayers, ncell_2d, "
                    "afield_proxy%data, lma_op1_proxy%ncell_3d, "
                    "lma_op1_proxy%local_stencil, "
                    "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, "
                    "cma_op1_bandwidth, cma_op1_alpha, "
                    "cma_op1_beta, cma_op1_gamma_m, "
                    "cma_op1_gamma_p, ndf_any_space_1_afield, "
                    "undf_any_space_1_afield, "
                    "map_any_space_1_afield(:,cell), "
                    "cbanded_map_any_space_1_afield, "
                    "ndf_any_space_2_lma_op1, "
                    "cbanded_map_any_space_2_lma_op1)")
        print expected
        assert expected in code


def test_cma_asm_scalar():
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator with a scalar as argument. The
    name of the scalar is deliberately chosen to provoke a clash with the
    name generated for one of the CMA parameters.'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.0.1_cma_assembly_scalar.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert ("USE operator_mod, ONLY: operator_type, operator_proxy_type, "
                "columnwise_operator_type, columnwise_operator_proxy_type") \
            in code
        assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
        assert "TYPE(columnwise_operator_type), intent(inout) :: cma_op1" \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert ("INTEGER, pointer :: cbanded_map_any_space_1_lma_op1(:,:) => "
                "null(), cbanded_map_any_space_2_lma_op1(:,:) => null()") \
            in code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        expected = ("CALL columnwise_op_asm_kernel_code(cell, "
                    "nlayers, ncell_2d, lma_op1_proxy%ncell_3d, "
                    "lma_op1_proxy%local_stencil, "
                    "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, "
                    "cma_op1_bandwidth, cma_op1_alpha_1, "
                    "cma_op1_beta, cma_op1_gamma_m, "
                    "cma_op1_gamma_p, cma_op1_alpha, "
                    "ndf_any_space_1_lma_op1, "
                    "cbanded_map_any_space_1_lma_op1, "
                    "ndf_any_space_2_lma_op1, "
                    "cbanded_map_any_space_2_lma_op1)")
        print expected
        assert expected in code


def test_cma_asm_field_same_fs():
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator with a field as argument.
    In this case the fs_from and fs_to spaces of the operator are the
    same so we only need to pass in one banded dofmap. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.4_cma_assembly_field_same_fs.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert ("USE operator_mod, ONLY: operator_type, operator_proxy_type, "
                "columnwise_operator_type, columnwise_operator_proxy_type") \
            in code
        assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
        assert "TYPE(columnwise_operator_type), intent(inout) :: cma_op1" \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert ("INTEGER, pointer :: cbanded_map_any_space_2_lma_op1(:,:) => "
                "null()\n") in code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        if distmem:
            # When distributed-memory is enabled then we compute operators
            # redundantly (out to the L1 halo)
            assert "DO cell=1,mesh%get_last_halo_cell(1)\n" in code
        else:
            assert "DO cell=1,cma_op1_proxy%fs_from%get_ncell()\n" in code
        expected = ("CALL columnwise_op_asm_same_fs_kernel_code(cell, "
                    "nlayers, ncell_2d, "
                    "lma_op1_proxy%ncell_3d, "
                    "lma_op1_proxy%local_stencil, afield_proxy%data, "
                    "cma_op1_matrix, cma_op1_nrow, "
                    "cma_op1_bandwidth, cma_op1_alpha, "
                    "cma_op1_beta, cma_op1_gamma_m, "
                    "cma_op1_gamma_p, ndf_any_space_1_lma_op1, "
                    "undf_any_space_1_lma_op1, "
                    "map_any_space_1_lma_op1(:,cell), "
                    "ndf_any_space_2_lma_op1, "
                    "cbanded_map_any_space_2_lma_op1)")
        assert expected in code
        # We do not perform halo swaps for operators
        assert "lma_op1_proxy%is_dirty(" not in code
        assert "cma_op1_proxy%is_dirty(" not in code


def test_cma_apply_indirection_dofmap_error():
    ''' Check that we raise expected internal error if DynInvokeDofmaps
    encounters an apply kernel that has more than one CMA op argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.1_cma_apply.f90"),
        distributed_memory=True,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    calls = invoke.schedule.calls()
    # We must go in and make the internal state inconsistent in order
    # to trigger the error. So, we set the type of all the arguments
    # in the kernel cal to be CMA operators...
    for arg in calls[0].arguments.args:
        arg._type = 'gh_columnwise_operator'
    with pytest.raises(GenerationError) as excinfo:
        invoke.dofmaps.__init__(invoke.schedule)
    assert ("Internal error: there should only be one CMA "
            "operator argument for a kernel that applies a "
            "CMA operator but found 3") in str(excinfo)


def test_cma_apply():
    ''' Test that we generate correct code for
    a kernel that applies a CMA operator '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.1_cma_apply.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert "INTEGER ncell_2d" in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert ("INTEGER, pointer :: cma_indirection_map_any_space_1_"
                "field_a(:) => null(), "
                "cma_indirection_map_any_space_2_field_b(:) => null()\n") \
            in code
        assert ("ndf_any_space_1_field_a = field_a_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_field_a = field_a_proxy%vspace%"
                "get_undf()") in code
        assert ("cma_indirection_map_any_space_1_field_a => "
                "cma_op1_proxy%indirection_dofmap_to") in code
        assert ("cma_indirection_map_any_space_2_field_b => "
                "cma_op1_proxy%indirection_dofmap_from") in code
        assert ("CALL columnwise_op_app_kernel_code(cell, ncell_2d, "
                "field_a_proxy%data, field_b_proxy%data, "
                "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, "
                "cma_op1_bandwidth, cma_op1_alpha, "
                "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
                "ndf_any_space_1_field_a, undf_any_space_1_field_a, "
                "map_any_space_1_field_a(:,cell), "
                "cma_indirection_map_any_space_1_field_a, "
                "ndf_any_space_2_field_b, undf_any_space_2_field_b, "
                "map_any_space_2_field_b(:,cell), "
                "cma_indirection_map_any_space_2_field_b)") \
            in code
        # We do not perform halo swaps for operators
        assert "cma_op1_proxy%is_dirty(" not in code


def test_cma_apply_discontinuous_spaces(tmpdir, f90, f90flags):
    ''' Test that we generate correct code for a kernel that applies
    a CMA operator to fields on discontinuous spaces w3 and w2v '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.1.2_cma_apply_disc.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code

        if utils.TEST_COMPILE:
            # If compilation testing has been enabled
            # (--compile --f90="<compiler_name>" flags to py.test)
            assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        # Check w3
        assert "INTEGER ncell_2d" in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert ("INTEGER, pointer :: cma_indirection_map_w3(:) "
                "=> null(), cma_indirection_map_any_space_1_field_b(:) => "
                "null()\n") in code
        assert ("ndf_w3 = field_a_proxy%vspace%get_ndf()\n"
                "      undf_w3 = field_a_proxy%vspace%"
                "get_undf()") in code
        assert ("cma_indirection_map_w3 => "
                "cma_op1_proxy%indirection_dofmap_to") in code
        # Check w2v
        assert "TYPE(columnwise_operator_proxy_type) cma_op2_proxy" in code
        assert "ncell_2d = cma_op2_proxy%ncell_2d" in code
        assert ("INTEGER, pointer :: "
                "cma_indirection_map_any_space_2_field_d(:) => null(), "
                "cma_indirection_map_w2v(:) => null()\n") in code
        assert ("ndf_w2v = field_c_proxy%vspace%get_ndf()\n"
                "      undf_w2v = field_c_proxy%vspace%"
                "get_undf()") in code
        assert ("cma_indirection_map_w2v => "
                "cma_op2_proxy%indirection_dofmap_to") in code
        if distmem:
            # The kernel only *reads* from a CMA operator and writes to a
            # field on a discontinuous space - therefore we do not need to
            # loop out into the L1 halo.
            assert code.count("DO cell=1,mesh%get_last_edge_cell()") == 2
        else:
            assert "DO cell=1,field_a_proxy%vspace%get_ncell()" in code
            assert "DO cell=1,field_c_proxy%vspace%get_ncell()" in code

        # Check w3
        assert ("CALL columnwise_op_app_w3_kernel_code(cell, ncell_2d, "
                "field_a_proxy%data, field_b_proxy%data, "
                "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, "
                "cma_op1_bandwidth, cma_op1_alpha, "
                "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
                "ndf_w3, undf_w3, map_w3(:,cell), cma_indirection_map_w3, "
                "ndf_any_space_1_field_b, undf_any_space_1_field_b, "
                "map_any_space_1_field_b(:,cell), "
                "cma_indirection_map_any_space_1_field_b)") \
            in code
        # Check w2v
        assert ("CALL columnwise_op_app_w2v_kernel_code(cell, ncell_2d, "
                "field_c_proxy%data, field_d_proxy%data, "
                "cma_op2_matrix, cma_op2_nrow, cma_op2_ncol, "
                "cma_op2_bandwidth, cma_op2_alpha, "
                "cma_op2_beta, cma_op2_gamma_m, cma_op2_gamma_p, "
                "ndf_w2v, undf_w2v, map_w2v(:,cell), cma_indirection_map_w2v, "
                "ndf_any_space_2_field_d, undf_any_space_2_field_d, "
                "map_any_space_2_field_d(:,cell), "
                "cma_indirection_map_any_space_2_field_d)") \
            in code

        if distmem:
            # Check w3
            assert "CALL field_a_proxy%set_dirty()" in code
            assert "cma_op1_proxy%is_dirty(" not in code
            # Check w2v
            assert "CALL field_c_proxy%set_dirty()" in code
            assert "cma_op2_proxy%is_dirty(" not in code


def test_cma_apply_same_space():
    ''' Test that we generate correct code for
    a kernel that applies a CMA operator which has the same to- and from-
    spaces '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.1.1_cma_apply_same_spaces.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert "INTEGER ncell_2d" in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert ("INTEGER, pointer :: cma_indirection_map_any_space_2_"
                "field_a(:) => null()\n") in code
        assert ("ndf_any_space_2_field_a = field_a_proxy%vspace%get_ndf()\n"
                "      undf_any_space_2_field_a = field_a_proxy%vspace%"
                "get_undf()") in code
        assert ("cma_indirection_map_any_space_2_field_a => "
                "cma_op1_proxy%indirection_dofmap_to") in code
        assert ("CALL columnwise_op_app_same_fs_kernel_code(cell, ncell_2d, "
                "field_a_proxy%data, field_b_proxy%data, "
                "cma_op1_matrix, cma_op1_nrow, "
                "cma_op1_bandwidth, cma_op1_alpha, "
                "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
                "ndf_any_space_2_field_a, undf_any_space_2_field_a, "
                "map_any_space_2_field_a(:,cell), "
                "cma_indirection_map_any_space_2_field_a)") \
            in code
        if distmem:
            assert "CALL field_a_proxy%set_dirty()" in code
            assert "cma_op1_proxy%is_dirty(" not in code


def test_cma_matrix_matrix():
    ''' Test that we generate correct code for an invoke containing
    a kernel that performs a matrix-matrix CMA calculation '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.2_cma_matrix_matrix.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_opc_proxy%ncell_2d" in code
        if distmem:
            # When distributed-memory is enabled then we compute operators
            # redundantly (out to the L1 halo)
            assert "DO cell=1,mesh%get_last_halo_cell(1)\n" in code
        else:
            assert "DO cell=1,cma_opc_proxy%fs_from%get_ncell()\n" in code

        assert ("CALL columnwise_op_mul_kernel_code(cell, "
                "ncell_2d, "
                "cma_opa_matrix, cma_opa_nrow, cma_opa_ncol, "
                "cma_opa_bandwidth, cma_opa_alpha, "
                "cma_opa_beta, cma_opa_gamma_m, cma_opa_gamma_p, "
                "cma_opb_matrix, cma_opb_nrow, cma_opb_ncol, "
                "cma_opb_bandwidth, cma_opb_alpha, "
                "cma_opb_beta, cma_opb_gamma_m, cma_opb_gamma_p, "
                "cma_opc_matrix, cma_opc_nrow, cma_opc_ncol, "
                "cma_opc_bandwidth, cma_opc_alpha, "
                "cma_opc_beta, cma_opc_gamma_m, cma_opc_gamma_p)") in code
        if distmem:
            assert "_dirty(" not in code


def test_cma_matrix_matrix_2scalars():
    ''' Test that we generate correct code for an invoke containing
    a kernel that performs a matrix-matrix CMA calculation including
    scalar arguments. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.2.1_cma_matrix_matrix.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_opc_proxy%ncell_2d" in code
        if distmem:
            # When distributed-memory is enabled then we compute operators
            # redundantly (out to the L1 halo)
            assert "DO cell=1,mesh%get_last_halo_cell(1)\n" in code
        else:
            assert "DO cell=1,cma_opc_proxy%fs_from%get_ncell()\n" in code

        assert ("CALL columnwise_op_mul_kernel_code(cell, "
                "ncell_2d, "
                "cma_opa_matrix, cma_opa_nrow, cma_opa_ncol, "
                "cma_opa_bandwidth, cma_opa_alpha, "
                "cma_opa_beta, cma_opa_gamma_m, cma_opa_gamma_p, "
                "alpha, "
                "cma_opb_matrix, cma_opb_nrow, cma_opb_ncol, "
                "cma_opb_bandwidth, cma_opb_alpha, "
                "cma_opb_beta, cma_opb_gamma_m, cma_opb_gamma_p, "
                "beta, "
                "cma_opc_matrix, cma_opc_nrow, cma_opc_ncol, "
                "cma_opc_bandwidth, cma_opc_alpha, "
                "cma_opc_beta, cma_opc_gamma_m, cma_opc_gamma_p)") in code
        if distmem:
            assert "_dirty(" not in code


def test_cma_multi_kernel():
    ''' Test that we generate correct code when an invoke contains multiple
    kernels with CMA operator arguments '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "20.5_multi_cma_invoke.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        assert ("      afield_proxy = afield%get_proxy()\n"
                "      lma_op1_proxy = lma_op1%get_proxy()\n"
                "      cma_op1_proxy = cma_op1%get_proxy()\n"
                "      field_a_proxy = field_a%get_proxy()\n"
                "      field_b_proxy = field_b%get_proxy()\n"
                "      cma_opb_proxy = cma_opb%get_proxy()\n"
                "      cma_opc_proxy = cma_opc%get_proxy()\n") in code

        assert "cma_op1_matrix => cma_op1_proxy%columnwise_matrix\n" in code
        assert "cma_op1_ncol = cma_op1_proxy%ncol\n" in code
        assert "cma_op1_nrow = cma_op1_proxy%nrow\n" in code
        assert "cma_op1_bandwidth = cma_op1_proxy%bandwidth\n" in code
        assert "cma_op1_alpha = cma_op1_proxy%alpha\n" in code
        assert "cma_op1_beta = cma_op1_proxy%beta\n" in code

        assert ("      cbanded_map_any_space_1_afield => "
                "cma_op1_proxy%column_banded_dofmap_to\n"
                "      cbanded_map_any_space_2_lma_op1 => "
                "cma_op1_proxy%column_banded_dofmap_from\n") in code
        assert ("cma_indirection_map_any_space_1_field_a => "
                "cma_op1_proxy%indirection_dofmap_to\n"
                "      cma_indirection_map_any_space_2_field_b => "
                "cma_op1_proxy%indirection_dofmap_from\n") in code

        if distmem:
            # When distributed-memory is enabled then we compute operators
            # redundantly (out to the L1 halo). Since the field that the
            # CMA operator is applied to is on any-space, we must assume
            # the worst and also loop out to L1 for it too.
            assert code.count("DO cell=1,mesh%get_last_halo_cell(1)\n") == 3
        else:
            assert "DO cell=1,cma_op1_proxy%fs_from%get_ncell()\n" in code
            assert "DO cell=1,field_a_proxy%vspace%get_ncell()\n" in code
            assert "DO cell=1,cma_opc_proxy%fs_from%get_ncell()\n" in code

        assert ("CALL columnwise_op_asm_field_kernel_code(cell, nlayers, "
                "ncell_2d, afield_proxy%data, lma_op1_proxy%ncell_3d, "
                "lma_op1_proxy%local_stencil, cma_op1_matrix, cma_op1_nrow, "
                "cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, "
                "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
                "ndf_any_space_1_afield, undf_any_space_1_afield, "
                "map_any_space_1_afield(:,cell), "
                "cbanded_map_any_space_1_afield, ndf_any_space_2_lma_op1, "
                "cbanded_map_any_space_2_lma_op1)") in code
        assert ("CALL columnwise_op_app_kernel_code(cell, ncell_2d, "
                "field_a_proxy%data, field_b_proxy%data, cma_op1_matrix, "
                "cma_op1_nrow, cma_op1_ncol, cma_op1_bandwidth, "
                "cma_op1_alpha, cma_op1_beta, cma_op1_gamma_m, "
                "cma_op1_gamma_p, ndf_any_space_1_field_a, "
                "undf_any_space_1_field_a, map_any_space_1_field_a(:,cell), "
                "cma_indirection_map_any_space_1_field_a, "
                "ndf_any_space_2_field_b, undf_any_space_2_field_b, "
                "map_any_space_2_field_b(:,cell), "
                "cma_indirection_map_any_space_2_field_b)\n") in code
        assert ("CALL columnwise_op_mul_kernel_code(cell, ncell_2d, "
                "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, "
                "cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
                "cma_op1_gamma_m, cma_op1_gamma_p, cma_opb_matrix, "
                "cma_opb_nrow, cma_opb_ncol, cma_opb_bandwidth, "
                "cma_opb_alpha, cma_opb_beta, cma_opb_gamma_m, "
                "cma_opb_gamma_p, cma_opc_matrix, cma_opc_nrow, cma_opc_ncol, "
                "cma_opc_bandwidth, cma_opc_alpha, cma_opc_beta, "
                "cma_opc_gamma_m, cma_opc_gamma_p)") in code

# Tests for the kernel-stub generator


def test_cma_asm_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_asm_kernel_mod.F90"),
                      api="dynamo0.3")
    print str(result)
    expected = (
        "  MODULE columnwise_op_asm_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_kernel_code(cell, nlayers, "
        "ncell_2d, op_1_ncell_3d, op_1, cma_op_2, cma_op_2_nrow, "
        "cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, "
        "cma_op_2_gamma_m, cma_op_2_gamma_p, ndf_any_space_1_op_1, "
        "cbanded_map_any_space_1_op_1, ndf_any_space_2_op_1, "
        "cbanded_map_any_space_2_op_1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_op_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_op_1\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_op_1,"
        "ndf_any_space_2_op_1,op_1_ncell_3d) :: op_1\n"
        "      INTEGER, intent(in) :: cma_op_2_nrow, cma_op_2_ncol, "
        "cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, cma_op_2_gamma_m,"
        " cma_op_2_gamma_p\n"
        "      REAL(KIND=r_def), intent(out), dimension(cma_op_2_bandwidth,"
        "cma_op_2_nrow,ncell_2d) :: cma_op_2\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_op_1,"
        "nlayers) :: cbanded_map_any_space_1_op_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_op_1,"
        "nlayers) :: cbanded_map_any_space_2_op_1\n"
        "    END SUBROUTINE columnwise_op_asm_kernel_code\n"
        "  END MODULE columnwise_op_asm_kernel_mod")
    assert expected in str(result)


def test_cma_asm_with_field_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly when a
    field is involved '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_asm_field_kernel_mod.F90"),
                      api="dynamo0.3")
    print str(result)
    expected = (
        "  MODULE columnwise_op_asm_field_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_field_kernel_code(cell, nlayers, "
        "ncell_2d, field_1_any_space_1_field_1, op_2_ncell_3d, op_2, "
        "cma_op_3, cma_op_3_nrow, "
        "cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p, ndf_any_space_1_field_1, "
        "undf_any_space_1_field_1, map_any_space_1_field_1, "
        "cbanded_map_any_space_1_field_1, "
        "ndf_any_space_2_op_2, cbanded_map_any_space_2_op_2)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: undf_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_op_2\n"
        "      REAL(KIND=r_def), intent(in), dimension("
        "undf_any_space_1_field_1) :: field_1_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension("
        "ndf_any_space_1_field_1,ndf_any_space_2_op_2,op_2_ncell_3d) :: op_2\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m,"
        " cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(out), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1) :: "
        "map_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1,"
        "nlayers) :: cbanded_map_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_op_2,"
        "nlayers) :: cbanded_map_any_space_2_op_2\n"
        "    END SUBROUTINE columnwise_op_asm_field_kernel_code\n"
        "  END MODULE columnwise_op_asm_field_kernel_mod")
    assert expected in str(result)


def test_cma_asm_same_fs_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly when the
    to and from spaces are the same '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_asm_same_fs_kernel_mod.F90"),
                      api="dynamo0.3")
    print str(result)
    expected = (
        "  MODULE columnwise_op_asm_same_fs_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_same_fs_kernel_code(cell, nlayers, "
        "ncell_2d, op_1_ncell_3d, op_1, field_2_any_space_1_op_1, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p, ndf_any_space_1_op_1, "
        "undf_any_space_1_op_1, map_any_space_1_op_1, ndf_any_space_2_op_1, "
        "cbanded_map_any_space_2_op_1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_op_1\n"
        "      INTEGER, intent(in) :: undf_any_space_1_op_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_op_1\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_op_1,"
        "ndf_any_space_2_op_1,op_1_ncell_3d) :: op_1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_any_space_1_op_1) "
        ":: field_2_any_space_1_op_1\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_bandwidth, "
        "cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(out), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_op_1) :: "
        "map_any_space_1_op_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_op_1,nlayers) "
        ":: cbanded_map_any_space_2_op_1\n")
    assert expected in str(result)


def test_cma_app_stub_gen():
    ''' Test the kernel-stub generator for a CMA apply kernel. This has
    two fields and one CMA operator as arguments. '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_app_kernel_mod.F90"),
                      api="dynamo0.3")
    print result
    expected = (
        "  MODULE columnwise_op_app_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_app_kernel_code(cell, ncell_2d, "
        "field_1_any_space_1_field_1, field_2_any_space_2_field_2, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, "
        "cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "ndf_any_space_1_field_1, undf_any_space_1_field_1, "
        "map_any_space_1_field_1, cma_indirection_map_any_space_1_field_1, "
        "ndf_any_space_2_field_2, undf_any_space_2_field_2, "
        "map_any_space_2_field_2, cma_indirection_map_any_space_2_field_2)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: undf_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_field_2\n"
        "      INTEGER, intent(in) :: undf_any_space_2_field_2\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_any_space_1_field_1) :: field_1_any_space_1_field_1\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(undf_any_space_2_field_2) :: field_2_any_space_2_field_2\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1) :: "
        "map_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(cma_op_3_nrow) :: "
        "cma_indirection_map_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_field_2) :: "
        "map_any_space_2_field_2\n"
        "      INTEGER, intent(in), dimension(cma_op_3_ncol) :: "
        "cma_indirection_map_any_space_2_field_2\n"
        "    END SUBROUTINE columnwise_op_app_kernel_code\n"
        "  END MODULE columnwise_op_app_kernel_mod")
    assert expected in str(result)


def test_cma_app_same_space_stub_gen():
    ''' Test the kernel-stub generator for a CMA apply kernel where the
    to/from function spaces of the CMA operator are the same. This kernel has
    two fields and one CMA operator as arguments. '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_app_same_fs_kernel_mod.F90"),
                      api="dynamo0.3")
    print result
    expected = (
        "  MODULE columnwise_op_app_same_fs_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_app_same_fs_kernel_code(cell, ncell_2d, "
        "field_1_any_space_2_field_1, field_2_any_space_2_field_1, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_bandwidth, cma_op_3_alpha, "
        "cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "ndf_any_space_2_field_1, undf_any_space_2_field_1, "
        "map_any_space_2_field_1, "
        "cma_indirection_map_any_space_2_field_1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_field_1\n"
        "      INTEGER, intent(in) :: undf_any_space_2_field_1\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_any_space_2_field_1) :: field_1_any_space_2_field_1\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(undf_any_space_2_field_1) :: field_2_any_space_2_field_1\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_field_1) :: "
        "map_any_space_2_field_1\n"
        "      INTEGER, intent(in), dimension(cma_op_3_nrow) :: "
        "cma_indirection_map_any_space_2_field_1\n"
        "    END SUBROUTINE columnwise_op_app_same_fs_kernel_code\n"
        "  END MODULE columnwise_op_app_same_fs_kernel_mod")
    assert expected in str(result)


def test_cma_mul_stub_gen():
    ''' Test the kernel-stub generator for a CMA matrix-matrix kernel '''
    result = generate(os.path.join(BASE_PATH, "columnwise_op_mul_kernel.F90"),
                      api="dynamo0.3")
    print result
    expected = (
        "  MODULE columnwise_op_mul_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_mul_kernel_code(cell, ncell_2d, "
        "cma_op_1, cma_op_1_nrow, cma_op_1_ncol, cma_op_1_bandwidth, "
        "cma_op_1_alpha, cma_op_1_beta, cma_op_1_gamma_m, cma_op_1_gamma_p, "
        "cma_op_2, cma_op_2_nrow, cma_op_2_ncol, cma_op_2_bandwidth, "
        "cma_op_2_alpha, cma_op_2_beta, cma_op_2_gamma_m, cma_op_2_gamma_p, "
        "cma_op_3, cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, "
        "cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: cma_op_1_nrow, cma_op_1_ncol, "
        "cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, "
        "cma_op_1_gamma_m, cma_op_1_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_1_bandwidth,"
        "cma_op_1_nrow,ncell_2d) :: cma_op_1\n"
        "      INTEGER, intent(in) :: cma_op_2_nrow, cma_op_2_ncol, "
        "cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, "
        "cma_op_2_gamma_m, cma_op_2_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_2_bandwidth,"
        "cma_op_2_nrow,ncell_2d) :: cma_op_2\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(inout), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "    END SUBROUTINE columnwise_op_mul_kernel_code\n"
        "  END MODULE columnwise_op_mul_kernel_mod")
    assert expected in str(result)


def test_cma_mul_with_scalars_stub_gen():
    ''' Test the kernel-stub generator for a CMA matrix-matrix kernel that
    includes scalar arguments '''
    result = generate(
        os.path.join(BASE_PATH, "columnwise_op_mul_2scalars_kernel.F90"),
        api="dynamo0.3")
    print result
    expected = (
        "  MODULE columnwise_op_mul_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_mul_kernel_code(cell, ncell_2d, "
        "cma_op_1, cma_op_1_nrow, cma_op_1_ncol, cma_op_1_bandwidth, "
        "cma_op_1_alpha, cma_op_1_beta, cma_op_1_gamma_m, cma_op_1_gamma_p, "
        "rscalar_2, "
        "cma_op_3, cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, "
        "cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "rscalar_4, "
        "cma_op_5, cma_op_5_nrow, cma_op_5_ncol, cma_op_5_bandwidth, "
        "cma_op_5_alpha, cma_op_5_beta, cma_op_5_gamma_m, cma_op_5_gamma_p)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: cma_op_1_nrow, cma_op_1_ncol, "
        "cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, "
        "cma_op_1_gamma_m, cma_op_1_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_1_bandwidth,"
        "cma_op_1_nrow,ncell_2d) :: cma_op_1\n"
        "      REAL(KIND=r_def), intent(in) :: rscalar_2\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      REAL(KIND=r_def), intent(in) :: rscalar_4\n"
        "      INTEGER, intent(in) :: cma_op_5_nrow, cma_op_5_ncol, "
        "cma_op_5_bandwidth, cma_op_5_alpha, cma_op_5_beta, "
        "cma_op_5_gamma_m, cma_op_5_gamma_p\n"
        "      REAL(KIND=r_def), intent(inout), dimension(cma_op_5_bandwidth,"
        "cma_op_5_nrow,ncell_2d) :: cma_op_5\n"
        "    END SUBROUTINE columnwise_op_mul_kernel_code\n"
        "  END MODULE columnwise_op_mul_kernel_mod")
    assert expected in str(result)
