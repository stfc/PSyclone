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

''' This module tests the support for Column-Matrix-Assembly operators in
the Dynamo 0.3 API using pytest. '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access


import os
import pytest
import fparser
from fparser import api as fpapi
from parse import ParseError, parse
from dynamo0p3 import DynKernMetadata
from psyGen import PSyFactory, GenerationError
from genkernelstub import generate

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


def test_cma_mdata_assembly_missing_op():  # pylint: disable=invalid-name
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
    assert ("Kernel testkern_cma_type writes to a column-wise operator but "
            "does not conform to the rules for matrix-matrix (it has "
            "arguments other than CMA operators) or for assembly (it does "
            "not have any read-only LMA operator arguments) kernels") in \
        str(excinfo)


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
            "does not conform to the rules for matrix-matrix (it has arguments"
            " other than CMA operators) or for assembly (it does not have any "
            "read-only LMA operator arguments) kernels") in \
        str(excinfo)


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
    assert ("Kernel testkern_cma_type assembles a column-wise operator but "
            "also writes to ['gh_operator'] argument(s). This is not "
            "allowed.") in str(excinfo)


def test_cma_mdata_assembly_diff_spaces():  # pylint: disable=invalid-name
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


def test_cma_mdata_asm_vector_error():  # pylint: disable=invalid-name
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


def test_cma_mdata_asm_stencil_error():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_too_many_ops():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_too_many_flds():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_no_read_fld():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_no_write_fld():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_wrong_spaces():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_vector_error():  # pylint: disable=invalid-name
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


def test_cma_mdata_apply_fld_stencil_error():  # pylint: disable=invalid-name
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
  type(arg_type) :: meta_args(3) = (/                                      &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2),&
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
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  access_descriptor[1]='gh_read'\n"
        "  function_space_to[2]='any_space_1'\n"
        "  function_space_from[3]='any_space_2'\n")
    print dkm_str
    assert expected in dkm_str
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_too_few_args():  # pylint: disable=invalid-name
    ''' Check that we raise the expected error when there are too few
    arguments specified in meta-data '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2),&\n", "", 2)
    code = code.replace("meta_args(3)", "meta_args(1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("writes to a single CMA operator but has no other arguments. "
            "It is therefore not a valid assembly or matrix-matrix") in \
        str(excinfo)


def test_cma_mdata_matrix_field_arg():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    reads from a field argument. Adding an argument that is not a CMA
    operator means that PSyclone attempts to identify this as an assembly
    kernel. '''
    fparser.logging.disable('CRITICAL')
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2)", "arg_type(GH_FIELD, GH_READ, ANY_SPACE_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("assembles a CMA operator and therefore should only have one "
            "CMA operator argument but found 2") in str(excinfo)


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
    code = code.replace("meta_args(3)", "meta_args(4)", 1)
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
        assert ("TYPE(columnwise_operator_type), intent(inout) :: cma_op1") \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        assert ("CALL columnwise_op_asm_kernel_code(cell, nlayers, ncell_2d, "
                "lma_op1_proxy%ncell_3d, lma_op1_proxy%local_stencil, "
                "cma_op1_proxy%columnwise_matrix, "
                "cma_op1_proxy%nrow, cma_op1_proxy%ncol, "
                "cma_op1_proxy%bandwidth, cma_op1_proxy%alpha, "
                "cma_op1_proxy%beta, cma_op1_proxy%gamma_m, "
                "cma_op1_proxy%gamma_p, ndf_any_space_1_lma_op1, "
                "ndf_any_space_2_lma_op1, "
                "cma_op1_proxy%column_banded_dofmap_to, "
                "cma_op1_proxy%column_banded_dofmap_from)") in code


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
        assert ("TYPE(columnwise_operator_type), intent(inout) :: cma_op1") \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        expected =  ("CALL columnwise_op_asm_field_kernel_code(cell, "
                     "nlayers, ncell_2d, "
                     "afield_proxy%data, lma_op1_proxy%ncell_3d, "
                     "lma_op1_proxy%local_stencil, "
                     "cma_op1_proxy%columnwise_matrix, "
                     "cma_op1_proxy%nrow, cma_op1_proxy%ncol, "
                     "cma_op1_proxy%bandwidth, cma_op1_proxy%alpha, "
                     "cma_op1_proxy%beta, cma_op1_proxy%gamma_m, "
                     "cma_op1_proxy%gamma_p, ndf_any_space_1_afield, "
                     "undf_any_space_1_afield, map_any_space_1_afield(:,cell), "
                     "ndf_any_space_2_lma_op1, "
                     "cma_op1_proxy%column_banded_dofmap_to, "
                     "cma_op1_proxy%column_banded_dofmap_from)")
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
        assert ("TYPE(columnwise_operator_type), intent(inout) :: cma_op1") \
            in code
        assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
        assert "INTEGER ncell_2d" in code
        assert "ncell_2d = cma_op1_proxy%ncell_2d" in code
        assert "cma_op1_proxy = cma_op1%get_proxy()" in code
        expected =  ("CALL columnwise_op_asm_same_fs_kernel_code(cell, "
                     "nlayers, ncell_2d, "
                     "lma_op1_proxy%ncell_3d, "
                     "lma_op1_proxy%local_stencil, afield_proxy%data, "
                     "cma_op1_proxy%columnwise_matrix, "
                     "cma_op1_proxy%nrow, "
                     "cma_op1_proxy%bandwidth, cma_op1_proxy%alpha, "
                     "cma_op1_proxy%beta, cma_op1_proxy%gamma_m, "
                     "cma_op1_proxy%gamma_p, ndf_any_space_1_lma_op1, "
                     "undf_any_space_1_lma_op1, "
                     "map_any_space_1_lma_op1(:,cell), "
                     "ndf_any_space_2_lma_op1, "
                     "cma_op1_proxy%column_banded_dofmap_to)")
        print expected
        assert expected in code


def test_cma_apply():
    ''' Test that we generate correct code for an invoke containing
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
        assert ("ndf_any_space_1_field_a = field_a_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_field_a = field_a_proxy%vspace%"
                "get_undf()") in code

        assert ("CALL columnwise_op_app_kernel_code(cell, ncell_2d, "
                "field_a_proxy%data, field_b_proxy%data, "
                "cma_op1_proxy%columnwise_matrix, "
                "cma_op1_proxy%nrow, cma_op1_proxy%ncol, "
                "cma_op1_proxy%bandwidth, cma_op1_proxy%alpha, "
                "cma_op1_proxy%beta, cma_op1_proxy%gamma_m, "
                "cma_op1_proxy%gamma_p, "
                "ndf_any_space_1_field_a, undf_any_space_1_field_a, "
                "map_any_space_1_field_a(:,cell), "
                "ndf_any_space_2_field_b, undf_any_space_2_field_b, "
                "map_any_space_2_field_b(:,cell), "
                "cma_op1_proxy%indirection_dofmap_to, "
                "cma_op1_proxy%indirection_dofmap_from)") \
            in code


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
        assert ("CALL columnwise_op_mul_kernel_code(cell, "
                "ncell_2d, "
                "cma_opa_proxy%columnwise_matrix, "
                "cma_opa_proxy%nrow, cma_opa_proxy%ncol, "
                "cma_opa_proxy%bandwidth, cma_opa_proxy%alpha, "
                "cma_opa_proxy%beta, cma_opa_proxy%gamma_m, "
                "cma_opa_proxy%gamma_p, "
                "cma_opb_proxy%columnwise_matrix, "
                "cma_opb_proxy%nrow, cma_opb_proxy%ncol, "
                "cma_opb_proxy%bandwidth, cma_opb_proxy%alpha, "
                "cma_opb_proxy%beta, cma_opb_proxy%gamma_m, "
                "cma_opb_proxy%gamma_p, "
                "cma_opc_proxy%columnwise_matrix, "
                "cma_opc_proxy%nrow, cma_opc_proxy%ncol, "
                "cma_opc_proxy%bandwidth, cma_opc_proxy%alpha, "
                "cma_opc_proxy%beta, cma_opc_proxy%gamma_m, "
                "cma_opc_proxy%gamma_p)") \
            in code


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
        assert 0


# Tests for the kernel-stub generator


def test_cma_asm_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly '''
    result = generate("test_files/dynamo0p3/columnwise_op_asm_kernel_mod.F90",
                      api="dynamo0.3")
    print str(result)
    expected = (
        "  MODULE columnwise_op_asm_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_kernel_code(cell, nlayers, "
        "ncell_2d, op_1_ncell_3d, op_1, cma_op_2, cma_op_2_nrow, "
        "cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, "
        "cma_op_2_gamma_m, cma_op_2_gamma_p, ndf_any_space_1_field_1, "
        "undf_any_space_1_field_1, dofmap_any_space_1_field_1, "
        "ndf_any_space_2_op_1, cma_op_2_column_banded_dofmap_to, "
        "cma_op_2_column_banded_dofmap_from)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_op_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_2_op_1\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_field_1,"
        "ndf_any_space_2_op_1,op_1_ncell_3d) :: op_1\n"
        "      INTEGER, intent(in) :: cma_op_2_nrow, cma_op_2_ncol, "
        "cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, cma_op_2_gamma_m,"
        " cma_op_2_gamma_p\n"
        "      REAL(KIND=r_def), intent(out), dimension(cma_op_2_bandwidth,"
        "cma_op_2_nrow,ncell_2d) :: cma_op_2\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_op_1,"
        "nlayers) :: cma_op_2_column_banded_dofmap_to\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_op_1,"
        "nlayers) :: cma_op_2_column_banded_dofmap_from\n"
        "    END SUBROUTINE columnwise_op_asm_kernel_code\n"
        "  END MODULE columnwise_op_asm_kernel_mod")
    assert expected in str(result)


def test_cma_asm_with_field_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly when a
    field is involved '''
    result = generate(
        "test_files/dynamo0p3/columnwise_op_asm_field_kernel_mod.F90",
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
        "ndf_any_space_2_op_2, cma_op_3_column_banded_dofmap_to, "
        "cma_op_3_column_banded_dofmap_from)\n"
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
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_field_1,"
        "ndf_any_space_2_op_2,op_2_ncell_3d) :: op_2\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m,"
        " cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(out), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1) :: "
        "map_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1,"
        "nlayers) :: cma_op_3_column_banded_dofmap_to\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_2_op_2,"
        "nlayers) :: cma_op_3_column_banded_dofmap_from\n"
        "    END SUBROUTINE columnwise_op_asm_field_kernel_code\n"
        "  END MODULE columnwise_op_asm_field_kernel_mod")
    assert expected in str(result)


def test_cma_asm_same_fs_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly when the
    to and from spaces are the same '''
    result = generate("test_files/dynamo0p3/"
                      "columnwise_op_asm_same_fs_kernel_mod.F90",
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
        "cma_op_3_column_banded_dofmap_to)\n"
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
        ":: cma_op_3_column_banded_dofmap_to\n")
    assert expected in str(result)


def test_cma_app_stub_gen():
    ''' Test the kernel-stub generator for a CMA apply kernel. This has
    two fields and one CMA operator as arguments. '''
    result = generate("test_files/dynamo0p3/columnwise_op_app_kernel_mod.F90",
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
        "map_any_space_1_field_1, ndf_any_space_2_field_2, "
        "undf_any_space_2_field_2, map_any_space_2_field_2, "
        "cma_op_3_indirection_dofmap_to, cma_op_3_indirection_dofmap_from)\n"
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
        "      INTEGER, intent(in), dimension(ndf_any_space_2_field_2) :: "
        "map_any_space_2_field_2\n"
        "      INTEGER, intent(in), dimension(cma_op_3_nrow) :: "
        "cma_op_3_indirection_dofmap_to\n"
        "      INTEGER, intent(in), dimension(cma_op_3_ncol) :: "
        "cma_op_3_indirection_dofmap_from\n"
        "    END SUBROUTINE columnwise_op_app_kernel_code\n"
        "  END MODULE columnwise_op_app_kernel_mod")
    assert expected in str(result)


def test_cma_mul_stub_gen():
    ''' Test the kernel-stub generator for a CMA matrix-matrix kernel '''
    result = generate("test_files/dynamo0p3/columnwise_op_mul_kernel.F90",
                      api="dynamo0.3")
    print result
    expected = (
        "  MODULE columnwise_op_mul_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_mul_kernel_code(cell, ncell_2d, cma_op_1, cma_op_1_nrow, cma_op_1_ncol, cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, cma_op_1_gamma_m, cma_op_1_gamma_p, cma_op_2, cma_op_2_nrow, cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, cma_op_2_gamma_m, cma_op_2_gamma_p, cma_op_3, cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: ncell_2d\n"
        "      INTEGER, intent(in) :: cma_op_1_nrow, cma_op_1_ncol, "
        "cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, cma_op_1_gamma_m, cma_op_1_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_1_bandwidth,cma_op_1_nrow,ncell_2d) :: cma_op_1\n"
        "      INTEGER, intent(in) :: cma_op_2_nrow, cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, cma_op_2_gamma_m, cma_op_2_gamma_p\n"
        "      REAL(KIND=r_def), intent(in), dimension(cma_op_2_bandwidth,cma_op_2_nrow,ncell_2d) :: cma_op_2\n"
        "      INTEGER, intent(in) :: cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_def), intent(inout), dimension(cma_op_3_bandwidth,cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "    END SUBROUTINE columnwise_op_mul_kernel_code\n"
        "  END MODULE columnwise_op_mul_kernel_mod")
    assert expected in str(result)
