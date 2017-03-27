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


import pytest
import fparser
from fparser import api as fpapi
from parse import ParseError
from dynamo0p3 import DynKernMetadata

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


def test_cma_mdata_asm_fld_vector_error():  # pylint: disable=invalid-name
    ''' Check that we raise the expected error if a kernel assembling a
    CMA operator reads from a field vector'''
    fparser.logging.disable('CRITICAL')
    # Change the space of the field that is written
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field,gh_read, any_space_1)",
        "arg_type(gh_field*3,gh_read, any_space_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type assembles a CMA operator but has a "
            "vector argument (gh_field*3). This is not permitted.") in \
        str(excinfo)


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
    assert ("Kernel testkern_cma_type assembles a CMA operator but specifies "
            "a stencil access (x1d) on a field argument. This is not "
            "permitted.") in str(excinfo)


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


def test_cma_mdata_apply_fld_vector_error():  # pylint: disable=invalid-name
    ''' Check that we raise the expected error if the meta-data for a kernel
    that applies a CMA operator contains a field vector argument '''
    fparser.logging.disable('CRITICAL')
    code = CMA_APPLY.replace("arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1)",
                             "arg_type(GH_FIELD*3,  GH_INC,  ANY_SPACE_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Kernel testkern_cma_type applies a CMA operator but has a "
            "vector argument (gh_field*3). This is forbidden.") in str(excinfo)


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
    assert ("Kernel testkern_cma_type applies a CMA operator but has a "
            "field argument with a stencil access (x1d). This is "
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
