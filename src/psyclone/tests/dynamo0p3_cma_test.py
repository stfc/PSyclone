# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic and L. Turner, Met Office
# Modified: J. Henrichs, Bureau of Meteorology

''' This module tests the support for Column-Matrix-Assembly operators in
the LFRic (Dynamo 0.3) API using pytest. '''

import os
import pytest
import fparser
from fparser import api as fpapi

from psyclone.tests.lfric_build import LFRicBuild
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import (LFRicArgDescriptor, LFRicConstants,
                                   LFRicKernMetadata)
from psyclone.dynamo0p3 import DynDofmaps
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import ModuleGen
from psyclone.gen_kernel_stub import generate
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

TEST_API = "dynamo0.3"

# Define LMA/CMA operator write accesses for testing purposes
OP_WRITE_ACCESSES = ["gh_write", "gh_readwrite"]

CMA_ASSEMBLE = '''
module testkern_cma
  type, extends(kernel_type) :: testkern_cma_type
     type(arg_type), meta_args(4) =                                &
          (/ arg_type(gh_operator, gh_real, gh_read, any_space_1,  &
                                                     any_space_2), &
             arg_type(gh_columnwise_operator, gh_real, gh_write,   &
                                    any_space_1, any_space_2),     &
             arg_type(gh_field, gh_real, gh_read, any_space_1),    &
             arg_type(gh_scalar, gh_real, gh_read)                 &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_cma_code
  end type testkern_cma_type
contains
  subroutine testkern_cma_code(a, b, c, d)
  end subroutine testkern_cma_code
end module testkern_cma
'''


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use Dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


def test_cma_mdata_assembly():
    ''' Check that we can parse metadata entries relating to Column-Matrix
    Assembly. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_ASSEMBLE
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = LFRicKernMetadata(ast, name=name)
    cma_op_desc = dkm.arg_descriptors[1]

    # Assert correct string representation from LFRicArgDescriptor
    cma_op_desc_str = str(cma_op_desc)
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_write'\n"
        "  function_space_to[3]='any_space_1'\n"
        "  function_space_from[4]='any_space_2'\n")

    assert expected in cma_op_desc_str
    assert dkm._cma_operation == "assembly"

    # Check LFRicArgDescriptor argument properties
    assert cma_op_desc.argument_type == "gh_columnwise_operator"
    assert cma_op_desc.data_type == "gh_real"
    assert cma_op_desc.function_space_to == "any_space_1"
    assert cma_op_desc.function_space_from == "any_space_2"
    assert cma_op_desc.function_space == "any_space_2"
    assert cma_op_desc.function_spaces == ['any_space_1', 'any_space_2']
    assert str(cma_op_desc.access) == "WRITE"
    assert cma_op_desc.mesh is None
    assert cma_op_desc.stencil is None
    assert cma_op_desc.vector_size == 1


def test_cma_mdata_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a column-wise operator has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_cma_type"
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_columnwise_operator, gh_real, gh_write,   &\n",
        "arg_type(gh_columnwise_operator, gh_unreal, gh_write,   &\n", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    const = LFRicConstants()
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"In the LFRic API the 2nd argument of a 'meta_arg' "
            f"entry should be a valid data type (one of "
            f"{const.VALID_SCALAR_DATA_TYPES}), but found 'gh_unreal' "
            f"in 'arg_type(gh_columnwise_operator, gh_unreal, "
            f"gh_write, any_space_1, any_space_2)'." in str(excinfo.value))


def test_cma_mdata_init_wrong_argument_type():
    ''' Test that an error is raised if something other than an operator
    is passed to the LFRicArgDescriptor._init_operator() method. '''
    ast = fpapi.parse(CMA_ASSEMBLE, ignore_comments=False)
    name = "testkern_cma_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get an argument which is not an operator
    wrong_arg = metadata._inits[3]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over, 0)._init_operator(wrong_arg)
    assert ("Expected an operator argument but got an argument of type "
            "'gh_scalar'." in str(excinfo.value))


def test_cma_mdata_init_wrong_data_type():
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_operator() method. '''
    ast = fpapi.parse(CMA_ASSEMBLE, ignore_comments=False)
    name = "testkern_cma_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get a column-wise operator argument descriptor and set a wrong data type
    cma_op_arg = metadata._inits[1]
    cma_op_arg.args[1].name = "gh_integer"
    with pytest.raises(ParseError) as excinfo:
        LFRicArgDescriptor(
            cma_op_arg, metadata.iterates_over, 0)._init_operator(cma_op_arg)
    const = LFRicConstants()
    assert (f"In the LFRic API the allowed data types for operator arguments "
            f"are one of {const.VALID_OPERATOR_DATA_TYPES}, but found "
            f"'gh_integer' in 'arg_type(gh_columnwise_operator, gh_integer, "
            f"gh_write, any_space_1, any_space_2)'." in str(excinfo.value))


def test_cma_mdata_assembly_missing_op():
    ''' Check that we raise the expected error if the supplied metadata
    is assembling a gh_columnwise_operator but doesn't have a read-only
    gh_operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_ASSEMBLE.splitlines()
    # Remove the (required) LMA operator
    code[4:6] = ["          (/ &"]
    code = "\n".join(code).replace("meta_args(4) =", "meta_args(3) =", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_cma_type' has a single column-wise operator "
            "argument but does not conform to the rules for an Assembly "
            "kernel because it does not have any read-only LMA operator "
            "arguments.") in str(excinfo.value)


def test_cma_mdata_multi_writes():
    ''' Check that we raise the expected error if the supplied metadata
    specifies more than one CMA operator that is written to '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Replace the field arg with another CMA operator that is written to
    for access in OP_WRITE_ACCESSES:
        cmaopstring = (
            "arg_type(gh_columnwise_operator, gh_real, " + access +
            ",any_space_1,any_space_2)")
        code = CMA_ASSEMBLE.replace(
            "arg_type(gh_field, gh_real, gh_read, any_space_1)",
            cmaopstring, 1)
        ast = fpapi.parse(code, ignore_comments=False)
        name = "testkern_cma_type"
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("An LFRic kernel cannot update more than one CMA "
                "(column-wise) operator but kernel 'testkern_cma_type' "
                "updates 2") in str(excinfo.value)
        code = CMA_ASSEMBLE.replace(
            "arg_type(gh_field, gh_real, gh_read, any_space_1)",
            cmaopstring + ",&\n" +
            cmaopstring, 1)
        code = code.replace("meta_args(4) = ", "meta_args(5) = ", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("An LFRic kernel cannot update more than one CMA "
                "(column-wise) operator but kernel 'testkern_cma_type' "
                "updates 3") in str(excinfo.value)


def test_cma_mdata_mutable_op():
    ''' Check that we raise the expected error if the supplied metadata
    is assembling a gh_columnwise_operator but doesn't have a read-only
    gh_operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Make the LMA operator gh_write and gh_readwrite instead of gh_read
    for access in OP_WRITE_ACCESSES:
        opstring = "gh_operator, gh_real, " + access + \
                   ", any_space_1,  &\n"
        code = CMA_ASSEMBLE.replace(
            "gh_operator, gh_real, gh_read, any_space_1,  &\n",
            opstring, 1)
        ast = fpapi.parse(code, ignore_comments=False)
        name = "testkern_cma_type"
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("Kernel 'testkern_cma_type' writes to a column-wise operator "
                "but also writes to ['gh_operator'] argument(s). This is "
                "not allowed.") in str(excinfo.value)


def test_cma_mdata_writes_lma_op():
    ''' Check that we raise the expected error if the supplied metadata
    is assembling a gh_columnwise_operator but also writes to a
    gh_operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Add an additional LMA operator that has write or readwrite access
    for access in OP_WRITE_ACCESSES:
        opstring = (
            "             arg_type(gh_operator, gh_real, " + access +
            ", any_space_1, any_space_2), &")
        code = CMA_ASSEMBLE.split("\n")
        code.insert(6, opstring)
        code = "\n".join(code).replace("meta_args(4)", "meta_args(5)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        name = "testkern_cma_type"
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("Kernel 'testkern_cma_type' writes to a column-wise operator "
                "but also writes to ['gh_operator'] argument(s). This is "
                "not allowed.") in str(excinfo.value)


def test_cma_mdata_assembly_diff_spaces():
    ''' Check that we successfully parse the supplied metadata if it
    is assembling a gh_columnwise_operator but the to/from spaces don't
    match those of the supplied 'gh_operator'.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change the to space of the LMA operator
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_operator, gh_real, gh_read, any_space_1,",
        "arg_type(gh_operator, gh_real, gh_read, any_space_3,", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = LFRicKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[0])
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_operator'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n"
        "  function_space_to[3]='any_space_3'\n"
        "  function_space_from[4]='any_space_2'\n")
    assert expected in dkm_str
    assert dkm._cma_operation == "assembly"


def test_cma_mdata_asm_vector_error():
    ''' Check that we raise the expected error if a kernel assembling a
    CMA operator has any vector arguments. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_cma_type"

    # Reject a field vector argument
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field, gh_real, gh_read, any_space_1)",
        "arg_type(gh_field*3, gh_real, gh_read, any_space_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_cma_type' takes a CMA operator but has a "
            "vector argument 'gh_field*3'. This is forbidden."
            in str(excinfo.value))

    # Reject a CMA operator vector argument
    code = CMA_ASSEMBLE.replace(
        "gh_columnwise_operator,", "gh_columnwise_operator*2,", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API, vector notation is only supported "
            "for ['gh_field'] argument types but found "
            "'gh_columnwise_operator * 2'." in str(excinfo.value))


def test_cma_mdata_asm_fld_stencil_error():
    ''' Check that we raise the expected error if a kernel assembling a
    CMA operator specifies a stencil access on a field. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change the space of the field that is written
    code = CMA_ASSEMBLE.replace(
        "arg_type(gh_field, gh_real, gh_read, any_space_1)",
        "arg_type(gh_field, gh_real, gh_read, any_space_1, stencil(x1d))", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_cma_type' takes a CMA operator but has an "
            "argument with a stencil access ('x1d'). This is forbidden."
            in str(excinfo.value))


def test_invoke_uniq_declns_valid_access_cma_op():
    ''' Tests that all valid access modes for user-defined CMA operator
    arguments (AccessType.READ, AccessType.WRITE, AccessType.READWRITE)
    are accepted by Invoke.unique_declarations(). Also tests the
    correctness of names of arguments and their proxies.

    '''
    # Test READ
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "20.5_multi_cma_invoke.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    cma_ops_read_args = (psy.invokes.invoke_list[0].unique_declarations(
        ["gh_columnwise_operator"], access=AccessType.READ))
    cma_ops_read = [arg.declaration_name for arg in cma_ops_read_args]
    cma_ops_proxy_read = [arg.proxy_declaration_name for arg in
                          cma_ops_read_args]
    assert cma_ops_read == ["cma_op1", "cma_opb"]
    assert cma_ops_proxy_read == ["cma_op1_proxy", "cma_opb_proxy"]

    # Test READWRITE
    cma_ops_readwritten_args = (psy.invokes.invoke_list[0].unique_declarations(
        ["gh_columnwise_operator"], access=AccessType.READWRITE))
    cma_ops_readwritten = [arg.declaration_name for arg in
                           cma_ops_readwritten_args]
    cma_ops_proxy_readwritten = [arg.proxy_declaration_name for arg in
                                 cma_ops_readwritten_args]
    assert cma_ops_readwritten == ["cma_opc"]
    assert cma_ops_proxy_readwritten == ["cma_opc_proxy"]

    # Test WRITE
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "20.0_cma_assembly.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    cma_ops_written_args = (psy.invokes.invoke_list[0].unique_declarations(
        ["gh_columnwise_operator"], access=AccessType.WRITE))
    cma_ops_written = [arg.declaration_name for arg in cma_ops_written_args]
    cma_ops_proxy_written = [arg.proxy_declaration_name for arg
                             in cma_ops_written_args]
    assert cma_ops_written == ["cma_op1"]
    assert cma_ops_proxy_written == ["cma_op1_proxy"]


def test_cma_operator_arg_lfricconst_properties(monkeypatch):
    ''' Tests that properties of supported CMA operator arguments
    ('real'-valued 'columnwise_operator_type') defined in LFRicConstants
    are correctly set up in the DynKernelArgument class.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "20.5_multi_cma_invoke.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.kernels()[0]
    cma_op_arg = kernel.arguments.args[2]

    assert cma_op_arg.module_name == "columnwise_operator_mod"
    assert cma_op_arg.data_type == "columnwise_operator_type"
    assert cma_op_arg.proxy_data_type == "columnwise_operator_proxy_type"
    assert cma_op_arg.intrinsic_type == "real"
    assert cma_op_arg.precision == "r_solver"

    # Monkeypatch to check with an invalid argument type of an
    # operator argument. The LFRicConstants class needs to be
    # initialised before the monkeypatch.
    _ = LFRicConstants()
    monkeypatch.setattr(LFRicConstants, "VALID_OPERATOR_NAMES",
                        ["calico"])
    monkeypatch.setattr(cma_op_arg, "_argument_type", "calico")

    with pytest.raises(InternalError) as err:
        cma_op_arg._init_data_type_properties(None, check=False)
    assert ("Expected 'gh_operator' or 'gh_columnwise_operator' "
            "argument type but found 'calico'." in str(err.value))


CMA_APPLY = '''
module testkern_cma_apply
  type, extends(kernel_type) :: testkern_cma_type
  type(arg_type) :: meta_args(3) = (/                     &
       arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, &
                                ANY_SPACE_1, ANY_SPACE_2) &
       /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_cma_code
  end type testkern_cma_type
contains
  subroutine testkern_cma_code(a, b, c, d)
  end subroutine testkern_cma_code
end module testkern_cma_apply
'''


def test_cma_mdata_apply():
    ''' Check that we can parse metadata entries relating to the
    application of Column-Matrix operators. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_APPLY
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = LFRicKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n"
        "  function_space[3]='any_space_2'\n")

    assert expected in dkm_str
    dkm_str = str(dkm.arg_descriptors[2])
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n"
        "  function_space_to[3]='any_space_1'\n"
        "  function_space_from[4]='any_space_2'\n")
    assert expected in dkm_str
    assert dkm._cma_operation == "apply"


def test_cma_mdata_apply_too_many_ops():
    ''' Check that we raise the expected error if there are too-many
    CMA operators '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Add an additional read-only CMA operator to the argument list
    code = CMA_APPLY.replace(
        "                         ANY_SPACE_1, ANY_SPACE_2) &\n",
        "                         ANY_SPACE_1, ANY_SPACE_2), &\n"
        "       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, &\n"
        "                                ANY_SPACE_1, ANY_SPACE_2) &\n", 1)
    code = code.replace("meta_args(3)", "meta_args(4)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("a kernel that applies a CMA operator must only have one such "
            "operator in its list of arguments but found 2"
            in str(excinfo.value))


def test_cma_mdata_apply_too_many_flds():
    ''' Check that we raise the expected error if there are too-many
    field args to a kernel that applies a CMA operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Add an additional read-only field to the argument list
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2), &\n",
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2), &\n"
        "       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2), &\n", 1)
    code = code.replace("meta_args(3)", "meta_args(4)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("a kernel that applies a CMA operator must have 3 arguments "
            "(the operator and two fields) but kernel 'testkern_cma_type' "
            "has 4") in str(excinfo.value)


def test_cma_mdata_apply_no_read_fld():
    ''' Check that we raise the expected error if there is no read-only
    field arg to a kernel that applies a CMA operator.'''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Make the read-only field gh_inc instead
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2), ",
        "arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_2), ", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("has a read-only CMA operator. In order to apply it the kernel "
            "must have one read-only field argument") in str(excinfo.value)


def test_cma_mdata_apply_no_write_fld():
    ''' Check that we raise the expected error if there is no written
    field arg to a kernel that applies a CMA operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Turn the written field into an operator instead
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1), ",
        "arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, ANY_SPACE_1, "
        "ANY_SPACE_1), ", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("has a read-only CMA operator. In order to apply it the kernel "
            "must write to one field argument") in str(excinfo.value)


def test_cma_mdata_apply_wrong_spaces():
    ''' Check that we raise the expected error if the function spaces of the
    read and write fields do not match the from and to function spaces of the
    CMA operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change the space of the field that is written
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1)",
        "arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("applies a CMA operator but the function space of the field "
            "argument it writes to ('any_space_3') does not match the 'to' "
            "space of the operator ('any_space_1')") in str(excinfo.value)
    # Change the space of the field that is read
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2)",
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("applies a CMA operator but the function space of the field "
            "argument it reads from ('any_space_3') does not match the 'from' "
            "space of the operator ('any_space_2')") in str(excinfo.value)


def test_cma_mdata_apply_vector_error():
    ''' Check that we raise the expected error if the metadata for a kernel
    that applies a CMA operator contains a vector argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_cma_type"

    # Reject a field vector argument
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1)",
        "arg_type(GH_FIELD*3, GH_REAL, GH_INC,  ANY_SPACE_1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_cma_type' takes a CMA operator but has a "
            "vector argument 'gh_field*3'. This is forbidden."
            in str(excinfo.value))

    # Reject a CMA operator vector argument
    code = CMA_APPLY.replace("GH_COLUMNWISE_OPERATOR,",
                             "GH_COLUMNWISE_OPERATOR*4,", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API, vector notation is only supported "
            "for ['gh_field'] argument types but found "
            "'gh_columnwise_operator * 4'." in str(excinfo.value))


def test_cma_mdata_apply_fld_stencil_error():
    ''' Check that we raise the expected error if the metadata for a kernel
    that applies a CMA operator contains a field argument with a stencil
    access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2)",
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2, STENCIL(X1D))", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_cma_type' takes a CMA operator but has an "
            "argument with a stencil access ('x1d'). This is "
            "forbidden.") in str(excinfo.value)


def test_cma_mdata_apply_invalid_field_data_type():
    ''' Check that we raise the expected error if the metadata for a kernel
    that applies a CMA operator contains a field argument with an invalid
    data type (other than 'gh_real'). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_APPLY.replace(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2)",
        "arg_type(GH_FIELD, GH_INTEGER, GH_READ, ANY_SPACE_2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API a kernel that takes a CMA operator argument "
            "must only have field arguments with 'gh_real' data type but "
            "kernel 'testkern_cma_type' has a field argument with "
            "'gh_integer' data type." in str(excinfo.value))


CMA_MATRIX = '''
module testkern_cma_matrix_matrix
  type, extends(kernel_type) :: testkern_cma_type
  type(arg_type) :: meta_args(4) = (/                      &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,  &
                                ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_SCALAR,              GH_REAL, GH_READ), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,  &
                                ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_WRITE, &
                                ANY_SPACE_1, ANY_SPACE_2)  &
       /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_cma_code
  end type testkern_cma_type
contains
  subroutine testkern_cma_code(a, b, c, d)
  end subroutine testkern_cma_code
end module testkern_cma_matrix_matrix
'''


def test_cma_mdata_matrix_prod():
    ''' Check that we can parse metadata entries relating to a kernel
    that performs a product of two CMA operators. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_MATRIX
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = LFRicKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[2])
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_columnwise_operator'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n"
        "  function_space_to[3]='any_space_1'\n"
        "  function_space_from[4]='any_space_2'\n")

    assert expected in dkm_str
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_too_few_args():
    ''' Check that we raise the expected error when there are too few
    arguments specified in metadata '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_MATRIX.split("\n")
    # Remove read-only cma operators
    del code[4:6]
    del code[5:7]
    code = "\n".join(code).replace("meta_args(4)", "meta_args(2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("has a single column-wise operator argument but does not conform "
            "to the rules for an Assembly kernel because it does not have "
            "any read-only LMA operator arguments") in str(excinfo.value)


def test_cma_mdata_matrix_field_arg():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    reads from a field argument. Adding an argument that is not a CMA
    operator or scalar means that PSyclone attempts to identify this as an
    assembly kernel. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_MATRIX.split("\n")
    code[4:6] = ["       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_1), &"]
    code = "\n".join(code)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("A column-wise matrix-matrix kernel must have only column-wise "
            "operators and scalars as arguments but kernel "
            "'testkern_cma_type' has: ['gh_field', ") in str(excinfo.value)


def test_cma_mdata_matrix_no_scalar_arg():
    ''' Check that we successfully parse metadata for a matrix-matrix kernel
    that has no scalar arguments. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_MATRIX.replace(
        "arg_type(GH_SCALAR,              GH_REAL, GH_READ)",
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, "
        "ANY_SPACE_2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = LFRicKernMetadata(ast, name=name)
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_2_scalar_args():
    ''' Check that we successfully parse metadata for a matrix-matrix kernel
    that has 2 scalar arguments. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ,  &\n"
        "                         ANY_SPACE_1, ANY_SPACE_2), &\n",
        "arg_type(GH_SCALAR,              GH_REAL, GH_READ)",
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    dkm = LFRicKernMetadata(ast, name=name)
    assert dkm._cma_operation == "matrix-matrix"


def test_cma_mdata_matrix_2_writes():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    writes (write and readwrite access) to more than one CMA operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for access in OP_WRITE_ACCESSES:
        cmaopstring = "       arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, " + \
                      access + ", ANY_SPACE_1, ANY_SPACE_2), &"
        code = CMA_MATRIX.split("\n")
        code.insert(7, cmaopstring)
        code = "\n".join(code).replace("meta_args(4)", "meta_args(5)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        name = "testkern_cma_type"
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("An LFRic kernel cannot update more than one CMA "
                "(column-wise) operator but kernel 'testkern_cma_type' "
                "updates 2") in str(excinfo.value)


def test_cma_mdata_stencil_invalid():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    specifies a stencil. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CMA_MATRIX.replace(
        "                 ANY_SPACE_1, ANY_SPACE_2), &\n",
        "                 ANY_SPACE_1, stencil(cross)), &\n", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API argument 5 of a 'meta_arg' operator entry must "
            "be a valid function-space name") in str(excinfo.value)
    code = CMA_MATRIX.replace(
        "                 ANY_SPACE_1, ANY_SPACE_2)  &\n",
        "                 ANY_SPACE_1, ANY_SPACE_2, stencil(cross)) &\n", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_cma_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)

    const = LFRicConstants()
    assert (f"each 'meta_arg' entry must have 5 arguments if its first "
            f"argument is an operator (one of {const.VALID_OPERATOR_NAMES})"
            in str(excinfo.value))


def test_cma_mdata_matrix_vector_error():
    ''' Check that we raise the expected error when a matrix-matrix kernel
    contains a vector argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_cma_type"

    # Reject a CMA operator vector argument
    code = CMA_MATRIX.replace(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_WRITE, &\n",
        "arg_type(GH_COLUMNWISE_OPERATOR*3, GH_REAL, GH_WRITE, &\n",
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API, vector notation is only supported "
            "for ['gh_field'] argument types but found "
            "'gh_columnwise_operator * 3'." in str(excinfo.value))


def test_cma_asm_cbanded_dofmap_error():
    ''' Check that we raise expected internal error if DynInvokeDofmaps
    encounters an assembly kernel that has more than one CMA op argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.0_cma_assembly.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    calls = invoke.schedule.kernels()
    # We must go in and make the internal state inconsistent in order
    # to trigger the error. So, we set the type of all the arguments
    # in the kernel cal to be CMA operators...
    for arg in calls[0].arguments.args:
        arg._argument_type = 'gh_columnwise_operator'
    with pytest.raises(GenerationError) as excinfo:
        invoke.dofmaps.__init__(invoke)
    assert ("Internal error: there should only be one CMA operator argument "
            "for a CMA assembly kernel but found 2") in str(excinfo.value)


def test_cma_asm(tmpdir, dist_mem):
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.0_cma_assembly.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    output = (
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE columnwise_operator_mod, ONLY: columnwise_operator_type, "
        "columnwise_operator_proxy_type\n")
    assert output in code
    assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
    assert ("REAL(KIND=r_def), pointer, dimension(:,:,:) :: "
            "lma_op1_local_stencil => null()" in code)
    assert "TYPE(columnwise_operator_type), intent(in) :: cma_op1" in code
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert ("REAL(KIND=r_solver), pointer, dimension(:,:,:) :: "
            "cma_op1_cma_matrix => null()" in code)
    assert "TYPE(mesh_type), pointer :: mesh => null()" in code
    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert ("INTEGER(KIND=i_def), pointer :: cbanded_map_adspc1_lma_op1(:,:) "
            "=> null(), cbanded_map_adspc2_lma_op1(:,:) => null()") in code
    assert "ncell_2d = mesh%get_ncells_2d" in code
    assert "cma_op1_proxy = cma_op1%get_proxy()" in code
    assert ("CALL columnwise_op_asm_kernel_code(cell, nlayers, ncell_2d, "
            "lma_op1_proxy%ncell_3d, lma_op1_local_stencil, "
            "cma_op1_cma_matrix, cma_op1_nrow, cma_op1_ncol, "
            "cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, cma_op1_gamma_m, "
            "cma_op1_gamma_p, ndf_adspc1_lma_op1, cbanded_map_adspc1_lma_op1, "
            "ndf_adspc2_lma_op1, cbanded_map_adspc2_lma_op1)") in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_asm_field(tmpdir, dist_mem):
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator with a field as argument.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.3_cma_assembly_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    output = (
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE columnwise_operator_mod, ONLY: columnwise_operator_type, "
        "columnwise_operator_proxy_type\n")
    assert output in code
    assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
    assert "TYPE(columnwise_operator_type), intent(in) :: cma_op1" in code
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert ("INTEGER(KIND=i_def), pointer :: "
            "cbanded_map_aspc1_afield(:,:) => null(), "
            "cbanded_map_aspc2_lma_op1(:,:) => null()" in code)
    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "mesh => afield_proxy%vspace%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert "cma_op1_proxy = cma_op1%get_proxy()" in code
    expected = (
        "CALL columnwise_op_asm_field_kernel_code(cell, nlayers, ncell_2d, "
        "afield_data, lma_op1_proxy%ncell_3d, "
        "lma_op1_local_stencil, cma_op1_cma_matrix, cma_op1_nrow, "
        "cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
        "cma_op1_gamma_m, cma_op1_gamma_p, ndf_aspc1_afield, "
        "undf_aspc1_afield, map_aspc1_afield(:,cell), "
        "cbanded_map_aspc1_afield, ndf_aspc2_lma_op1, "
        "cbanded_map_aspc2_lma_op1)")
    assert expected in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_asm_scalar(dist_mem, tmpdir):
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator with a scalar as argument. The
    name of the scalar is deliberately chosen to provoke a clash with the
    name generated for one of the CMA parameters.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.0.1_cma_assembly_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    output = (
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE columnwise_operator_mod, ONLY: columnwise_operator_type, "
        "columnwise_operator_proxy_type\n")
    assert output in code
    assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
    assert "TYPE(columnwise_operator_type), intent(in) :: cma_op1" in code
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert ("INTEGER(KIND=i_def), pointer :: "
            "cbanded_map_aspc1_lma_op1(:,:) => null(), "
            "cbanded_map_aspc2_lma_op1(:,:) => null()" in code)
    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert "cma_op1_proxy = cma_op1%get_proxy()" in code
    expected = ("CALL columnwise_op_asm_kernel_scalar_code(cell, "
                "nlayers, ncell_2d, lma_op1_proxy%ncell_3d, "
                "lma_op1_local_stencil, cma_op1_cma_matrix, cma_op1_nrow, "
                "cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha_1, "
                "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
                "cma_op1_alpha, ndf_aspc1_lma_op1, cbanded_map_aspc1_lma_op1, "
                "ndf_aspc2_lma_op1, cbanded_map_aspc2_lma_op1)")

    assert expected in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_asm_field_same_fs(dist_mem, tmpdir):
    ''' Test that we generate correct code for an invoke containing
    a kernel that assembles a CMA operator with a field as argument.
    In this case the fs_from and fs_to spaces of the operator are the
    same so we only need to pass in one banded dofmap.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.4_cma_assembly_field_same_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    output = (
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE columnwise_operator_mod, ONLY: columnwise_operator_type, "
        "columnwise_operator_proxy_type\n")
    assert output in code
    assert "TYPE(operator_proxy_type) lma_op1_proxy" in code
    assert ("TYPE(columnwise_operator_type), intent(in) :: cma_op1"
            in code)
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert ("INTEGER(KIND=i_def), pointer :: "
            "cbanded_map_aspc2_lma_op1(:,:) => null()\n" in code)
    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "mesh => lma_op1_proxy%fs_from%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert "cma_op1_proxy = cma_op1%get_proxy()" in code
    if dist_mem:
        # When distributed-memory is enabled then we compute operators
        # redundantly (out to the L1 halo)
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in code
    else:
        assert "loop0_stop = cma_op1_proxy%fs_from%get_ncell()\n" in code
    assert "DO cell=loop0_start,loop0_stop\n" in code
    expected = ("CALL columnwise_op_asm_same_fs_kernel_code(cell, "
                "nlayers, ncell_2d, lma_op1_proxy%ncell_3d, "
                "lma_op1_local_stencil, afield_data, "
                "cma_op1_cma_matrix, cma_op1_nrow, cma_op1_bandwidth, "
                "cma_op1_alpha, cma_op1_beta, cma_op1_gamma_m, "
                "cma_op1_gamma_p, ndf_aspc1_lma_op1, undf_aspc1_lma_op1, "
                "map_aspc1_lma_op1(:,cell), ndf_aspc2_lma_op1, "
                "cbanded_map_aspc2_lma_op1)")
    assert expected in code
    # We do not perform halo swaps for operators
    assert "lma_op1_proxy%is_dirty(" not in code
    assert "cma_op1_proxy%is_dirty(" not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_apply_indirection_dofmap_error():
    ''' Check that we raise expected internal error if DynInvokeDofmaps
    encounters an apply kernel that has more than one CMA op argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.1_cma_apply.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    calls = invoke.schedule.kernels()
    # We must go in and make the internal state inconsistent in order
    # to trigger the error. So, we set the type of all the arguments
    # in the kernel cal to be CMA operators...
    for arg in calls[0].arguments.args:
        arg._argument_type = 'gh_columnwise_operator'
    with pytest.raises(GenerationError) as excinfo:
        invoke.dofmaps.__init__(invoke)
    assert ("Internal error: there should only be one CMA "
            "operator argument for a kernel that applies a "
            "CMA operator but found 3") in str(excinfo.value)


def test_cma_apply(tmpdir, dist_mem):
    ''' Test that we generate correct code for a kernel that applies
    a CMA operator. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.1_cma_apply.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert "mesh => field_a_proxy%vspace%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert ("INTEGER(KIND=i_def), pointer :: cma_indirection_map_aspc1_"
            "field_a(:) => null(), "
            "cma_indirection_map_aspc2_field_b(:) => null()\n") in code
    assert ("ndf_aspc1_field_a = field_a_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_field_a = field_a_proxy%vspace%"
            "get_undf()") in code
    assert ("cma_indirection_map_aspc1_field_a => "
            "cma_op1_proxy%indirection_dofmap_to") in code
    assert ("cma_indirection_map_aspc2_field_b => "
            "cma_op1_proxy%indirection_dofmap_from") in code
    assert ("CALL columnwise_op_app_kernel_code(cell, ncell_2d, "
            "field_a_data, field_b_data, cma_op1_cma_matrix, "
            "cma_op1_nrow, cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, "
            "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
            "ndf_aspc1_field_a, undf_aspc1_field_a, "
            "map_aspc1_field_a(:,cell), cma_indirection_map_aspc1_field_a, "
            "ndf_aspc2_field_b, undf_aspc2_field_b, map_aspc2_field_b(:,cell),"
            " cma_indirection_map_aspc2_field_b)") in code
    # We do not perform halo swaps for operators
    assert "cma_op1_proxy%is_dirty(" not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_apply_discontinuous_spaces(tmpdir, dist_mem):
    ''' Test that we generate correct code for a kernel that applies a CMA
    operator to fields on discontinuous spaces any_discontinuous_space_1
    and w2v.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.1.2_cma_apply_disc.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    # Check any_discontinuous_space_1
    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert ("INTEGER(KIND=i_def), pointer :: "
            "cma_indirection_map_adspc1_field_a(:) => null(), "
            "cma_indirection_map_aspc1_field_b(:) => null()\n") in code
    assert ("ndf_adspc1_field_a = field_a_proxy%vspace%get_ndf()\n"
            "      undf_adspc1_field_a = "
            "field_a_proxy%vspace%get_undf()") in code
    assert ("cma_indirection_map_adspc1_field_a => "
            "cma_op1_proxy%indirection_dofmap_to") in code
    # Check w2v
    assert "TYPE(columnwise_operator_proxy_type) cma_op2_proxy" in code
    assert "mesh => field_a_proxy%vspace%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert ("INTEGER(KIND=i_def), pointer :: "
            "cma_indirection_map_w2v(:) => null(), "
            "cma_indirection_map_aspc2_field_d(:) => null()\n") in code
    assert ("ndf_w2v = field_c_proxy%vspace%get_ndf()\n"
            "      undf_w2v = field_c_proxy%vspace%get_undf()") in code
    assert ("cma_indirection_map_w2v => "
            "cma_op2_proxy%indirection_dofmap_to") in code
    if dist_mem:
        # The kernel only *reads* from a CMA operator and writes to a
        # field on a discontinuous space - therefore we do not need to
        # loop out into the L1 halo.
        assert code.count("loop0_stop = mesh%get_last_edge_cell()") == 2
    else:
        assert "loop0_stop = field_a_proxy%vspace%get_ncell()" in code
        assert "loop0_stop = field_c_proxy%vspace%get_ncell()" in code

    # Check any_discontinuous_space_1
    assert ("CALL columnwise_op_app_anydspace_kernel_code(cell, "
            "ncell_2d, field_a_data, field_b_data, "
            "cma_op1_cma_matrix, cma_op1_nrow, cma_op1_ncol, "
            "cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
            "cma_op1_gamma_m, cma_op1_gamma_p, ndf_adspc1_field_a, "
            "undf_adspc1_field_a, map_adspc1_field_a(:,cell), "
            "cma_indirection_map_adspc1_field_a, ndf_aspc1_field_b, "
            "undf_aspc1_field_b, map_aspc1_field_b(:,cell), "
            "cma_indirection_map_aspc1_field_b") in code
    # Check w2v
    assert ("CALL columnwise_op_app_w2v_kernel_code(cell, ncell_2d, "
            "field_c_data, field_d_data, cma_op2_cma_matrix, "
            "cma_op2_nrow, cma_op2_ncol, cma_op2_bandwidth, cma_op2_alpha, "
            "cma_op2_beta, cma_op2_gamma_m, cma_op2_gamma_p, ndf_w2v, "
            "undf_w2v, map_w2v(:,cell), cma_indirection_map_w2v, "
            "ndf_aspc2_field_d, undf_aspc2_field_d, map_aspc2_field_d"
            "(:,cell), cma_indirection_map_aspc2_field_d)") in code

    if dist_mem:
        # Check any_discontinuous_space_1
        assert "CALL field_a_proxy%set_dirty()" in code
        assert "cma_op1_proxy%is_dirty(" not in code
        # Check w2v
        assert "CALL field_c_proxy%set_dirty()" in code
        assert "cma_op2_proxy%is_dirty(" not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_apply_same_space(dist_mem, tmpdir):
    ''' Test that we generate correct code for
    a kernel that applies a CMA operator which has the same to- and from-
    spaces.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.1.1_cma_apply_same_spaces.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "TYPE(columnwise_operator_proxy_type) cma_op1_proxy" in code
    assert "mesh => field_a_proxy%vspace%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert ("INTEGER(KIND=i_def), pointer :: cma_indirection_map_aspc2_"
            "field_a(:) => null()\n") in code
    assert ("ndf_aspc2_field_a = field_a_proxy%vspace%get_ndf()\n"
            "      undf_aspc2_field_a = field_a_proxy%vspace%"
            "get_undf()") in code
    assert ("cma_indirection_map_aspc2_field_a => "
            "cma_op1_proxy%indirection_dofmap_to") in code
    assert ("CALL columnwise_op_app_same_fs_kernel_code(cell, ncell_2d, "
            "field_a_data, field_b_data, "
            "cma_op1_cma_matrix, cma_op1_nrow, "
            "cma_op1_bandwidth, cma_op1_alpha, "
            "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
            "ndf_aspc2_field_a, undf_aspc2_field_a, "
            "map_aspc2_field_a(:,cell), "
            "cma_indirection_map_aspc2_field_a)") in code
    if dist_mem:
        assert "CALL field_a_proxy%set_dirty()" in code
        assert "cma_op1_proxy%is_dirty(" not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_matrix_matrix(tmpdir, dist_mem):
    ''' Test that we generate correct code for an invoke containing
    a kernel that performs a matrix-matrix CMA calculation '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.2_cma_matrix_matrix.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "mesh => cma_opa_proxy%fs_from%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code

    if dist_mem:
        # When distributed-memory is enabled then we compute operators
        # redundantly (out to the L1 halo)
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in code
    else:
        assert "loop0_stop = cma_opc_proxy%fs_from%get_ncell()\n" in code

    assert ("CALL columnwise_op_mul_kernel_code(cell, "
            "ncell_2d, "
            "cma_opa_cma_matrix, cma_opa_nrow, cma_opa_ncol, "
            "cma_opa_bandwidth, cma_opa_alpha, "
            "cma_opa_beta, cma_opa_gamma_m, cma_opa_gamma_p, "
            "cma_opb_cma_matrix, cma_opb_nrow, cma_opb_ncol, "
            "cma_opb_bandwidth, cma_opb_alpha, "
            "cma_opb_beta, cma_opb_gamma_m, cma_opb_gamma_p, "
            "cma_opc_cma_matrix, cma_opc_nrow, cma_opc_ncol, "
            "cma_opc_bandwidth, cma_opc_alpha, "
            "cma_opc_beta, cma_opc_gamma_m, cma_opc_gamma_p)") in code
    if dist_mem:
        assert "_dirty(" not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_matrix_matrix_2scalars(tmpdir, dist_mem):
    ''' Test that we generate correct code for an invoke containing
    a kernel that performs a matrix-matrix CMA calculation including
    scalar arguments. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.2.1_cma_matrix_matrix.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)
    assert "INTEGER(KIND=i_def) ncell_2d" in code
    assert "mesh => cma_opa_proxy%fs_from%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code

    if dist_mem:
        # When distributed-memory is enabled then we compute operators
        # redundantly (out to the L1 halo)
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in code
    else:
        assert "loop0_stop = cma_opc_proxy%fs_from%get_ncell()\n" in code

    assert ("CALL columnwise_op_mul_2scalars_kernel_code(cell, "
            "ncell_2d, "
            "cma_opa_cma_matrix, cma_opa_nrow, cma_opa_ncol, "
            "cma_opa_bandwidth, cma_opa_alpha, "
            "cma_opa_beta, cma_opa_gamma_m, cma_opa_gamma_p, "
            "alpha, "
            "cma_opb_cma_matrix, cma_opb_nrow, cma_opb_ncol, "
            "cma_opb_bandwidth, cma_opb_alpha, "
            "cma_opb_beta, cma_opb_gamma_m, cma_opb_gamma_p, "
            "beta, "
            "cma_opc_cma_matrix, cma_opc_nrow, cma_opc_ncol, "
            "cma_opc_bandwidth, cma_opc_alpha, "
            "cma_opc_beta, cma_opc_gamma_m, cma_opc_gamma_p)") in code
    if dist_mem:
        assert "_dirty(" not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cma_multi_kernel(tmpdir, dist_mem):
    ''' Test that we generate correct code when an invoke contains multiple
    kernels with CMA operator arguments.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "20.5_multi_cma_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert ("      afield_proxy = afield%get_proxy()\n"
            "      afield_data => afield_proxy%data\n"
            "      lma_op1_proxy = lma_op1%get_proxy()\n"
            "      lma_op1_local_stencil => lma_op1_proxy%local_stencil\n"
            "      cma_op1_proxy = cma_op1%get_proxy()\n"
            "      field_a_proxy = field_a%get_proxy()\n"
            "      field_a_data => field_a_proxy%data\n"
            "      field_b_proxy = field_b%get_proxy()\n"
            "      field_b_data => field_b_proxy%data\n"
            "      cma_opb_proxy = cma_opb%get_proxy()\n"
            "      cma_opc_proxy = cma_opc%get_proxy()\n") in code

    assert "cma_op1_cma_matrix => cma_op1_proxy%columnwise_matrix\n" in code
    assert "cma_op1_ncol = cma_op1_proxy%ncol\n" in code
    assert "cma_op1_nrow = cma_op1_proxy%nrow\n" in code
    assert "cma_op1_bandwidth = cma_op1_proxy%bandwidth\n" in code
    assert "cma_op1_alpha = cma_op1_proxy%alpha\n" in code
    assert "cma_op1_beta = cma_op1_proxy%beta\n" in code

    assert ("      cbanded_map_aspc1_afield => "
            "cma_op1_proxy%column_banded_dofmap_to\n"
            "      cbanded_map_aspc2_lma_op1 => "
            "cma_op1_proxy%column_banded_dofmap_from\n") in code
    assert ("cma_indirection_map_aspc1_field_a => "
            "cma_op1_proxy%indirection_dofmap_to\n"
            "      cma_indirection_map_aspc2_field_b => "
            "cma_op1_proxy%indirection_dofmap_from\n") in code

    if dist_mem:
        # When distributed-memory is enabled then we compute operators
        # redundantly (out to the L1 halo). Since the field that the
        # CMA operator is applied to is on any-space, we must assume
        # the worst and also loop out to L1 for it too.
        assert code.count("_stop = mesh%get_last_halo_cell(1)\n") == 3
    else:
        assert ("      loop0_stop = cma_op1_proxy%fs_from%get_ncell()\n"
                "      loop1_start = 1\n"
                "      loop1_stop = field_a_proxy%vspace%get_ncell()\n"
                "      loop2_start = 1\n"
                "      loop2_stop = cma_opc_proxy%fs_from%get_ncell()\n"
                in code)

    assert ("CALL columnwise_op_asm_field_kernel_code(cell, nlayers, "
            "ncell_2d, afield_data, lma_op1_proxy%ncell_3d, "
            "lma_op1_local_stencil, cma_op1_cma_matrix, cma_op1_nrow, "
            "cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
            "cma_op1_gamma_m, cma_op1_gamma_p, ndf_aspc1_afield, "
            "undf_aspc1_afield, map_aspc1_afield(:,cell), "
            "cbanded_map_aspc1_afield, ndf_aspc2_lma_op1, "
            "cbanded_map_aspc2_lma_op1)") in code
    assert ("CALL columnwise_op_app_kernel_code(cell, ncell_2d, "
            "field_a_data, field_b_data, cma_op1_cma_matrix, "
            "cma_op1_nrow, cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, "
            "cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
            "ndf_aspc1_field_a, undf_aspc1_field_a, "
            "map_aspc1_field_a(:,cell), cma_indirection_map_aspc1_field_a, "
            "ndf_aspc2_field_b, undf_aspc2_field_b, "
            "map_aspc2_field_b(:,cell), "
            "cma_indirection_map_aspc2_field_b)\n") in code
    assert ("CALL columnwise_op_mul_kernel_code(cell, ncell_2d, "
            "cma_op1_cma_matrix, cma_op1_nrow, cma_op1_ncol, "
            "cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, cma_op1_gamma_m, "
            "cma_op1_gamma_p, "
            "cma_opb_cma_matrix, cma_opb_nrow, cma_opb_ncol, "
            "cma_opb_bandwidth, cma_opb_alpha, cma_opb_beta, cma_opb_gamma_m, "
            "cma_opb_gamma_p, "
            "cma_opc_cma_matrix, cma_opc_nrow, cma_opc_ncol, "
            "cma_opc_bandwidth, cma_opc_alpha, cma_opc_beta, cma_opc_gamma_m, "
            "cma_opc_gamma_p)") in code

    assert LFRicBuild(tmpdir).code_compiles(psy)

# Tests for the kernel-stub generator


def test_dyndofmap_stubdecln_err():
    ''' Check that DynDofmaps._stub_declarations raises the expected errors
    if the stored CMA information is invalid. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "20.5_multi_cma_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    dofmaps = DynDofmaps(psy.invokes.invoke_list[0])
    mod = ModuleGen(name="test_module")
    for cma in dofmaps._unique_indirection_maps.values():
        cma["direction"] = "not-a-direction"
    with pytest.raises(InternalError) as err:
        dofmaps._stub_declarations(mod)
    assert ("Invalid direction ('not-a-direction') found for CMA operator "
            "when collecting indirection dofmaps" in str(err.value))
    for cma in dofmaps._unique_cbanded_maps.values():
        cma["direction"] = "not-a-direction"
    with pytest.raises(InternalError) as err:
        dofmaps._stub_declarations(mod)
    assert ("Invalid direction ('not-a-direction') found for CMA operator "
            "when collecting column-banded dofmaps" in str(err.value))


def test_cma_asm_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly.

    '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_asm_kernel_mod.F90"),
                      api=TEST_API)

    expected = (
        "  MODULE columnwise_op_asm_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_kernel_code(cell, nlayers, "
        "ncell_2d, op_1_ncell_3d, op_1, cma_op_2, cma_op_2_nrow, "
        "cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, "
        "cma_op_2_gamma_m, cma_op_2_gamma_p, ndf_adspc1_op_1, "
        "cbanded_map_adspc1_op_1, ndf_adspc2_op_1, cbanded_map_adspc2_op_1)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_adspc1_op_1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension("
        "ndf_adspc1_op_1,nlayers) :: cbanded_map_adspc1_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_adspc2_op_1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension("
        "ndf_adspc2_op_1,nlayers) :: cbanded_map_adspc2_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_2_nrow, "
        "cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, "
        "cma_op_2_gamma_m, cma_op_2_gamma_p\n"
        "      REAL(KIND=r_solver), intent(inout), dimension("
        "cma_op_2_bandwidth,cma_op_2_nrow,ncell_2d) :: cma_op_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_adspc1_op_1,"
        "ndf_adspc2_op_1,op_1_ncell_3d) :: op_1\n"
        "    END SUBROUTINE columnwise_op_asm_kernel_code\n"
        "  END MODULE columnwise_op_asm_kernel_mod")
    assert expected in str(result)


def test_cma_asm_with_field_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly when a
    field is involved.

    '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_asm_field_kernel_mod.F90"),
                      api=TEST_API)

    expected = (
        "  MODULE columnwise_op_asm_field_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_field_kernel_code(cell, nlayers, "
        "ncell_2d, field_1_aspc1_field_1, op_2_ncell_3d, op_2, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, "
        "cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "ndf_aspc1_field_1, undf_aspc1_field_1, map_aspc1_field_1, "
        "cbanded_map_aspc1_field_1, ndf_aspc2_op_2, cbanded_map_aspc2_op_2)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_field_1) :: map_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_field_1,nlayers) :: cbanded_map_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc2_op_2\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc2_op_2,nlayers) :: cbanded_map_aspc2_op_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow, "
        "cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_solver), intent(inout), dimension("
        "cma_op_3_bandwidth,cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_aspc1_field_1) :: "
        "field_1_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension("
        "ndf_aspc1_field_1,ndf_aspc2_op_2,op_2_ncell_3d) :: op_2\n"
        "    END SUBROUTINE columnwise_op_asm_field_kernel_code\n"
        "  END MODULE columnwise_op_asm_field_kernel_mod")
    assert expected in str(result)


def test_cma_asm_same_fs_stub_gen():
    ''' Test the kernel-stub generator for CMA operator assembly when the
    to and from spaces are the same.

    '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_asm_same_fs_kernel_mod.F90"),
                      api=TEST_API)

    expected = (
        "  MODULE columnwise_op_asm_same_fs_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_asm_same_fs_kernel_code(cell, nlayers, "
        "ncell_2d, op_1_ncell_3d, op_1, field_2_aspc1_op_1, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p, ndf_aspc1_op_1, undf_aspc1_op_1, "
        "map_aspc1_op_1, ndf_aspc2_op_1, cbanded_map_aspc2_op_1)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc1_op_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_op_1) :: map_aspc1_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc2_op_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc2_op_1,nlayers) :: cbanded_map_aspc2_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_aspc1_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow, "
        "cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_solver), intent(inout), dimension("
        "cma_op_3_bandwidth,cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_aspc1_op_1) :: "
        "field_2_aspc1_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_aspc1_op_1,"
        "ndf_aspc2_op_1,op_1_ncell_3d) :: op_1\n")
    assert expected in str(result)


def test_cma_app_stub_gen():
    ''' Test the kernel-stub generator for a CMA apply kernel. This has
    two fields and one CMA operator as arguments.

    '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_app_kernel_mod.F90"),
                      api=TEST_API)

    expected = (
        "  MODULE columnwise_op_app_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_app_kernel_code(cell, ncell_2d, "
        "field_1_aspc1_field_1, field_2_aspc2_field_2, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, "
        "cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "ndf_aspc1_field_1, undf_aspc1_field_1, map_aspc1_field_1, "
        "cma_indirection_map_aspc1_field_1, ndf_aspc2_field_2, "
        "undf_aspc2_field_2, map_aspc2_field_2, "
        "cma_indirection_map_aspc2_field_2)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_field_1) :: map_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc2_field_2\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc2_field_2) :: map_aspc2_field_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(cma_op_3_nrow) :: "
        "cma_indirection_map_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_ncol\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(cma_op_3_ncol) :: "
        "cma_indirection_map_aspc2_field_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_aspc1_field_1, "
        "undf_aspc2_field_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_bandwidth, "
        "cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_solver), intent(in), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_aspc1_field_1) :: field_1_aspc1_field_1\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(undf_aspc2_field_2) :: field_2_aspc2_field_2\n"
        "    END SUBROUTINE columnwise_op_app_kernel_code\n"
        "  END MODULE columnwise_op_app_kernel_mod")
    assert expected in str(result)


def test_cma_app_same_space_stub_gen():
    ''' Test the kernel-stub generator for a CMA apply kernel where the
    to/from function spaces of the CMA operator are the same. This kernel has
    two fields and one CMA operator as arguments.

    '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_app_same_fs_kernel_mod.F90"),
                      api=TEST_API)

    expected = (
        "  MODULE columnwise_op_app_same_fs_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_app_same_fs_kernel_code(cell, ncell_2d, "
        "field_1_aspc2_field_1, field_2_aspc2_field_1, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p, ndf_aspc2_field_1, "
        "undf_aspc2_field_1, map_aspc2_field_1, "
        "cma_indirection_map_aspc2_field_1)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc2_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc2_field_1) :: map_aspc2_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(cma_op_3_nrow) :: "
        "cma_indirection_map_aspc2_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_aspc2_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_bandwidth, "
        "cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_solver), intent(in), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_aspc2_field_1) :: field_1_aspc2_field_1\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(undf_aspc2_field_1) :: field_2_aspc2_field_1\n"
        "    END SUBROUTINE columnwise_op_app_same_fs_kernel_code\n"
        "  END MODULE columnwise_op_app_same_fs_kernel_mod")
    assert expected in str(result)


def test_cma_mul_stub_gen():
    ''' Test the kernel-stub generator for a CMA matrix-matrix kernel '''
    result = generate(os.path.join(BASE_PATH,
                                   "columnwise_op_mul_kernel_mod.F90"),
                      api=TEST_API)

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
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_1_nrow, "
        "cma_op_1_ncol, cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, "
        "cma_op_1_gamma_m, cma_op_1_gamma_p\n"
        "      REAL(KIND=r_solver), intent(in), dimension(cma_op_1_bandwidth,"
        "cma_op_1_nrow,ncell_2d) :: cma_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_2_nrow, "
        "cma_op_2_ncol, cma_op_2_bandwidth, cma_op_2_alpha, cma_op_2_beta, "
        "cma_op_2_gamma_m, cma_op_2_gamma_p\n"
        "      REAL(KIND=r_solver), intent(in), dimension(cma_op_2_bandwidth,"
        "cma_op_2_nrow,ncell_2d) :: cma_op_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow, "
        "cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_solver), intent(inout), dimension("
        "cma_op_3_bandwidth,cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "    END SUBROUTINE columnwise_op_mul_kernel_code\n"
        "  END MODULE columnwise_op_mul_kernel_mod")
    assert expected in str(result)


def test_cma_mul_with_scalars_stub_gen():
    ''' Test the kernel-stub generator for a CMA matrix-matrix kernel that
    includes scalar arguments '''
    result = generate(
        os.path.join(BASE_PATH, "columnwise_op_mul_2scalars_kernel_mod.F90"),
        api=TEST_API)

    expected = (
        "  MODULE columnwise_op_mul_2scalars_kernel_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE columnwise_op_mul_2scalars_kernel_code(cell, "
        "ncell_2d, cma_op_1, cma_op_1_nrow, cma_op_1_ncol, "
        "cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, cma_op_1_gamma_m, "
        "cma_op_1_gamma_p, rscalar_2, "
        "cma_op_3, cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, "
        "cma_op_3_alpha, cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "rscalar_4, "
        "cma_op_5, cma_op_5_nrow, cma_op_5_ncol, cma_op_5_bandwidth, "
        "cma_op_5_alpha, cma_op_5_beta, cma_op_5_gamma_m, cma_op_5_gamma_p)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell, ncell_2d\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_1_nrow, "
        "cma_op_1_ncol, cma_op_1_bandwidth, cma_op_1_alpha, cma_op_1_beta, "
        "cma_op_1_gamma_m, cma_op_1_gamma_p\n"
        "      REAL(KIND=r_solver), intent(in), dimension(cma_op_1_bandwidth,"
        "cma_op_1_nrow,ncell_2d) :: cma_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_3_nrow, "
        "cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, cma_op_3_beta, "
        "cma_op_3_gamma_m, cma_op_3_gamma_p\n"
        "      REAL(KIND=r_solver), intent(in), dimension(cma_op_3_bandwidth,"
        "cma_op_3_nrow,ncell_2d) :: cma_op_3\n"
        "      INTEGER(KIND=i_def), intent(in) :: cma_op_5_nrow, "
        "cma_op_5_ncol, cma_op_5_bandwidth, cma_op_5_alpha, cma_op_5_beta, "
        "cma_op_5_gamma_m, cma_op_5_gamma_p\n"
        "      REAL(KIND=r_solver), intent(inout), "
        "dimension(cma_op_5_bandwidth,cma_op_5_nrow,ncell_2d) :: cma_op_5\n"
        "      REAL(KIND=r_def), intent(in) :: rscalar_2, rscalar_4\n"
        "    END SUBROUTINE columnwise_op_mul_2scalars_kernel_code\n"
        "  END MODULE columnwise_op_mul_2scalars_kernel_mod")
    assert expected in str(result)
