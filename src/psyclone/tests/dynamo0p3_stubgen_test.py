# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' This module tests the Dynamo 0.3 kernel-stub generator using pytest. '''

# imports
from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, KernStubArgList
from psyclone.psyGen import InternalError, GenerationError
from psyclone.parse.utils import ParseError
from psyclone.gen_kernel_stub import generate

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def setup_function():
    '''
    pytest fixture that is called before every test function in this file. It
    ensures that a new NameSpace is created for every test.
    '''
    from psyclone.psyGen import NameSpaceFactory
    NameSpaceFactory._instance = None


def test_kernel_stub_invalid_scalar_argument():
    '''Check that we raise an exception if an unexpected datatype is found
    when using the KernStubArgList scalar method'''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Sabotage the scalar argument to make it have an invalid type.
    arg = kernel.arguments.args[1]
    arg._type = "invalid"
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    with pytest.raises(InternalError) as excinfo:
        create_arg_list.scalar(arg)
    assert (
        "Expected argument type to be one of '['gh_real', "
        "'gh_integer']' but got 'invalid'") in str(excinfo.value)


def test_dynscalars_err(monkeypatch):
    ''' Check that the DynScalarArgs constructor raises the expected error
    if it encounters an unrecognised type of scalar. '''
    from psyclone.dynamo0p3 import DynScalarArgs
    from psyclone import dynamo0p3
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Sabotage the scalar argument to make it have an invalid type.
    arg = kernel.arguments.args[1]
    arg._type = "invalid-scalar-type"
    # Monkeypatch the list of supported scalar types to include this one
    monkeypatch.setattr(dynamo0p3, "VALID_SCALAR_NAMES",
                        ["gh_real", "gh_integer", "invalid-scalar-type"])
    with pytest.raises(InternalError) as err:
        _ = DynScalarArgs(kernel)
    assert ("Scalar type 'invalid-scalar-type' is in VALID_SCALAR_NAMES but "
            "not handled" in str(err))


def test_kernel_stub_ind_dofmap_errors():
    '''Check that we raise the expected exceptions if the wrong arguments
    are supplied to KernelStubArgList.indirection_dofmap() '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
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


def test_kernstubarglist_arglist_error():
    '''Check that we raise an exception if we call the arglist method in
    kernstubarglist without first calling the generate method'''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Now call KernStubArgList to raise an exception
    create_arg_list = KernStubArgList(kernel)
    with pytest.raises(GenerationError) as excinfo:
        _ = create_arg_list.arglist
    assert (
        "Internal error. The argument list in KernStubArgList:arglist() is "
        "empty. Has the generate() method been "
        "called?") in str(excinfo.value)


def test_stub_generate_with_anyw2():
    '''check that the stub generate produces the expected output when we
    have any_w2 fields. In particular, check basis functions as these
    have specific sizes associated with the particular function space'''
    result = generate(os.path.join(BASE_PATH,
                                   "testkern_multi_anyw2_basis_mod.f90"),
                      api=TEST_API)
    expected_output = (
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_any_w2,"
        "np_xy,np_z) :: basis_any_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_any_w2,"
        "np_xy,np_z) :: diff_basis_any_w2")
    assert expected_output in str(result)


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
    "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
    "      INTEGER, intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w1) ::"
    " field_1_w1\n"
    "    END SUBROUTINE simple_code\n"
    "  END MODULE simple_mod")


def test_stub_generate_working():
    ''' check that the stub generate produces the expected output '''
    result = generate(os.path.join(BASE_PATH, "simple.f90"),
                      api=TEST_API)
    assert SIMPLE in str(result)


def test_stub_generate_working_noapi():
    ''' check that the stub generate produces the expected output when
    we use the default api (which should be dynamo0.3)'''
    result = generate(os.path.join(BASE_PATH, "simple.f90"))
    assert SIMPLE in str(result)


SIMPLE_WITH_SCALARS = (
    "  MODULE simple_with_scalars_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE simple_with_scalars_code(nlayers, rscalar_1, field_2_w1, "
    "iscalar_3, ndf_w1, undf_w1, map_w1)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
    "      INTEGER, intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(in) :: rscalar_1\n"
    "      INTEGER, intent(in) :: iscalar_3\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w1) ::"
    " field_2_w1\n"
    "    END SUBROUTINE simple_with_scalars_code\n"
    "  END MODULE simple_with_scalars_mod")


def test_stub_generate_with_scalars():
    ''' check that the stub generate produces the expected output when
    the kernel has scalar arguments '''
    result = generate(os.path.join(BASE_PATH, "simple_with_scalars.f90"),
                      api=TEST_API)
    assert SIMPLE_WITH_SCALARS in str(result)


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


def test_stub_generate_with_scalar_sums():
    '''check that the stub generator raises an exception when a kernel has
    a reduction (since these are not permitted for user-supplied kernels)'''
    with pytest.raises(ParseError) as err:
        _ = generate(
            os.path.join(BASE_PATH, "simple_with_reduction.f90"),
            api=TEST_API)
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
    fparser.logging.disable(fparser.logging.CRITICAL)
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
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_1_w1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w1) :: "
        "field_3_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in str(generated_code)


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
    generated_code = str(kernel.gen_stub)
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
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER, intent(in) :: ndf_w2h\n"
        "      INTEGER, intent(in), dimension(ndf_w2h) :: map_w2h\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      INTEGER, intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      INTEGER, intent(in) :: undf_w0, undf_w1, undf_w2, undf_w3, "
        "undf_wtheta, undf_w2h, undf_w2v\n"
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
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in generated_code


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
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v3\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in str(generated_code)


def test_arg_descriptor_vec_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected when we have a vector quantity '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w0'")
    assert expected_output in result


# orientation : spaces
ORIENTATION_OUTPUT = (
    "    SUBROUTINE dummy_orientation_code(cell, nlayers, field_1_w0, "
    "op_2_ncell_3d, op_2, field_3_w2, op_4_ncell_3d, op_4, ndf_w0, "
    "undf_w0, map_w0, orientation_w0, ndf_w1, orientation_w1, ndf_w2, "
    "undf_w2, map_w2, orientation_w2, ndf_w3, orientation_w3)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      INTEGER, intent(in) :: ndf_w0\n"
    "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
    "      INTEGER, intent(in) :: ndf_w2\n"
    "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
    "      INTEGER, intent(in) :: undf_w0, ndf_w1, undf_w2, ndf_w3\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
    "field_1_w0\n"
    "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
    "field_3_w2\n"
    "      INTEGER, intent(in) :: cell\n"
    "      INTEGER, intent(in) :: op_2_ncell_3d\n"
    "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
    "op_2_ncell_3d) :: op_2\n"
    "      INTEGER, intent(in) :: op_4_ncell_3d\n"
    "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
    "op_4_ncell_3d) :: op_4\n"
    "      INTEGER, intent(in), dimension(ndf_w0) :: orientation_w0\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: orientation_w1\n"
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
    assert ORIENTATION_OUTPUT in str(generated_code)


def test_enforce_bc_kernel_stub_gen():
    ''' Test that the enforce_bc_kernel boundary layer argument modification
    is handled correctly for kernel stubs '''
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
        "map_any_space_1_field_1, boundary_dofs_field_1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1) :: "
        "map_any_space_1_field_1\n"
        "      INTEGER, intent(in) :: undf_any_space_1_field_1\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_any_space_1_field_1)"
        " :: field_1_any_space_1_field_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_field_1,2) :: "
        "boundary_dofs_field_1\n"
        "    END SUBROUTINE enforce_bc_code\n"
        "  END MODULE enforce_bc_mod")
    assert output in str(generated_code)


def test_enforce_op_bc_kernel_stub_gen():
    ''' Test that the enforce_operator_bc_kernel boundary dofs argument
    modification is handled correctly for kernel stubs '''
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
        " op_1, ndf_any_space_1_op_1, ndf_any_space_2_op_1, "
        "boundary_dofs_op_1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_any_space_1_op_1, "
        "ndf_any_space_2_op_1\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension("
        "ndf_any_space_1_op_1,ndf_any_space_2_op_1,op_1_ncell_3d) :: op_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1_op_1,2) :: "
        "boundary_dofs_op_1\n"
        "    END SUBROUTINE enforce_operator_bc_code\n"
        "  END MODULE enforce_operator_bc_mod")
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
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_1_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in str(generated_code)


def test_kernel_stub_usage():
    ''' Check that the kernel-stub generator prints a usage message
    if no arguments are supplied '''
    from subprocess import Popen, STDOUT, PIPE

    usage_msg = (
        "usage: genkernelstub [-h] [-o OUTFILE] [-api API] [-l] filename\n"
        )

    # We use the Popen constructor here rather than check_output because
    # the latter is only available in Python 2.7 onwards.
    out = Popen(['genkernelstub'],
                stdout=PIPE,
                stderr=STDOUT).communicate()[0]
    assert usage_msg in out.decode('utf-8')


def test_kernel_stub_gen_cmd_line():
    ''' Check that we can call the kernel-stub generator from the
    command line '''
    from subprocess import Popen, PIPE
    # We use the Popen constructor here rather than check_output because
    # the latter is only available in Python 2.7 onwards.
    out = Popen(["genkernelstub",
                 os.path.join(BASE_PATH, "dummy_orientation_mod.f90")],
                stdout=PIPE).communicate()[0]

    assert ORIENTATION_OUTPUT in out.decode('utf-8')


def test_stub_stencil_extent():
    ''' Check that correct stub code is produced when there is a stencil
    access '''
    ast = fpapi.parse(os.path.join(BASE_PATH, "testkern_stencil_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    result1 = (
        "SUBROUTINE testkern_stencil_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_dofmap, "
        "field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = "INTEGER, intent(in) :: field_2_stencil_size"
    assert result2 in generated_code
    assert (
        "INTEGER, intent(in), dimension(ndf_w2,field_2_stencil_size) "
        ":: field_2_stencil_dofmap" in generated_code)


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
    result1 = (
        "    SUBROUTINE testkern_stencil_xory1d_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_direction, "
        "field_2_stencil_dofmap, field_3_w2, field_4_w3, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER, intent(in) :: field_2_stencil_size\n"
        "      INTEGER, intent(in) :: field_2_direction\n"
        "      INTEGER, intent(in), dimension(ndf_w2,field_2_stencil_size) :: "
        "field_2_stencil_dofmap")
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
    result1 = (
        "    SUBROUTINE testkern_stencil_vector_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, field_2_w3_v1, field_2_w3_v2, "
        "field_2_w3_v3, field_2_w3_v4, field_2_stencil_size, "
        "field_2_stencil_dofmap, ndf_w0, undf_w0, map_w0, ndf_w3, undf_w3, "
        "map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER, intent(in) :: field_2_stencil_size\n"
        "      INTEGER, intent(in), dimension(ndf_w3,field_2_stencil_size) "
        ":: field_2_stencil_dofmap")
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
    result1 = (
        "    SUBROUTINE testkern_stencil_multi_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_dofmap, field_3_w2,"
        " field_3_stencil_size, field_3_direction, field_3_stencil_dofmap, "
        "field_4_w3, field_4_stencil_size, field_4_stencil_dofmap, ndf_w1, "
        "undf_w1, map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = (
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: "
        "field_4_w3\n"
        "      INTEGER, intent(in) :: field_2_stencil_size, "
        "field_3_stencil_size, field_4_stencil_size\n"
        "      INTEGER, intent(in) :: field_3_direction\n"
        "      INTEGER, intent(in), dimension(ndf_w2,field_2_stencil_size) :: "
        "field_2_stencil_dofmap\n"
        "      INTEGER, intent(in), dimension(ndf_w2,field_3_stencil_size) :: "
        "field_3_stencil_dofmap\n"
        "      INTEGER, intent(in), dimension(ndf_w3,field_4_stencil_size) :: "
        "field_4_stencil_dofmap")

    assert result2 in generated_code
