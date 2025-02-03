# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Modified I. Kavcic and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the LFRic (Dynamo 0.3) kernel-stub generator using
    pytest. '''

import os
import pytest

import fparser
from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants, LFRicKern, LFRicKernMetadata
from psyclone.errors import GenerationError
from psyclone.gen_kernel_stub import generate

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "lfric"


@pytest.fixture(scope="function", autouse=True)
def setup():
    '''Make sure that all tests here use lfric as API.'''
    Config.get().api = "lfric"


def test_kernel_stub_invalid_iteration_space():
    ''' Check that we raise an exception if we attempt to generate kernel
    stub for a kernel with an unsupported iteration space. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_dofs_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert ("supports kernels that operate on one of "
            "['cell_column', 'halo_cell_column', "
            "'owned_and_halo_cell_column'] but found 'dof' in kernel "
            "'testkern_dofs_code'." in str(excinfo.value))
    kernel._iterates_over = "domain"
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert ("supports kernels that operate on one of "
            "['cell_column', 'halo_cell_column', "
            "'owned_and_halo_cell_column'] but found 'domain' in kernel "
            "'testkern_dofs_code'." in str(excinfo.value))


def test_stub_generate_with_anyw2():
    '''check that the stub generate produces the expected output when we
    have any_w2 fields. In particular, check basis functions as these
    have specific sizes associated with the particular function space'''
    result = generate(os.path.join(BASE_PATH,
                                   "testkern_multi_anyw2_basis_mod.f90"),
                      api=TEST_API)
    expected_output = (
        "    real(kind=r_def), dimension(3,ndf_any_w2,np_xy_qr_xyoz,"
        "np_z_qr_xyoz), intent(in) :: basis_any_w2_qr_xyoz\n"
        "    real(kind=r_def), dimension(1,ndf_any_w2,np_xy_qr_xyoz,"
        "np_z_qr_xyoz), intent(in) :: diff_basis_any_w2_qr_xyoz")
    assert expected_output in result


SIMPLE = (
    "module simple_mod\n"
    "  implicit none\n"
    "  public\n"
    "\n"
    "  contains\n"
    "  subroutine simple_code(nlayers, field_1_w1, ndf_w1, undf_w1,"
    " map_w1)\n"
    "    use constants_mod\n"
    "    integer(kind=i_def), intent(in) :: nlayers\n"
    "    integer(kind=i_def), intent(in) :: ndf_w1\n"
    "    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1\n"
    "    integer(kind=i_def), intent(in) :: undf_w1\n"
    "    real(kind=r_def), dimension(undf_w1), intent(inout) :: "
    "field_1_w1\n"
    "\n"
    "\n"
    "  end subroutine simple_code\n"
    "\n"
    "end module simple_mod\n")


def test_stub_generate_working():
    ''' Check that the stub generate produces the expected output '''
    result = generate(os.path.join(BASE_PATH, "testkern_simple_mod.f90"),
                      api=TEST_API)
    assert SIMPLE == result


# Fields : intent
INTENT = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(3) =                     &
          (/ arg_type(gh_field, gh_real, gh_write, w3), &
             arg_type(gh_field, gh_real, gh_inc,   w1), &
             arg_type(gh_field, gh_real, gh_read,  w1)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_load_meta_wrong_type():
    ''' Test that the load_meta function raises an appropriate error
    if the metadata contains an un-recognised type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    # Break the metadata
    metadata.arg_descriptors[0]._argument_type = "gh_hedge"
    with pytest.raises(GenerationError) as excinfo:
        kernel.load_meta(metadata)
    const = LFRicConstants()
    assert (f"LFRicKern.load_meta() expected one of "
            f"{const.VALID_ARG_TYPE_NAMES} but found "
            f"'gh_hedge'" in str(excinfo.value))


def test_intent(fortran_writer):
    ''' test that field intent is generated correctly for kernel stubs '''
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "module dummy_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine dummy_code(nlayers, field_1_w3, field_2_w1, "
        "field_3_w1, ndf_w3, undf_w3, map_w3, ndf_w1, undf_w1, map_w1)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w1\n"
        "    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1\n"
        "    integer(kind=i_def), intent(in) :: ndf_w3\n"
        "    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3\n"
        "    integer(kind=i_def), intent(in) :: undf_w3\n"
        "    integer(kind=i_def), intent(in) :: undf_w1\n"
        "    real(kind=r_def), dimension(undf_w3), intent(inout) :: "
        "field_1_w3\n"
        "    real(kind=r_def), dimension(undf_w1), intent(inout) :: "
        "field_2_w1\n"
        "    real(kind=r_def), dimension(undf_w1), intent(in) :: "
        "field_3_w1\n"
        "\n"
        "\n"
        "  end subroutine dummy_code\n"
        "\n"
        "end module dummy_mod\n")
    assert output == fortran_writer(generated_code)


# Fields : spaces
SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(12) =                          &
          (/ arg_type(gh_field, gh_real, gh_inc,   w0),       &
             arg_type(gh_field, gh_real, gh_inc,   w1),       &
             arg_type(gh_field, gh_real, gh_inc,   w2),       &
             arg_type(gh_field, gh_real, gh_write, w2broken), &
             arg_type(gh_field, gh_real, gh_inc,   w2trace),  &
             arg_type(gh_field, gh_real, gh_write, w3),       &
             arg_type(gh_field, gh_real, gh_write, wtheta),   &
             arg_type(gh_field, gh_real, gh_inc,   w2h),      &
             arg_type(gh_field, gh_real, gh_write, w2v),      &
             arg_type(gh_field, gh_real, gh_inc,   w2htrace), &
             arg_type(gh_field, gh_real, gh_write, w2vtrace), &
             arg_type(gh_field, gh_real, gh_read,  wchi)      &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_spaces(fortran_writer):
    ''' Test that field spaces are handled correctly for kernel stubs.

    '''
    ast = fpapi.parse(SPACES, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    output = (
        "module dummy_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine dummy_code(nlayers, field_1_w0, field_2_w1, "
        "field_3_w2, field_4_w2broken, field_5_w2trace, field_6_w3, "
        "field_7_wtheta, field_8_w2h, field_9_w2v, field_10_w2htrace, "
        "field_11_w2vtrace, field_12_wchi, "
        "ndf_w0, undf_w0, map_w0, ndf_w1, undf_w1, map_w1, "
        "ndf_w2, undf_w2, map_w2, ndf_w2broken, undf_w2broken, map_w2broken, "
        "ndf_w2trace, undf_w2trace, map_w2trace, ndf_w3, undf_w3, map_w3, "
        "ndf_wtheta, undf_wtheta, map_wtheta, ndf_w2h, undf_w2h, map_w2h, "
        "ndf_w2v, undf_w2v, map_w2v, ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace, ndf_w2vtrace, undf_w2vtrace, map_w2vtrace, "
        "ndf_wchi, undf_wchi, map_wchi)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w0\n"
        "    integer(kind=i_def), dimension(ndf_w0), intent(in) :: map_w0\n"
        "    integer(kind=i_def), intent(in) :: ndf_w1\n"
        "    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2\n"
        "    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2broken\n"
        "    integer(kind=i_def), dimension(ndf_w2broken), intent(in) "
        ":: map_w2broken\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2h\n"
        "    integer(kind=i_def), dimension(ndf_w2h), intent(in) "
        ":: map_w2h\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2htrace\n"
        "    integer(kind=i_def), dimension(ndf_w2htrace), intent(in) "
        ":: map_w2htrace\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2trace\n"
        "    integer(kind=i_def), dimension(ndf_w2trace), intent(in) "
        ":: map_w2trace\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2v\n"
        "    integer(kind=i_def), dimension(ndf_w2v), intent(in) "
        ":: map_w2v\n"
        "    integer(kind=i_def), intent(in) :: ndf_w2vtrace\n"
        "    integer(kind=i_def), dimension(ndf_w2vtrace), intent(in) "
        ":: map_w2vtrace\n"
        "    integer(kind=i_def), intent(in) :: ndf_w3\n"
        "    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3\n"
        "    integer(kind=i_def), intent(in) :: ndf_wchi\n"
        "    integer(kind=i_def), dimension(ndf_wchi), intent(in) "
        ":: map_wchi\n"
        "    integer(kind=i_def), intent(in) :: ndf_wtheta\n"
        "    integer(kind=i_def), dimension(ndf_wtheta), intent(in) "
        ":: map_wtheta\n"
        "    integer(kind=i_def), intent(in) :: undf_w0\n"
        "    integer(kind=i_def), intent(in) :: undf_w1\n"
        "    integer(kind=i_def), intent(in) :: undf_w2\n"
        "    integer(kind=i_def), intent(in) :: undf_w2broken\n"
        "    integer(kind=i_def), intent(in) :: undf_w2trace\n"
        "    integer(kind=i_def), intent(in) :: undf_w3\n"
        "    integer(kind=i_def), intent(in) :: undf_wtheta\n"
        "    integer(kind=i_def), intent(in) :: undf_w2h\n"
        "    integer(kind=i_def), intent(in) :: undf_w2v\n"
        "    integer(kind=i_def), intent(in) :: undf_w2htrace\n"
        "    integer(kind=i_def), intent(in) :: undf_w2vtrace\n"
        "    integer(kind=i_def), intent(in) :: undf_wchi\n"
        "    real(kind=r_def), dimension(undf_w0), intent(inout) "
        ":: field_1_w0\n"
        "    real(kind=r_def), dimension(undf_w1), intent(inout) "
        ":: field_2_w1\n"
        "    real(kind=r_def), dimension(undf_w2), intent(inout) "
        ":: field_3_w2\n"
        "    real(kind=r_def), dimension(undf_w2broken), intent(inout) "
        ":: field_4_w2broken\n"
        "    real(kind=r_def), dimension(undf_w2trace), intent(inout) "
        ":: field_5_w2trace\n"
        "    real(kind=r_def), dimension(undf_w3), intent(inout) "
        ":: field_6_w3\n"
        "    real(kind=r_def), dimension(undf_wtheta), intent(inout) "
        ":: field_7_wtheta\n"
        "    real(kind=r_def), dimension(undf_w2h), intent(inout) "
        ":: field_8_w2h\n"
        "    real(kind=r_def), dimension(undf_w2v), intent(inout) "
        ":: field_9_w2v\n"
        "    real(kind=r_def), dimension(undf_w2htrace), intent(inout) "
        ":: field_10_w2htrace\n"
        "    real(kind=r_def), dimension(undf_w2vtrace), intent(inout) "
        ":: field_11_w2vtrace\n"
        "    real(kind=r_def), dimension(undf_wchi), intent(in) "
        ":: field_12_wchi\n"
        "\n"
        "\n"
        "  end subroutine dummy_code\n"
        "\n"
        "end module dummy_mod\n")
    assert output == generated_code


ANY_SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(3) =                                  &
          (/ arg_type(gh_field, gh_real, gh_read,                    &
                                         any_discontinuous_space_1), &
             arg_type(gh_field, gh_real, gh_inc,       any_space_7), &
             arg_type(gh_field, gh_real, gh_readwrite,               &
                                         any_discontinuous_space_4)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_any_spaces(fortran_writer):
    ''' Test that any_space and any_discontinuous_space metadata are handled
    correctly for kernel stubs.

    '''
    ast = fpapi.parse(ANY_SPACES, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    output = (
        "module dummy_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine dummy_code(nlayers, field_1_adspc1_field_1, "
        "field_2_aspc7_field_2, field_3_adspc4_field_3, "
        "ndf_adspc1_field_1, undf_adspc1_field_1, map_adspc1_field_1, "
        "ndf_aspc7_field_2, undf_aspc7_field_2, map_aspc7_field_2, "
        "ndf_adspc4_field_3, undf_adspc4_field_3, map_adspc4_field_3)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_adspc1_field_1\n"
        "    integer(kind=i_def), dimension("
        "ndf_adspc1_field_1), intent(in) :: map_adspc1_field_1\n"
        "    integer(kind=i_def), intent(in) :: ndf_adspc4_field_3\n"
        "    integer(kind=i_def), dimension("
        "ndf_adspc4_field_3), intent(in) :: map_adspc4_field_3\n"
        "    integer(kind=i_def), intent(in) :: ndf_aspc7_field_2\n"
        "    integer(kind=i_def), "
        "dimension(ndf_aspc7_field_2), intent(in) :: map_aspc7_field_2\n"
        "    integer(kind=i_def), intent(in) :: undf_adspc1_field_1\n"
        "    integer(kind=i_def), intent(in) :: undf_aspc7_field_2\n"
        "    integer(kind=i_def), intent(in) :: undf_adspc4_field_3\n"
        "    real(kind=r_def), dimension"
        "(undf_adspc1_field_1), intent(in) :: field_1_adspc1_field_1\n"
        "    real(kind=r_def), dimension"
        "(undf_aspc7_field_2), intent(inout) :: field_2_aspc7_field_2\n"
        "    real(kind=r_def), dimension"
        "(undf_adspc4_field_3), intent(inout) :: field_3_adspc4_field_3\n"
        "\n"
        "\n"
        "  end subroutine dummy_code\n"
        "\n"
        "end module dummy_mod\n")
    assert output == generated_code


# Fields : vectors
VECTORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =                    &
          (/ arg_type(gh_field*3, gh_real, gh_inc, w0) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_vectors(fortran_writer):
    ''' test that field vectors are handled correctly for kernel stubs '''
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    output = (
        "module dummy_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine dummy_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, ndf_w0, undf_w0, map_w0)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w0\n"
        "    integer(kind=i_def), dimension(ndf_w0), intent(in) :: map_w0\n"
        "    integer(kind=i_def), intent(in) :: undf_w0\n"
        "    real(kind=r_def), dimension(undf_w0), intent(inout) :: "
        "field_1_w0_v1\n"
        "    real(kind=r_def), dimension(undf_w0), intent(inout) :: "
        "field_1_w0_v2\n"
        "    real(kind=r_def), dimension(undf_w0), intent(inout) :: "
        "field_1_w0_v3\n"
        "\n"
        "\n"
        "  end subroutine dummy_code\n"
        "\n"
        "end module dummy_mod\n")
    assert output in generated_code


def test_arg_descriptor_vec_str():
    ''' Tests that the string method for LFRicArgDescriptor works as
    expected when we have a vector quantity. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_inc'\n"
        "  function_space[3]='w0'")
    assert expected_output in result


def test_enforce_bc_kernel_stub_gen(fortran_writer):
    ''' Test that the enforce_bc_kernel boundary layer argument modification
    is handled correctly for kernel stubs.

    '''
    ast = fpapi.parse(os.path.join(BASE_PATH, "enforce_bc_kernel_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    output = (
        "module enforce_bc_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine enforce_bc_code(nlayers, field_1_aspc1_field_1, "
        "ndf_aspc1_field_1, undf_aspc1_field_1, map_aspc1_field_1, "
        "boundary_dofs_field_1)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_aspc1_field_1\n"
        "    integer(kind=i_def), "
        "dimension(ndf_aspc1_field_1), intent(in) :: map_aspc1_field_1\n"
        "    integer(kind=i_def), intent(in) :: undf_aspc1_field_1\n"
        "    real(kind=r_def), "
        "dimension(undf_aspc1_field_1), intent(inout) :: field_1_aspc1_field_1"
        "\n"
        "    integer(kind=i_def), "
        "dimension(ndf_aspc1_field_1,2), intent(in) :: boundary_dofs_field_1\n"
        "\n"
        "\n"
        "  end subroutine enforce_bc_code\n"
        "\n"
        "end module enforce_bc_mod\n")
    assert output == generated_code


def test_enforce_op_bc_kernel_stub_gen(fortran_writer):
    ''' Test that the enforce_operator_bc_kernel boundary dofs argument
    modification is handled correctly for kernel stubs.

    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "enforce_operator_bc_kernel_mod.F90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    output = (
        "module enforce_operator_bc_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine enforce_operator_bc_code(cell, nlayers, "
        "op_1_ncell_3d, op_1, ndf_aspc1_op_1, ndf_aspc2_op_1, "
        "boundary_dofs_op_1)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_aspc1_op_1\n"
        "    integer(kind=i_def), intent(in) :: ndf_aspc2_op_1\n"
        "    integer(kind=i_def), intent(in) :: cell\n"
        "    integer(kind=i_def), intent(in) :: op_1_ncell_3d\n"
        "    real(kind=r_def), dimension("
        "op_1_ncell_3d,ndf_aspc1_op_1,ndf_aspc2_op_1), intent(inout) :: op_1\n"
        "    integer(kind=i_def), "
        "dimension(ndf_aspc1_op_1,2), intent(in) :: boundary_dofs_op_1\n"
        "\n"
        "\n"
        "  end subroutine enforce_operator_bc_code\n"
        "\n"
        "end module enforce_operator_bc_mod\n")
    assert output == generated_code


def test_multi_qr_stub_gen(fortran_writer):
    ''' Test that the stub generator correctly handles a kernel requiring
    more than one quadrature rule. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_2qr_mod.F90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    assert ("subroutine testkern_2qr_code(nlayers, field_1_w1, field_2_w2, "
            "field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, "
            "basis_w1_qr_face, basis_w1_qr_edge, ndf_w2, undf_w2, map_w2, "
            "diff_basis_w2_qr_face, diff_basis_w2_qr_edge, ndf_w3, undf_w3, "
            "map_w3, basis_w3_qr_face, basis_w3_qr_edge, "
            "diff_basis_w3_qr_face, diff_basis_w3_qr_edge, nfaces_qr_face, "
            "np_xyz_qr_face, weights_xyz_qr_face, nedges_qr_edge, "
            "np_xyz_qr_edge, weights_xyz_qr_edge)" in generated_code)
    assert ("    integer(kind=i_def), intent(in) :: np_xyz_qr_face\n"
            "    integer(kind=i_def), intent(in) :: nfaces_qr_face\n"
            "    integer(kind=i_def), intent(in) :: np_xyz_qr_edge\n"
            "    integer(kind=i_def), intent(in) :: nedges_qr_edge\n"
            in generated_code)
    assert (
        "    real(kind=r_def), dimension(3,ndf_w1,"
        "np_xyz_qr_face,nfaces_qr_face), intent(in) :: basis_w1_qr_face\n"
        "    real(kind=r_def), dimension(3,ndf_w1,"
        "np_xyz_qr_edge,nedges_qr_edge), intent(in) :: basis_w1_qr_edge\n"
        "    real(kind=r_def), dimension(1,ndf_w2,"
        "np_xyz_qr_face,nfaces_qr_face), intent(in) :: diff_basis_w2_qr_face\n"
        "    real(kind=r_def), dimension(1,ndf_w2,"
        "np_xyz_qr_edge,nedges_qr_edge), intent(in) :: diff_basis_w2_qr_edge\n"
        "    real(kind=r_def), dimension(1,ndf_w3,"
        "np_xyz_qr_face,nfaces_qr_face), intent(in) :: basis_w3_qr_face\n"
        "    real(kind=r_def), dimension(3,ndf_w3,"
        "np_xyz_qr_face,nfaces_qr_face), intent(in) :: diff_basis_w3_qr_face\n"
        "    real(kind=r_def), dimension(1,ndf_w3,"
        "np_xyz_qr_edge,nedges_qr_edge), intent(in) :: basis_w3_qr_edge\n"
        "    real(kind=r_def), dimension(3,ndf_w3,"
        "np_xyz_qr_edge,nedges_qr_edge), intent(in) :: diff_basis_w3_qr_edge"
        in generated_code)
    assert ("    real(kind=r_def), dimension(np_xyz_qr_face,"
            "nfaces_qr_face), intent(in) :: weights_xyz_qr_face\n"
            "    real(kind=r_def), dimension(np_xyz_qr_edge,"
            "nedges_qr_edge), intent(in) :: weights_xyz_qr_edge\n"
            in generated_code)


def test_qr_plus_eval_stub_gen(fortran_writer):
    ''' Test the stub generator for a kernel that requires both an evaluator
    and quadrature. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_qr_eval_mod.F90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    code = fortran_writer(kernel.gen_stub)
    assert (
        "subroutine testkern_qr_eval_code(nlayers, field_1_w1, field_2_w2,"
        " field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, basis_w1_qr_face, "
        "basis_w1_on_w1, ndf_w2, undf_w2, map_w2, diff_basis_w2_qr_face, "
        "diff_basis_w2_on_w1, ndf_w3, undf_w3, map_w3, basis_w3_qr_face, "
        "basis_w3_on_w1, diff_basis_w3_qr_face, diff_basis_w3_on_w1, "
        "nfaces_qr_face, np_xyz_qr_face, weights_xyz_qr_face)"
        in code)
    assert ("    integer(kind=i_def), intent(in) :: np_xyz_qr_face\n"
            "    integer(kind=i_def), intent(in) :: nfaces_qr_face\n"
            in code)
    assert (
        "    real(kind=r_def), dimension(3,ndf_w1,np_xyz_qr_face"
        ",nfaces_qr_face), intent(in) :: basis_w1_qr_face\n"
        "    real(kind=r_def), dimension(3,ndf_w1,ndf_w1), intent(in) :: "
        "basis_w1_on_w1\n"
        "    real(kind=r_def), dimension(1,ndf_w2,np_xyz_qr_face"
        ",nfaces_qr_face), intent(in) :: diff_basis_w2_qr_face\n"
        "    real(kind=r_def), dimension(1,ndf_w2,ndf_w1), intent(in) :: "
        "diff_basis_w2_on_w1\n"
        "    real(kind=r_def), dimension(1,ndf_w3,np_xyz_qr_face"
        ",nfaces_qr_face), intent(in) :: basis_w3_qr_face\n"
        "    real(kind=r_def), dimension(3,ndf_w3,np_xyz_qr_face"
        ",nfaces_qr_face), intent(in) :: diff_basis_w3_qr_face\n"
        "    real(kind=r_def), dimension(1,ndf_w3,ndf_w1), intent(in) :: "
        "basis_w3_on_w1\n"
        "    real(kind=r_def), dimension(3,ndf_w3,ndf_w1), intent(in) :: "
        "diff_basis_w3_on_w1\n"
        "    real(kind=r_def), dimension(np_xyz_qr_face,"
        "nfaces_qr_face), intent(in) :: weights_xyz_qr_face" in code)


SUB_NAME = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =                  &
          (/ arg_type(gh_field, gh_real, gh_inc, w1) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy
  end type dummy_type
contains
  subroutine dummy()
  end subroutine dummy
end module dummy_mod
'''


def test_sub_name(fortran_writer):
    ''' test for expected behaviour when the kernel subroutine does
    not conform to the convention of having "_code" at the end of its
    name. In this case we append "_code to the name and _mod to the
    kernel name.'''
    ast = fpapi.parse(SUB_NAME, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    output = (
        "module dummy_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine dummy_code(nlayers, field_1_w1, "
        "ndf_w1, undf_w1, map_w1)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w1\n"
        "    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1\n"
        "    integer(kind=i_def), intent(in) :: undf_w1\n"
        "    real(kind=r_def), dimension(undf_w1), intent(inout) :: "
        "field_1_w1\n"
        "\n"
        "\n"
        "  end subroutine dummy_code\n"
        "\n"
        "end module dummy_mod\n")
    assert output == generated_code
