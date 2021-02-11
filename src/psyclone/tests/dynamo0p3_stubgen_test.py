# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the Dynamo 0.3 kernel-stub generator using pytest. '''

# imports
from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, LFRicScalarArgs
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.utils import ParseError
from psyclone.gen_kernel_stub import generate
from psyclone.f2pygen import ModuleGen

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"


def test_kernel_stub_invalid_iteration_space():
    ''' Check that we raise an exception if we attempt to generate kernel
    stub for a kernel with an unsupported iteration space. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_dofs_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert ("supports kernels that operate on one of "
            "['cells', 'cell_column'] but found 'dof' in kernel "
            "'testkern_dofs_code'." in str(excinfo.value))
    kernel._iterates_over = "domain"
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert ("supports kernels that operate on one of "
            "['cells', 'cell_column'] but found 'domain' in kernel "
            "'testkern_dofs_code'." in str(excinfo.value))


def test_lfricscalars_stub_err():
    ''' Check that LFRicScalarArgs._stub_declarations() raises the
    expected internal error if it encounters an unrecognised data
    type of a scalar argument when generating a kernel stub.

    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    # Sabotage the scalar argument to make it have an invalid data type
    arg = kernel.arguments.args[1]
    arg.descriptor._data_type = "gh_invalid_scalar"
    with pytest.raises(InternalError) as err:
        LFRicScalarArgs(kernel)._stub_declarations(ModuleGen(name="my_mod"))
    assert ("Found an unsupported data type 'gh_invalid_scalar' for the "
            "scalar argument 'iscalar_2'. Supported types are {0}.".
            format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES)
            in str(err.value))


def test_stub_generate_with_anyw2():
    '''check that the stub generate produces the expected output when we
    have any_w2 fields. In particular, check basis functions as these
    have specific sizes associated with the particular function space'''
    result = generate(os.path.join(BASE_PATH,
                                   "testkern_multi_anyw2_basis_mod.f90"),
                      api=TEST_API)
    expected_output = (
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_any_w2,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_any_w2_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_any_w2,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_any_w2_qr_xyoz")
    assert expected_output in str(result)


SIMPLE = (
    "  MODULE simple_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE simple_code(nlayers, field_1_w1, ndf_w1, undf_w1,"
    " map_w1)\n"
    "      USE constants_mod, ONLY: r_def, i_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
    "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
    "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
    "      INTEGER(KIND=i_def), intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
    "field_1_w1\n"
    "    END SUBROUTINE simple_code\n"
    "  END MODULE simple_mod")


def test_stub_generate_working():
    ''' Check that the stub generate produces the expected output '''
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
    "      USE constants_mod, ONLY: r_def, i_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
    "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
    "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
    "      INTEGER(KIND=i_def), intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(in) :: rscalar_1\n"
    "      INTEGER(KIND=i_def), intent(in) :: iscalar_3\n"
    "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) ::"
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
    "      USE constants_mod, ONLY: r_def, i_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
    "      REAL(KIND=r_def), intent(inout) :: rscalar_1\n"
    "      INTEGER(KIND=i_def), intent(inout) :: iscalar_2\n"
    "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
    "      INTEGER(KIND=i_def), intent(in) :: undf_w3\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w3) :: field_3_w3\n"
    "      REAL(KIND=r_def), intent(inout) :: rscalar_4\n"
    "      INTEGER(KIND=i_def), intent(inout) :: iscalar_5\n"
    "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
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
        "A user-supplied LFRic kernel must not write/update a scalar "
        "argument but kernel 'simple_with_reduction_type' has a scalar "
        "argument with 'gh_sum' access." in str(err.value))


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
    if the meta-data contains an un-recognised type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    # Break the meta-data
    metadata.arg_descriptors[0]._argument_type = "gh_hedge"
    with pytest.raises(GenerationError) as excinfo:
        kernel.load_meta(metadata)
    assert ("DynKern.load_meta() expected one of {0} but found "
            "'gh_hedge'".format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
            in str(excinfo.value))


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
        "    SUBROUTINE dummy_code(nlayers, field_1_w3, field_2_w1, "
        "field_3_w1, ndf_w3, undf_w3, map_w3, ndf_w1, undf_w1, map_w1)\n"
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w3, undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w3) :: "
        "field_1_w3\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w1) :: "
        "field_3_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in str(generated_code)


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


def test_spaces():
    ''' Test that field spaces are handled correctly for kernel stubs.

    '''
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
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2broken\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2broken) "
        ":: map_w2broken\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2h\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2h) "
        ":: map_w2h\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2htrace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2htrace) "
        ":: map_w2htrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2trace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2trace) "
        ":: map_w2trace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2v\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v) "
        ":: map_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2vtrace) "
        ":: map_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wchi\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wchi) "
        ":: map_wchi\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) "
        ":: map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0, undf_w1, undf_w2, "
        "undf_w2broken, undf_w2trace, undf_w3, undf_wtheta, undf_w2h, "
        "undf_w2v, undf_w2htrace, undf_w2vtrace, undf_wchi\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) "
        ":: field_1_w0\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) "
        ":: field_2_w1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w2) "
        ":: field_3_w2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2broken) "
        ":: field_4_w2broken\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w2trace) "
        ":: field_5_w2trace\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w3) "
        ":: field_6_w3\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_wtheta) "
        ":: field_7_wtheta\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w2h) "
        ":: field_8_w2h\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2v) "
        ":: field_9_w2v\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w2htrace) "
        ":: field_10_w2htrace\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2vtrace) "
        ":: field_11_w2vtrace\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_wchi) "
        ":: field_12_wchi\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in generated_code


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


def test_any_spaces():
    ''' Test that any_space and any_discontinuous_space metadata are handled
    correctly for kernel stubs.

    '''
    ast = fpapi.parse(ANY_SPACES, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_adspc1_field_1, "
        "field_2_aspc7_field_2, field_3_adspc4_field_3, "
        "ndf_adspc1_field_1, undf_adspc1_field_1, map_adspc1_field_1, "
        "ndf_aspc7_field_2, undf_aspc7_field_2, map_aspc7_field_2, "
        "ndf_adspc4_field_3, undf_adspc4_field_3, map_adspc4_field_3)\n"
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_adspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension("
        "ndf_adspc1_field_1) :: map_adspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_adspc4_field_3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension("
        "ndf_adspc4_field_3) :: map_adspc4_field_3\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc7_field_2\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc7_field_2) :: map_aspc7_field_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_adspc1_field_1, "
        "undf_aspc7_field_2, undf_adspc4_field_3\n"
        "      REAL(KIND=r_def), intent(in), dimension"
        "(undf_adspc1_field_1) :: field_1_adspc1_field_1\n"
        "      REAL(KIND=r_def), intent(inout), dimension"
        "(undf_aspc7_field_2) :: field_2_aspc7_field_2\n"
        "      REAL(KIND=r_def), intent(inout), dimension"
        "(undf_adspc4_field_3) :: field_3_adspc4_field_3\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in generated_code


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
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0_v1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0_v2\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0_v3\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in str(generated_code)


def test_arg_descriptor_vec_str():
    ''' Tests that the string method for LFRicArgDescriptor works as
    expected when we have a vector quantity. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_inc'\n"
        "  function_space[3]='w0'")
    assert expected_output in result


def test_enforce_bc_kernel_stub_gen():
    ''' Test that the enforce_bc_kernel boundary layer argument modification
    is handled correctly for kernel stubs.

    '''
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
        "    SUBROUTINE enforce_bc_code(nlayers, field_1_aspc1_field_1, "
        "ndf_aspc1_field_1, undf_aspc1_field_1, map_aspc1_field_1, "
        "boundary_dofs_field_1)\n"
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_field_1) :: map_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_aspc1_field_1\n"
        "      REAL(KIND=r_def), intent(inout), "
        "dimension(undf_aspc1_field_1) :: field_1_aspc1_field_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_field_1,2) :: boundary_dofs_field_1\n"
        "    END SUBROUTINE enforce_bc_code\n"
        "  END MODULE enforce_bc_mod")
    assert output in str(generated_code)


def test_enforce_op_bc_kernel_stub_gen():
    ''' Test that the enforce_operator_bc_kernel boundary dofs argument
    modification is handled correctly for kernel stubs.

    '''
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
        "    SUBROUTINE enforce_operator_bc_code(cell, nlayers, "
        "op_1_ncell_3d, op_1, ndf_aspc1_op_1, ndf_aspc2_op_1, "
        "boundary_dofs_op_1)\n"
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_aspc1_op_1, "
        "ndf_aspc2_op_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension("
        "ndf_aspc1_op_1,ndf_aspc2_op_1,op_1_ncell_3d) :: op_1\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_aspc1_op_1,2) :: boundary_dofs_op_1\n"
        "    END SUBROUTINE enforce_operator_bc_code\n"
        "  END MODULE enforce_operator_bc_mod")
    assert output in generated_code


def test_multi_qr_stub_gen():
    ''' Test that the stub generator correctly handles a kernel requiring
    more than one quadrature rule. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_2qr_mod.F90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    assert ("SUBROUTINE testkern_2qr_code(nlayers, field_1_w1, field_2_w2, "
            "field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, "
            "basis_w1_qr_face, basis_w1_qr_edge, ndf_w2, undf_w2, map_w2, "
            "diff_basis_w2_qr_face, diff_basis_w2_qr_edge, ndf_w3, undf_w3, "
            "map_w3, basis_w3_qr_face, basis_w3_qr_edge, "
            "diff_basis_w3_qr_face, diff_basis_w3_qr_edge, nfaces_qr_face, "
            "np_xyz_qr_face, weights_xyz_qr_face, nedges_qr_edge, "
            "np_xyz_qr_edge, weights_xyz_qr_edge)" in generated_code)
    assert ("INTEGER(KIND=i_def), intent(in) :: np_xyz_qr_face, "
            "nfaces_qr_face, np_xyz_qr_edge, nedges_qr_edge" in generated_code)
    assert (
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
        "np_xyz_qr_face,nfaces_qr_face) :: basis_w1_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
        "np_xyz_qr_edge,nedges_qr_edge) :: basis_w1_qr_edge\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,"
        "np_xyz_qr_face,nfaces_qr_face) :: diff_basis_w2_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,"
        "np_xyz_qr_edge,nedges_qr_edge) :: diff_basis_w2_qr_edge\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,"
        "np_xyz_qr_face,nfaces_qr_face) :: basis_w3_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,"
        "np_xyz_qr_face,nfaces_qr_face) :: diff_basis_w3_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,"
        "np_xyz_qr_edge,nedges_qr_edge) :: basis_w3_qr_edge\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,"
        "np_xyz_qr_edge,nedges_qr_edge) :: diff_basis_w3_qr_edge"
        in generated_code)
    assert ("      REAL(KIND=r_def), intent(in), dimension(np_xyz_qr_face,"
            "nfaces_qr_face) :: weights_xyz_qr_face\n"
            "      REAL(KIND=r_def), intent(in), dimension(np_xyz_qr_edge,"
            "nedges_qr_edge) :: weights_xyz_qr_edge\n" in generated_code)


def test_qr_plus_eval_stub_gen():
    ''' Test the stub generator for a kernel that requires both an evaluator
    and quadrature. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_qr_eval_mod.F90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    gen_code = str(kernel.gen_stub)
    assert (
        "SUBROUTINE testkern_qr_eval_code(nlayers, field_1_w1, field_2_w2,"
        " field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, basis_w1_qr_face, "
        "basis_w1_on_w1, ndf_w2, undf_w2, map_w2, diff_basis_w2_qr_face, "
        "diff_basis_w2_on_w1, ndf_w3, undf_w3, map_w3, basis_w3_qr_face, "
        "basis_w3_on_w1, diff_basis_w3_qr_face, diff_basis_w3_on_w1, "
        "nfaces_qr_face, np_xyz_qr_face, weights_xyz_qr_face)"
        in gen_code)
    assert ("INTEGER(KIND=i_def), intent(in) :: np_xyz_qr_face, nfaces_qr_face"
            in gen_code)
    assert (
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,np_xyz_qr_face"
        ",nfaces_qr_face) :: basis_w1_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,ndf_w1) :: "
        "basis_w1_on_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,np_xyz_qr_face"
        ",nfaces_qr_face) :: diff_basis_w2_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,ndf_w1) :: "
        "diff_basis_w2_on_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,np_xyz_qr_face"
        ",nfaces_qr_face) :: basis_w3_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,np_xyz_qr_face"
        ",nfaces_qr_face) :: diff_basis_w3_qr_face\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,ndf_w1) :: "
        "basis_w3_on_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,ndf_w1) :: "
        "diff_basis_w3_on_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xyz_qr_face,"
        "nfaces_qr_face) :: weights_xyz_qr_face" in gen_code)


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
        "      USE constants_mod, ONLY: r_def, i_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
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
                 os.path.join(BASE_PATH, "simple_with_scalars.f90")],
                stdout=PIPE).communicate()[0]

    assert SIMPLE_WITH_SCALARS in out.decode('utf-8')


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
    result2 = "INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size"
    assert result2 in generated_code
    assert (
        "INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_2_stencil_size) :: field_2_stencil_dofmap"
        in generated_code)


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
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_2_direction\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_2_stencil_size) :: field_2_stencil_dofmap")
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
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w3,field_2_stencil_size) :: field_2_stencil_dofmap")
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
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size, "
        "field_3_stencil_size, field_4_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_3_direction\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_2_stencil_size) :: field_2_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_3_stencil_size) :: field_3_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w3,field_4_stencil_size) :: field_4_stencil_dofmap")

    assert result2 in generated_code
