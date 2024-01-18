# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Author: I. Kavcic, Met Office
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: R. W. Ford, STFC Daresbury Lab
# Modified: O. Brunt and L. Turner, Met Office

'''
Module containing pytest tests for kernel stub code generation and the related
functionality for the LFRic fields.
'''

# Imports
from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.domain.lfric import (LFRicConstants, LFRicKern,
                                   LFRicFields, LFRicKernMetadata)
from psyclone.f2pygen import ModuleGen, SubroutineGen
from psyclone.errors import InternalError


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

# General field checks (argument type, data type, etc)

FIELD_CODE = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), meta_args(6) =                             &
          (/ arg_type(gh_scalar, gh_real,    gh_read),          &
             arg_type(gh_field,  gh_real,    gh_inc,   w1),     &
             arg_type(gh_field,  gh_real,    gh_read,  w2),     &
             arg_type(gh_field,  gh_integer, gh_write, wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,  w3),     &
             arg_type(gh_scalar, gh_integer, gh_read)           &
           /)
     type(func_type), dimension(2) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''


def test_lfricfields_stub_err():
    ''' Check that the LFRicFields constructor raises the expected internal
    error if it encounters an unrecognised intrinsic type of a field
    argument when generating a kernel stub.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(FIELD_CODE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    # Create an empty Kernel stub module and subroutine objects
    psy_module = ModuleGen("testkern_2qr_int_field_mod")
    sub_stub = SubroutineGen(psy_module, name="testkern_2qr_int_field_code",
                             implicitnone=True)
    # Sabotage the field argument to make it have an invalid intrinsic type
    fld_arg = kernel.arguments.args[1]
    fld_arg.descriptor._data_type = "gh_invalid_type"
    print(fld_arg.descriptor._data_type)
    with pytest.raises(InternalError) as err:
        LFRicFields(kernel)._stub_declarations(sub_stub)
    const = LFRicConstants()
    assert (f"Found an unsupported data type 'gh_invalid_type' in "
            f"kernel stub declarations for the field argument 'field_2'. "
            f"Supported types are {const.VALID_FIELD_DATA_TYPES}."
            in str(err.value))


# Tests for kernel stubs containing integer-valued fields


INTEGER_FIELD_CODE = '''
module testkern_int_field_mod
  type, extends(kernel_type) :: testkern_int_field_type
     type(arg_type), meta_args(3) =                             &
         (/ arg_type(gh_field,   gh_integer, gh_write, wtheta), &
            arg_type(gh_field*3, gh_integer, gh_read,  w3),     &
            arg_type(gh_field,   gh_integer, gh_read,  w2trace, &
                                                stencil(cross)) &
          /)
     type(func_type), dimension(2) :: meta_funcs =     &
         (/ func_type(wtheta, gh_basis),               &
            func_type(w3,     gh_basis, gh_diff_basis) &
          /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_int_field_code
  end type testkern_int_field_type
contains
  subroutine testkern_int_field_code()
  end subroutine testkern_int_field_code
end module testkern_int_field_mod
'''


def test_int_field_gen_stub():
    ''' Test that we generate correct code for kernel stubs that
    contain integer-valued fields with stencils and basis/differential
    basis functions.

    '''
    ast = fpapi.parse(INTEGER_FIELD_CODE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE testkern_int_field_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE testkern_int_field_code(nlayers, field_1_wtheta, "
        "field_2_w3_v1, field_2_w3_v2, field_2_w3_v3, field_3_w2trace, "
        "field_3_stencil_size, field_3_stencil_dofmap, ndf_wtheta, "
        "undf_wtheta, map_wtheta, basis_wtheta_qr_xyoz, ndf_w3, undf_w3, "
        "map_w3, basis_w3_qr_xyoz, diff_basis_w3_qr_xyoz, ndf_w2trace, "
        "undf_w2trace, map_w2trace, np_xy_qr_xyoz, np_z_qr_xyoz, "
        "weights_xy_qr_xyoz, weights_z_qr_xyoz)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2trace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2trace) :: "
        "map_w2trace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) :: "
        "map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_wtheta, undf_w3, "
        "undf_w2trace\n"
        "      INTEGER(KIND=i_def), intent(inout), dimension(undf_wtheta) :: "
        "field_1_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w3) :: "
        "field_2_w3_v1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w3) :: "
        "field_2_w3_v2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w3) :: "
        "field_2_w3_v3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w2trace) :: "
        "field_3_w2trace\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_3_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2trace,"
        "field_3_stencil_size) :: field_3_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in) :: "
        "np_xy_qr_xyoz, np_z_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_wtheta,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_wtheta_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: "
        "weights_xy_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: "
        "weights_z_qr_xyoz\n"
        "    END SUBROUTINE testkern_int_field_code\n"
        "  END MODULE testkern_int_field_mod")
    assert output in generated_code


def test_int_field_all_stencils_gen_stub():
    ''' Test that we generate correct code for kernel stubs that
    contain integer-valued fields with all supported stencil accesses. '''
    ast = fpapi.parse(
        os.path.join(BASE_PATH, "testkern_stencil_multi_int_field_mod.f90"),
        ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE testkern_stencil_multi_int_field_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE testkern_stencil_multi_int_field_code(nlayers, "
        "field_1_w2broken, field_2_w1, field_2_stencil_size, "
        "field_2_stencil_dofmap, field_3_w0, field_3_stencil_size, "
        "field_3_direction, field_3_stencil_dofmap, field_4_w2v, "
        "field_4_stencil_size, field_4_stencil_dofmap, ndf_w2broken, "
        "undf_w2broken, map_w2broken, ndf_w1, undf_w1, map_w1, "
        "ndf_w0, undf_w0, map_w0, ndf_w2v, undf_w2v, map_w2v)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2broken\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2broken) :: "
        "map_w2broken\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2v\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v) :: "
        "map_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w2broken, undf_w1, "
        "undf_w0, undf_w2v\n"
        "      INTEGER(KIND=i_def), intent(inout), "
        "dimension(undf_w2broken) :: field_1_w2broken\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w0) :: "
        "field_3_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w2v) :: "
        "field_4_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size, "
        "field_3_stencil_size, field_4_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_3_direction\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1,"
        "field_2_stencil_size) :: field_2_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0,"
        "field_3_stencil_size) :: field_3_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v,"
        "field_4_stencil_size) :: field_4_stencil_dofmap\n"
        "    END SUBROUTINE testkern_stencil_multi_int_field_code\n"
        "  END MODULE testkern_stencil_multi_int_field_mod")
    assert output in generated_code


# Tests for kernel stubs containing real- and integer-valued fields


def test_real_int_field_gen_stub():
    ''' Test that we generate correct code for kernel stubs that
    contain real- and integer-valued fields with basis and differential
    basis functions on one real- and one integer-valued field.

    '''
    code = FIELD_CODE.replace(
        "func_type(w1, gh_basis),",
        "func_type(w1, gh_basis, gh_diff_basis),", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE testkern_field_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE testkern_field_code(nlayers, rscalar_1, field_2_w1, "
        "field_3_w2, field_4_wtheta, field_5_w3, iscalar_6, ndf_w1, undf_w1, "
        "map_w1, basis_w1_qr_xyoz, diff_basis_w1_qr_xyoz, ndf_w2, undf_w2, "
        "map_w2, ndf_wtheta, undf_wtheta, map_wtheta, ndf_w3, undf_w3, "
        "map_w3, basis_w3_qr_xyoz, diff_basis_w3_qr_xyoz, np_xy_qr_xyoz, "
        "np_z_qr_xyoz, weights_xy_qr_xyoz, weights_z_qr_xyoz)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) :: "
        "map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w1, undf_w2, "
        "undf_wtheta, undf_w3\n"
        "      REAL(KIND=r_def), intent(in) :: rscalar_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: iscalar_6\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      INTEGER(KIND=i_def), intent(inout), dimension(undf_wtheta) :: "
        "field_4_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(undf_w3) :: "
        "field_5_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: np_xy_qr_xyoz, "
        "np_z_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w1_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w1_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: "
        "weights_xy_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: "
        "weights_z_qr_xyoz\n"
        "    END SUBROUTINE testkern_field_code\n"
        "  END MODULE testkern_field_mod")
    assert output in generated_code
