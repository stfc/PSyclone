# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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

'''
A module to perform pytest tests on the
psyad/domain/lfric/lfric_adjoint.py file.

'''
import pytest

from psyclone.errors import InternalError
from psyclone.psyad.domain.lfric import generate_lfric_adjoint


def test_generate_lfric_adjoint_no_container_error(fortran_reader):
    '''
    Check that generate_lfric_adjoint raises the expected error when
    provided PSyIR that does not have a Container.

    '''
    psyir = fortran_reader.psyir_from_source("""\
program test
  implicit none
  integer :: var1, var2
  var1 = var2
end program test
""")
    with pytest.raises(InternalError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert ("An LFRic kernel must be within a Container but the supplied "
            "PSyIR does not contain one." in str(err.value))


def test_generate_lfric_adjoint_no_routines_error(fortran_reader):
    '''
    Check that generate_lfric_adjoint raises the expected error when
    provided PSyIR that does not contain any Routines.

    '''
    psyir = fortran_reader.psyir_from_source("""\
module test_mod
  implicit none
  integer :: var1
end module test_mod
""")
    with pytest.raises(InternalError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert "The supplied PSyIR does not contain any routines" in str(err.value)


def test_generate_lfric_adjoint_multi_kernel(fortran_reader, fortran_writer):
    '''Check that generate_lfric_adjoint creates the expected code when there
    are multiple kernels in a module.

    '''
    tl_fortran_str = (
        "module tl_test_mod\n"
        "  use kernel_mod\n"
        "  type, public, extends(kernel_type) :: tl_test_type\n"
        "    type(arg_type) :: meta_args(1) =         &\n"
        "         (/ arg_type(gh_field, gh_real, gh_readwrite, w3) /)\n"
        "    integer :: operates_on = CELL_COLUMN\n"
        "  end type\n\n"
        "  contains\n"
        "  subroutine kern1()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern1\n"
        "  subroutine kern2()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern2\n"
        "  subroutine kern3()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern3\n"
        "end module tl_test_mod\n")
    expected = (
        "module adj_test_mod\n"
        "  use kernel_mod\n"
        "  implicit none\n"
        "  type, public, extends(kernel_type) :: adj_test_type\n"
        "  type(ARG_TYPE) :: META_ARGS(1) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_readwrite, w3)/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "END TYPE adj_test_type\n\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine adj_kern1()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern1\n"
        "  subroutine adj_kern2()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern2\n"
        "  subroutine adj_kern3()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern3\n\n"
        "end module adj_test_mod\n")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_lfric_adjoint(psyir, ["psyir_tmp", "psyir_tmp_1"])
    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str == expected

TL_CODE_WITH_GEOM = (
    "module testkern_mod\n"
    "  use kinds_mod, only: i_def, r_def\n"
    "  use kernel_mod, only: kernel_type, arg_type, gh_field, gh_real, "
    "gh_write, w3, cell_column\n"
    "  type, extends(kernel_type) :: testkern_type\n"
    "     type(arg_type), dimension(4) :: meta_args =          & \n"
    "          (/ arg_type(gh_scalar, gh_real, gh_read),       & \n"
    "             arg_type(gh_field*3,gh_real, gh_read, wchi), & \n"
    "             arg_type(gh_field,  gh_real, gh_write,  w3), & \n"
    "             arg_type(gh_field,  gh_integer, gh_read,     & \n"
    "                      any_discontinuous_space_1)  & \n"
    "           /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "     procedure, nopass :: code => testkern_code\n"
    "  end type testkern_type\n"
    "contains\n"
    "  subroutine testkern_code(nlayers, ascalar, cfield1, cfield2, cfield3, &\n"
    "field, pids, ndf_wchi, undf_wchi, map_wchi, ndf_w3, undf_w3, &\n"
    "map_w3, ndf_adspace1, undf_adspace1, map_adspace1)\n"
    "    integer(kind=i_def), intent(in) :: nlayers\n"
    "    integer(kind=i_def), intent(in) :: ndf_w3, undf_w3\n"
    "    integer(kind=i_def), intent(in) :: ndf_wchi, undf_wchi\n"
    "    integer(kind=i_def), intent(in) :: ndf_adspace1, undf_adspace1\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_wchi) :: map_wchi\n"
    "    integer(kind=i_def), intent(in), dimension(ndf_adspace1) :: map_adspace1\n"
    "    real(kind=r_def), intent(in) :: ascalar\n"
    "    real(kind=r_def), intent(inout), dimension(undf_wchi) :: cfield1, cfield2, cfield3\n"
    "    real(kind=r_def), intent(inout), dimension(undf_w3) :: field\n"
    "    integer(kind=i_def), intent(in), dimension(undf_adspace1) :: pids\n"
    "    field = ascalar\n"
    "  end subroutine testkern_code\n"
    "end module testkern_mod\n"
)

def test_adjoint_metadata(fortran_reader, fortran_writer):
    '''Check that the adjoint metadata is set correctly.'''
    tl_fortran_str = (
        "module tl_test_mod\n"
        "  use kernel_mod\n"
        "  type, public, extends(kernel_type) :: tl_test_type\n"
        "    type(arg_type) :: meta_args(3) =         &\n"
        "         (/ arg_type(gh_field, gh_real, gh_readwrite, w3), &\n"
        "            arg_type(gh_field, gh_real, gh_read, w3), &\n"
        "            arg_type(gh_field, gh_real, gh_read, w3) /)\n"
        "    integer :: operates_on = CELL_COLUMN\n"
        "  contains\n"
        "    procedure, nopass :: tl_test_code\n"
        "  end type\n\n"
        "  contains\n"
        "  subroutine tl_test_code(nlayers, field1, field2, field3, ndf_w3, undf_w3, map_w3)\n"
        "    integer, intent(in) :: nlayers, ndf_w3, undf_w3\n"
        "    real(kind=r_def), dimension(undf_w3), intent(out) :: field1\n"
        "    real(kind=r_def), dimension(undf_w3), intent(in) :: field2, field3\n"
        "    integer, dimension(ndf_w3), intent(in) :: map_w3\n"
        "    integer df, k\n"
        "    do df = 1, ndf_w3\n"
        "      do k = 0, nlayers-1\n"
        "        field1(map_w3(df) + k) = field2(map_w3(df) + k) * field3(map_w3(df) + k)\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine tl_test_code\n"
        "end module tl_test_mod\n")
    expected = (
        "xxx")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_lfric_adjoint(psyir, ["field1", "field2"])
    ad_fortran_str = fortran_writer(ad_psyir)
    print(ad_fortran_str)
    exit(1)
    assert ad_fortran_str == expected
    
