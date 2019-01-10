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
# ----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab
# Modified work Copyright (c) 2018 by J. Henrichs, Bureau of Meteorology

'''Tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from __future__ import absolute_import, print_function
import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, InternalError
from psyclone.generator import GenerationError, ParseError
from psyclone.gocean1p0 import GOKern, GOLoop, GOSchedule
from psyclone_test_utils import get_invoke

API = "gocean1.0"


def test_field():
    ''' Tests that a kernel call with only fields produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_cu(cu_fld, p_fld, u_fld)\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = cu_fld%grid%subdomain%internal%xstop\n"
        "      jstop = cu_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_cu\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_two_kernels():
    ''' Tests that an invoke containing two kernel calls with only
    fields as arguments produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = psy.gen

    expected_output = (
        "  MODULE psy_single_invoke_two_kernels\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, unew_fld, uold_fld)\n"
        "      USE time_smooth_mod, ONLY: time_smooth_code\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld, "
        "unew_fld, uold_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = cu_fld%grid%subdomain%internal%xstop\n"
        "      jstop = cu_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL time_smooth_code(i, j, u_fld%data, unew_fld%data, "
        "uold_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0\n"
        "  END MODULE psy_single_invoke_two_kernels")

    assert str(generated_code).find(expected_output) != -1


def test_grid_property():
    ''' Tests that an invoke containing a kernel call requiring
    a property of the grid produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_grid_props.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_with_grid_props_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, u_fld, du_fld, d_fld)\n"
        "      USE kernel_requires_grid_props, ONLY: next_sshu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld, u_fld, du_fld, "
        "d_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = cu_fld%grid%subdomain%internal%xstop\n"
        "      jstop = cu_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop-1\n"
        "          CALL next_sshu_code(i, j, cu_fld%data, u_fld%data, "
        "u_fld%grid%tmask, u_fld%grid%area_t, u_fld%grid%area_u)\n"
        "        END DO \n"
        "      END DO \n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop-1\n"
        "          CALL next_sshu_code(i, j, du_fld%data, d_fld%data, "
        "d_fld%grid%tmask, d_fld%grid%area_t, d_fld%grid%area_u)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0\n"
        "  END MODULE psy_single_invoke_with_grid_props_test")

    assert generated_code.find(expected_output) != -1


def test_scalar_int_arg():
    ''' Tests that an invoke containing a kernel call requiring
    an integer, scalar argument produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_int_arg.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_scalar_int_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(ncycle, ssh_fld)\n"
        "      USE kernel_scalar_int, ONLY: bc_ssh_code\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      INTEGER, intent(inout) :: ncycle\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ssh_fld%grid%subdomain%internal%xstop\n"
        "      jstop = ssh_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_ssh_code(i, j, ncycle, ssh_fld%data, "
        "ssh_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_int_test")

    assert generated_code.find(expected_output) != -1


def test_scalar_float_arg():
    ''' Tests that an invoke containing a kernel call requiring
    a real, scalar argument produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_scalar_float_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(a_scalar, ssh_fld)\n"
        "      USE kernel_scalar_float, ONLY: bc_ssh_code\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      REAL(KIND=go_wp), intent(inout) :: a_scalar\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ssh_fld%grid%subdomain%internal%xstop\n"
        "      jstop = ssh_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_ssh_code(i, j, a_scalar, ssh_fld%data, "
        "ssh_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_float_test")

    assert generated_code.find(expected_output) != -1


def test_ne_offset_cf_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test14_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(vort_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_cf_mod, ONLY: compute_vort_code\n"
        "      TYPE(r2d_field), intent(inout) :: vort_fld, p_fld, u_fld, "
        "v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vort_fld%grid%subdomain%internal%xstop\n"
        "      jstop = vort_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop-1\n"
        "        DO i=1,istop-1\n"
        "          CALL compute_vort_code(i, j, vort_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_ne_offset_ct_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test15_ne_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_ct_mod, ONLY: compute_vort_code\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld, u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = p_fld%grid%subdomain%internal%xstop\n"
        "      jstop = p_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL compute_vort_code(i, j, p_fld%data, u_fld%data, "
        "v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_ne_offset_all_cu_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CU points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test16_ne_offset_cu_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_u(u_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = u_fld%grid%subdomain%internal%xstop\n"
        "      jstop = u_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop\n"
        "          CALL bc_solid_u_code(i, j, u_fld%data, u_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_u\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_ne_offset_all_cv_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CV points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test17_ne_offset_cv_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_v(v_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_v_code\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = v_fld%grid%subdomain%internal%xstop\n"
        "      jstop = v_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop+1\n"
        "          CALL bc_solid_v_code(i, j, v_fld%data, v_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_v\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_ne_offset_all_cf_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test18_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_f(f_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_f_code\n"
        "      TYPE(r2d_field), intent(inout) :: f_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = f_fld%grid%subdomain%internal%xstop\n"
        "      jstop = f_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop\n"
        "          CALL bc_solid_f_code(i, j, f_fld%data, f_fld%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_bc_solid_f\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_sw_offset_cf_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CF points '''
    _, invoke_info = parse(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "gocean1p0",
                     "test19.1_sw_offset_cf_updated_one_invoke.f90"),
        api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_z(z_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: compute_z_code\n"
        "      TYPE(r2d_field), intent(inout) :: z_fld, p_fld, u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = z_fld%grid%subdomain%internal%xstop\n"
        "      jstop = z_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=2,jstop+1\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_z_code(i, j, z_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data, p_fld%grid%dx, p_fld%grid%dy)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_z\n"
        "  END MODULE psy_single_invoke_test")
    assert generated_code.find(expected_output) != -1


def test_sw_offset_all_cf_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.2_sw_offset_all_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_f(z_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: apply_bcs_f_code\n"
        "      TYPE(r2d_field), intent(inout) :: z_fld, p_fld, u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = z_fld%grid%subdomain%internal%xstop\n"
        "      jstop = z_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_f_code(i, j, z_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_f\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_sw_offset_ct_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test20_sw_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_h(h_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: compute_h_code\n"
        "      TYPE(r2d_field), intent(inout) :: h_fld, p_fld, u_fld, v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = h_fld%grid%subdomain%internal%xstop\n"
        "      jstop = h_fld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL compute_h_code(i, j, h_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_h\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_sw_offset_all_ct_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test21_sw_offset_all_ct_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_h(hfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: apply_bcs_h_code\n"
        "      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = hfld%grid%subdomain%internal%xstop\n"
        "      jstop = hfld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_h_code(i, j, hfld%data, pfld%data, "
        "ufld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_h\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_sw_offset_all_cu_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CU points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test22_sw_offset_all_cu_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_u(ufld, vfld)\n"
        "      USE kernel_sw_offset_cu_mod, ONLY: apply_bcs_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: ufld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%subdomain%internal%xstop\n"
        "      jstop = ufld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_u_code(i, j, ufld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_u\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_sw_offset_all_cv_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CV points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test23_sw_offset_all_cv_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_v(vfld, ufld)\n"
        "      USE kernel_sw_offset_cv_mod, ONLY: apply_bcs_v_code\n"
        "      TYPE(r2d_field), intent(inout) :: vfld, ufld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = vfld%grid%subdomain%internal%xstop\n"
        "      jstop = vfld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL apply_bcs_v_code(i, j, vfld%data, ufld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_apply_bcs_v\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code.find(expected_output) != -1


def test_offset_any_all_cu_points():
    ''' Test that we can generate code for a kernel that will operate
    with any offset and writes to a field on all cu points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test25_any_offset_all_cu_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_u(ufld, vfld, hfld)\n"
        "      USE kernel_any_offset_cu_mod, ONLY: compute_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: ufld, vfld, hfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%subdomain%internal%xstop\n"
        "      jstop = ufld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop\n"
        "        DO i=1,istop\n"
        "          CALL compute_u_code(i, j, ufld%data, vfld%data, "
        "hfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_compute_u\n"
        "  END MODULE psy_single_invoke_test")
    print(generated_code)
    assert generated_code.find(expected_output) != -1


def test_offset_any_all_points():
    ''' Test that we can generate code for a kernel that will operate
    with any offset and writes to a field on all points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test24_any_offset_all_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_copy(voldfld, vfld)\n"
        "      USE kernel_field_copy_mod, ONLY: field_copy_code\n"
        "      TYPE(r2d_field), intent(inout) :: voldfld, vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop, jstop\n"
        "      !\n"
        "      ! Look-up loop bounds\n"
        "      istop = voldfld%grid%subdomain%internal%xstop\n"
        "      jstop = voldfld%grid%subdomain%internal%ystop\n"
        "      !\n"
        "      DO j=1,jstop+1\n"
        "        DO i=1,istop+1\n"
        "          CALL field_copy_code(i, j, voldfld%data, vfld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "    END SUBROUTINE invoke_0_copy\n"
        "  END MODULE psy_single_invoke_test")
    print(generated_code)
    assert generated_code.find(expected_output) != -1


def test_goschedule_view(capsys):
    ''' Test that the GOSchedule::view() method works as expected '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    invoke.schedule.view()

    # The view method writes to stdout and this is captured by py.test
    # by default. We have to query this captured output.
    out, _ = capsys.readouterr()

    # Ensure we check for the correct (colour) control codes in the output
    sched = colored("GOSchedule", SCHEDULE_COLOUR_MAP["Schedule"])
    loop = colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])
    call = colored("KernCall", SCHEDULE_COLOUR_MAP["KernCall"])

    expected_output = (
        sched + "[invoke='invoke_0',Constant loop bounds=True]\n"
        "    " + loop + "[type='outer',field_space='go_cu',"
        "it_space='go_internal_pts']\n"
        "        " + loop + "[type='inner',field_space='go_cu',"
        "it_space='go_internal_pts']\n"
        "            " + call + " compute_cu_code(cu_fld,p_fld,u_fld) "
        "[module_inline=False]\n"
        "    " + loop + "[type='outer',field_space='go_every',"
        "it_space='go_internal_pts']\n"
        "        " + loop + "[type='inner',field_space='go_every',"
        "it_space='go_internal_pts']\n"
        "            " + call + " time_smooth_code(u_fld,unew_fld,uold_fld) "
        "[module_inline=False]")

    assert expected_output in out


def test_goschedule_str():
    ''' Test that the GOSchedule::__str__ method works as expected '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    expected_sched = (
        "GOSchedule(Constant loop bounds=True):\n"
        "Loop[]: j= lower=2,jstop,1\n"
        "Loop[]: i= lower=2,istop+1,1\n"
        "kern call: compute_cu_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "Loop[]: j= lower=1,jstop+1,1\n"
        "Loop[]: i= lower=1,istop+1,1\n"
        "kern call: time_smooth_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "End Schedule\n")
    sched_str = str(schedule)
    assert sched_str in expected_sched

    # Switch-off constant loop bounds
    schedule.const_loop_bounds = False
    sched_str = str(schedule)

    expected_sched = (
        "GOSchedule(Constant loop bounds=False):\n"
        "Loop[]: j= lower=cu_fld%internal%ystart,cu_fld%internal%ystop,1\n"
        "Loop[]: i= lower=cu_fld%internal%xstart,cu_fld%internal%xstop,1\n"
        "kern call: compute_cu_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "Loop[]: j= lower=1,SIZE(uold_fld%data, 2),1\n"
        "Loop[]: i= lower=1,SIZE(uold_fld%data, 1),1\n"
        "kern call: time_smooth_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "End Schedule\n")
    assert sched_str in expected_sched


def test_gosched_ijstop():
    ''' Test that the GOSchedule.{i,j}loop_stop rais an error if
    constant loop bounds are not being used '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn off constant loop bounds
    schedule.const_loop_bounds = False
    # Attempt to query the upper bound of the i loop
    with pytest.raises(GenerationError):
        _ = schedule.iloop_stop
    # Attempt to query the upper bound of the j loop
    with pytest.raises(GenerationError):
        _ = schedule.jloop_stop


def test_goloop_no_parent():
    ''' Attempt to generate code for a loop that has no GOSchedule
    as a parent '''
    goloop = GOLoop(loop_type="inner")
    # Try and generate the code for this loop even though it
    # has no parent schedule and no children
    with pytest.raises(GenerationError):
        goloop.gen_code(None)


def test_goloop_no_children():
    ''' Attempt to generate code for a loop that has no child
    kernel calls '''
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    # Try and generate the code for this loop even though it
    # has no children
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)


def test_goloop_unsupp_offset():
    ''' Attempt to generate code for a loop with constant bounds with
    an unsupported index offset '''
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    gokern = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern._index_offset = "offset_se"
    goiloop.addchild(gokern)
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)


def test_goloop_unmatched_offsets():
    ''' Attempt to generate code for a loop with constant bounds with
    two different index offsets '''
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    gokern1 = GOKern()
    gokern2 = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern1._index_offset = "go_offset_ne"
    gokern2._index_offset = "go_offset_sw"
    goiloop.addchild(gokern1)
    goiloop.addchild(gokern2)
    with pytest.raises(GenerationError) as excinfo:
        goiloop.gen_code(None)
    # Note that the kernels do not have a name, so there is a double space
    assert "All Kernels must expect the same grid offset but kernel  " \
        "has offset go_offset_sw which does not match go_offset_ne" \
        in str(excinfo.value)


def test_writetoread_dag(tmpdir, have_graphviz):
    ''' Test that the GOSchedule::dag() method works as expected when we
    have two kernels with a write -> read dependency '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_write_to_read.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    _ = tmpdir.chdir()
    invoke.schedule.dag()
    if have_graphviz:
        dot_file = os.path.join(str(tmpdir), "dag")
        assert os.path.isfile(dot_file)
        with open(dot_file, "r") as dfile:
            dot = dfile.read()
        assert dot.startswith("digraph")
        # write -> read means that the second loop can only begin once the
        # first loop is complete. Check that we have the correct forwards
        # dependence (green) and backwards dependence (red).
        assert ('"loop_[outer]_1_end" -> "loop_[outer]_4_start" [color=red]'
                in dot or
                '"loop_[outer]_1_end" -> "loop_[outer]_4_start" '
                '[color=#ff0000]' in dot)
        assert ('"loop_[outer]_1_end" -> "loop_[outer]_4_start" [color=green]'
                in dot or
                '"loop_[outer]_1_end" -> "loop_[outer]_4_start" '
                '[color=#00ff00]' in dot)


def test_dag(tmpdir, have_graphviz):
    ''' Test that the GOSchedule::dag() method works as expected '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "nemolite2d_alg_mod.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    _ = tmpdir.chdir()
    invoke.schedule.dag()
    if have_graphviz:
        assert os.path.isfile(os.path.join(str(tmpdir), "dag.svg"))
        dot_file = os.path.join(str(tmpdir), "dag")
        assert os.path.isfile(dot_file)
        with open(dot_file, "r") as dfile:
            dot = dfile.read()
        # The two kernels in this example are independent so we should
        # have no forwards/backwards dependencies
        for col in ["red", "#ff0000", "green", "#00ff00"]:
            assert '[color={0}]'.format(col) not in dot


def test_find_grid_access(monkeypatch):
    ''' Tests for the GOKernelArguments.find_grid_access method. This
    identifies the best kernel argument from which to access grid
    properties. '''
    from psyclone.gocean1p0 import GOKernelArgument
    _, invoke = get_invoke("single_invoke.f90", API, idx=0)
    schedule = invoke.schedule
    kern = schedule.children[0].children[0].children[0]
    assert isinstance(kern, GOKern)
    arg = kern.arguments.find_grid_access()
    assert isinstance(arg, GOKernelArgument)
    # The first read-only argument for this kernel is the pressure field
    assert arg.name == "p_fld"
    # Now monkeypatch the type of each of the kernel arguments so that
    # none of them is a field
    for karg in kern.arguments._args:
        monkeypatch.setattr(karg._arg, "_type", "broken")
    # find_grid_access should now return None
    arg = kern.arguments.find_grid_access()
    assert arg is None


def test_raw_arg_list_error(monkeypatch):
    ''' Test that we raise an internal error in the
    GOKernelArguments.raw_arg_list method if there's no argument from which
    to get the grid properties. '''
    _, invoke = get_invoke("test19.1_sw_offset_cf_updated_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    schedule.view()
    kern = schedule.children[0].children[0].children[0]
    assert isinstance(kern, GOKern)
    raw_list = kern.arguments.raw_arg_list()
    assert raw_list == ['i', 'j', 'z_fld%data', 'p_fld%data', 'u_fld%data',
                        'v_fld%data', 'p_fld%grid%dx', 'p_fld%grid%dy']
    # Now monkeypatch find_grid_access()
    monkeypatch.setattr(kern.arguments, "find_grid_access", lambda: None)
    kern.arguments._raw_arg_list = None
    with pytest.raises(GenerationError) as err:
        _ = kern.arguments.raw_arg_list()
    assert ("kernel compute_z_code requires grid property dx but does not "
            "have any arguments that are fields" in str(err))
    # Now monkeypatch one of the kernel arguments so that it has an
    # unrecognised type
    monkeypatch.setattr(kern.arguments._args[0]._arg, "_type", "broken")
    with pytest.raises(InternalError) as err:
        _ = kern.arguments.raw_arg_list()
    assert ("Kernel compute_z_code, argument z_fld has unrecognised type: "
            "'broken'" in str(err))


# -----------------------------------
# Parser Tests for the GOcean 1.0 API
# -----------------------------------


def test00p1_kernel_wrong_meta_arg_count():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data has the wrong number of arguments '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.1_invoke_kernel_wrong_meta_arg_count.f90"),
              api="gocean1.0")


def test00p1_invoke_kernel_using_const_scalar():  # pylint:disable=invalid-name
    '''Check that using a const scalar as parameter works.'''
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test00.1_invoke_kernel_using_const_scalar.f90")
    _, invoke_info = parse(filename, api="gocean1.0")
    out = str(PSyFactory(API).create(invoke_info).gen)
    # Old versions of PSyclone tried to declare '0' as a variable:
    # REAL(KIND=wp), intent(inout) :: 0
    # INTEGER, intent(inout) :: 0
    # Make sure this is not happening anymor
    import re
    assert re.search(r"\s*real.*:: *0", out, re.I) is None
    assert re.search(r"\s*integer.*:: *0", out, re.I) is None
    assert re.search(r"\s*real.*:: *real_val", out, re.I) is not None


def test00p2_kernel_invalid_meta_args():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data is not 'arg' '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.2_invoke_kernel_invalid_meta_args.f90"),
              api="gocean1.0")


def test00p3_kern_invalid_meta_arg_type():
    ''' Check that the parser catches the case where the type of
    one of the meta-args in the kernel meta-data is incorrect '''
    test_file = "test00.3_invoke_kernel_invalid_meta_arg_type.f90"
    with pytest.raises(ParseError):
        _, _ = parse(os.path.
                     join(os.path.
                          dirname(os.path.
                                  abspath(__file__)),
                          "test_files", "gocean1p0",
                          test_file),
                     api=API)


def test01_diff_kern_grid_offsets_one_invoke():
    ''' Check that the parser raises an error if two kernels in a
        single invoke specify different index offsets '''
    test_file = "test01_different_grid_offsets_one_invoke.f90"
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                test_file),
                           api=API)
    with pytest.raises(GenerationError):
        _ = PSyFactory(API).create(invoke_info)


def test02_diff_kern_grid_offsets_two_invokes():
    ''' Check that the parser raises an error if the two kernels
        in different invokes specify different index offsets. '''
    test_file = "test02_different_grid_offsets_two_invokes.f90"
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                test_file),
                           api=API)
    with pytest.raises(GenerationError):
        _ = PSyFactory(API).create(invoke_info)


def test03_kernel_missing_index_offset():
    ''' Check that we raise an error if a kernel's meta-data is
    missing the INDEX_OFFSET field. '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test03_invoke_kernel_missing_offset.f90"),
              api="gocean1.0")


def test04_kernel_invalid_index_offset():
    ''' Check that we raise an error if a kernel's meta-data is
    contains an invalid value for the INDEX_OFFSET field. '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test04_invoke_kernel_invalid_offset.f90"),
              api="gocean1.0")


def test05_kernel_missing_iterates_over():
    ''' Check that we raise an error if a kernel's meta-data is
    missing the ITERATES_OVER field. '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test05_invoke_kernel_missing_iterates_over.f90"),
              api="gocean1.0")


def test05p1_kernel_invalid_iterates_over():
    ''' Check that we raise an error if a kernel's meta-data has
    an invalid ITERATES_OVER field. '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test05.1_invoke_kernel_invalid_iterates_over.f90"),
              api="gocean1.0")


def test05p1_kernel_add_iteration_spaces():
    '''Check that adding a new iteration space works
    '''

    # Add new iteration space 'dofs'
    GOLoop.add_bounds("go_offset_sw:go_cu:dofs:1:2:3:{stop}")

    _, invoke_info = \
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test05.1_invoke_kernel_invalid_iterates_over.f90"),
              api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    expected_sched = (
        "GOSchedule(Constant loop bounds=True):\n"
        "Loop[]: j= lower=1,2,1\n"
        "Loop[]: i= lower=3,istop,1\n"
        "kern call: compute_cu_code\n"
        "EndLoop\n"
        "EndLoop\n"
        "End Schedule\n")
    sched_str = str(schedule)
    assert sched_str in expected_sched


def test06_kernel_invalid_access():
    ''' Check that we raise an error if a kernel's meta-data specifies
    an unrecognised access type for a kernel argument (i.e. something
    other than READ,WRITE,READWRITE) '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test06_invoke_kernel_wrong_access.f90"),
              api="gocean1.0")


def test07_kernel_wrong_gridpt_type():
    ''' Check that we raise an error if a kernel's meta-data specifies
    an unrecognised grid-point type for a field argument (i.e.
    something other than C{U,V,F,T}, I_SCALAR or R_SCALAR) '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test07_invoke_kernel_wrong_gridpt_type.f90"),
              api="gocean1.0")


def test08_kernel_invalid_grid_property():
    ''' Check that the parser raises an error if a kernel's meta-data
    specifies an unrecognised grid property '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test08_invoke_kernel_invalid_grid_property.f90"),
              api="gocean1.0")


def test08p1_kernel_without_fld_args():
    ''' Check that the parser raises an error if a kernel does not
    have a field object as an argument but requests a grid property '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test08.1_invoke_kernel_no_fld_args.f90"),
              api="gocean1.0")


def test09_kernel_missing_stencil_prop():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test09_invoke_kernel_missing_stencil.f90"),
              api="gocean1.0")


def test10_kernel_invalid_stencil_prop():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test10_invoke_kernel_invalid_stencil.f90"),
              api="gocean1.0")


def test13_kernel_invalid_fortran():
    ''' Check that the parser raises an error if the specified kernel
    code is not valid fortran '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test13_invoke_kernel_invalid_fortran.f90"),
              api="gocean1.0")


def test14_no_builtins():
    ''' Check that we raise an error if we attempt to create a
    built-in '''
    from psyclone.gocean1p0 import GOBuiltInCallFactory
    with pytest.raises(GenerationError) as excinfo:
        GOBuiltInCallFactory.create()
    assert "Built-ins are not supported for the GOcean" in str(excinfo.value)
