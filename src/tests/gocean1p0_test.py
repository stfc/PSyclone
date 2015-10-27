# ----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab

'''Tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from parse import parse
from psyGen import PSyFactory
import pytest
import os
from generator import GenerationError, ParseError

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_compute_cu(cu_fld, p_fld, u_fld)
      USE compute_cu_mod, ONLY: compute_cu_code
      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = cu_fld%grid%simulation_domain%xstop
      jstop = cu_fld%grid%simulation_domain%ystop
      !
      DO j=2,jstop
        DO i=2,istop+1
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_cu
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_two_kernels
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, unew_fld, uold_fld)
      USE time_smooth_mod, ONLY: time_smooth_code
      USE compute_cu_mod, ONLY: compute_cu_code
      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld, \
unew_fld, uold_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = cu_fld%grid%simulation_domain%xstop
      jstop = cu_fld%grid%simulation_domain%ystop
      !
      DO j=2,jstop
        DO i=2,istop+1
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO 
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL time_smooth_code(i, j, u_fld%data, unew_fld%data, \
uold_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0
  END MODULE psy_single_invoke_two_kernels"""

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

    expected_output = """  MODULE psy_single_invoke_with_grid_props_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_next_sshu(cu_fld, u_fld)
      USE kernel_requires_grid_props, ONLY: next_sshu_code
      TYPE(r2d_field), intent(inout) :: cu_fld, u_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = cu_fld%grid%simulation_domain%xstop
      jstop = cu_fld%grid%simulation_domain%ystop
      !
      DO j=2,jstop
        DO i=2,istop-1
          CALL next_sshu_code(i, j, cu_fld%data, u_fld%data, \
u_fld%grid%tmask, u_fld%grid%area_t, u_fld%grid%area_u)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_next_sshu
  END MODULE psy_single_invoke_with_grid_props_test"""

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

    expected_output = """  MODULE psy_single_invoke_scalar_int_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_bc_ssh(ncycle, ssh_fld)
      USE kernel_scalar_int, ONLY: bc_ssh_code
      TYPE(r2d_field), intent(inout) :: ssh_fld
      INTEGER, intent(inout) :: ncycle
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = ssh_fld%grid%simulation_domain%xstop
      jstop = ssh_fld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL bc_ssh_code(i, j, ncycle, ssh_fld%data, ssh_fld%grid%tmask)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_bc_ssh
  END MODULE psy_single_invoke_scalar_int_test"""

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

    expected_output = """  MODULE psy_single_invoke_scalar_float_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_bc_ssh(a_scalar, ssh_fld)
      USE kernel_scalar_float, ONLY: bc_ssh_code
      TYPE(r2d_field), intent(inout) :: ssh_fld
      REAL(KIND=wp), intent(inout) :: a_scalar
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = ssh_fld%grid%simulation_domain%xstop
      jstop = ssh_fld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL bc_ssh_code(i, j, a_scalar, ssh_fld%data, ssh_fld%grid%tmask)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_bc_ssh
  END MODULE psy_single_invoke_scalar_float_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_compute_vort(vort_fld, p_fld, u_fld, v_fld)
      USE kernel_ne_offset_cf_mod, ONLY: compute_vort_code
      TYPE(r2d_field), intent(inout) :: vort_fld, p_fld, u_fld, v_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = vort_fld%grid%simulation_domain%xstop
      jstop = vort_fld%grid%simulation_domain%ystop
      !
      DO j=1,jstop-1
        DO i=1,istop-1
          CALL compute_vort_code(i, j, vort_fld%data, p_fld%data, \
u_fld%data, v_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_vort
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_compute_vort(p_fld, u_fld, v_fld)
      USE kernel_ne_offset_ct_mod, ONLY: compute_vort_code
      TYPE(r2d_field), intent(inout) :: p_fld, u_fld, v_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = p_fld%grid%simulation_domain%xstop
      jstop = p_fld%grid%simulation_domain%ystop
      !
      DO j=2,jstop
        DO i=2,istop
          CALL compute_vort_code(i, j, p_fld%data, u_fld%data, v_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_vort
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_bc_solid_u(u_fld)
      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_u_code
      TYPE(r2d_field), intent(inout) :: u_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = u_fld%grid%simulation_domain%xstop
      jstop = u_fld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop
          CALL bc_solid_u_code(i, j, u_fld%data, u_fld%grid%tmask)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_bc_solid_u
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_bc_solid_v(v_fld)
      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_v_code
      TYPE(r2d_field), intent(inout) :: v_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = v_fld%grid%simulation_domain%xstop
      jstop = v_fld%grid%simulation_domain%ystop
      !
      DO j=1,jstop
        DO i=1,istop+1
          CALL bc_solid_v_code(i, j, v_fld%data, v_fld%grid%tmask)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_bc_solid_v
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_bc_solid_f(f_fld)
      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_f_code
      TYPE(r2d_field), intent(inout) :: f_fld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = f_fld%grid%simulation_domain%xstop
      jstop = f_fld%grid%simulation_domain%ystop
      !
      DO j=1,jstop
        DO i=1,istop
          CALL bc_solid_f_code(i, j, f_fld%data, f_fld%grid%tmask)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_bc_solid_f
  END MODULE psy_single_invoke_test"""

    assert generated_code.find(expected_output) != -1


def test_sw_offset_cf_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.1_sw_offset_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_compute_z(zfld, pfld, ufld, vfld)
      USE kernel_sw_offset_cf_mod, ONLY: compute_z_code
      TYPE(r2d_field), intent(inout) :: zfld, pfld, ufld, vfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = zfld%grid%simulation_domain%xstop
      jstop = zfld%grid%simulation_domain%ystop
      !
      DO j=2,jstop+1
        DO i=2,istop+1
          CALL compute_z_code(i, j, zfld%data, pfld%data, \
ufld%data, vfld%data, pfld%grid%dx, pfld%grid%dy)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_z
  END MODULE psy_single_invoke_test"""
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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_apply_bcs_f(zfld, pfld, ufld, vfld)
      USE kernel_sw_offset_cf_mod, ONLY: apply_bcs_f_code
      TYPE(r2d_field), intent(inout) :: zfld, pfld, ufld, vfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = zfld%grid%simulation_domain%xstop
      jstop = zfld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL apply_bcs_f_code(i, j, zfld%data, pfld%data, \
ufld%data, vfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_apply_bcs_f
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_compute_h(hfld, pfld, ufld, vfld)
      USE kernel_sw_offset_ct_mod, ONLY: compute_h_code
      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = hfld%grid%simulation_domain%xstop
      jstop = hfld%grid%simulation_domain%ystop
      !
      DO j=2,jstop
        DO i=2,istop
          CALL compute_h_code(i, j, hfld%data, pfld%data, ufld%data, vfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_h
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_apply_bcs_h(hfld, pfld, ufld, vfld)
      USE kernel_sw_offset_ct_mod, ONLY: apply_bcs_h_code
      TYPE(r2d_field), intent(inout) :: hfld, pfld, ufld, vfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = hfld%grid%simulation_domain%xstop
      jstop = hfld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL apply_bcs_h_code(i, j, hfld%data, pfld%data, \
ufld%data, vfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_apply_bcs_h
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_apply_bcs_u(ufld, vfld)
      USE kernel_sw_offset_cu_mod, ONLY: apply_bcs_u_code
      TYPE(r2d_field), intent(inout) :: ufld, vfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = ufld%grid%simulation_domain%xstop
      jstop = ufld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL apply_bcs_u_code(i, j, ufld%data, vfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_apply_bcs_u
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_apply_bcs_v(vfld, ufld)
      USE kernel_sw_offset_cv_mod, ONLY: apply_bcs_v_code
      TYPE(r2d_field), intent(inout) :: vfld, ufld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = vfld%grid%simulation_domain%xstop
      jstop = vfld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL apply_bcs_v_code(i, j, vfld%data, ufld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_apply_bcs_v
  END MODULE psy_single_invoke_test"""

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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_compute_u(ufld, vfld, hfld)
      USE kernel_any_offset_cu_mod, ONLY: compute_u_code
      TYPE(r2d_field), intent(inout) :: ufld, vfld, hfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = ufld%grid%simulation_domain%xstop
      jstop = ufld%grid%simulation_domain%ystop
      !
      DO j=1,jstop
        DO i=1,istop
          CALL compute_u_code(i, j, ufld%data, vfld%data, hfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_u
  END MODULE psy_single_invoke_test"""
    print generated_code
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

    expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0_copy(voldfld, vfld)
      USE kernel_field_copy_mod, ONLY: field_copy_code
      TYPE(r2d_field), intent(inout) :: voldfld, vfld
      INTEGER j
      INTEGER i
      INTEGER istop, jstop
      !
      ! Look-up loop bounds
      istop = voldfld%grid%simulation_domain%xstop
      jstop = voldfld%grid%simulation_domain%ystop
      !
      DO j=1,jstop+1
        DO i=1,istop+1
          CALL field_copy_code(i, j, voldfld%data, vfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_copy
  END MODULE psy_single_invoke_test"""
    print generated_code
    assert generated_code.find(expected_output) != -1


def test_goschedule_view(capsys):
    ''' Test that the GOSchedule::view() method works as expected '''
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

    expected_output = """GOSchedule[invoke='invoke_0',Constant loop bounds=True]
    Loop[type='outer',field_space='cu',it_space='internal_pts']
        Loop[type='inner',field_space='cu',it_space='internal_pts']
            Call compute_cu_code(cu_fld,p_fld,u_fld)
    Loop[type='outer',field_space='every',it_space='internal_pts']
        Loop[type='inner',field_space='every',it_space='internal_pts']
            Call time_smooth_code(u_fld,unew_fld,uold_fld)"""

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
    expected_sched = '''GOSchedule(Constant loop bounds=True):
Loop[]: j= lower=2,jstop,1
Loop[]: i= lower=2,istop+1,1
kern call: compute_cu_code
EndLoop
EndLoop
Loop[]: j= lower=1,jstop+1,1
Loop[]: i= lower=1,istop+1,1
kern call: time_smooth_code
EndLoop
EndLoop
End Schedule
'''
    sched_str = str(schedule)
    print sched_str
    assert sched_str in expected_sched

    # Switch-off constant loop bounds
    schedule.const_loop_bounds = False
    sched_str = str(schedule)

    expected_sched = '''GOSchedule(Constant loop bounds=False):
Loop[]: j= lower=cu_fld%internal%ystart,cu_fld%internal%ystop,1
Loop[]: i= lower=cu_fld%internal%xstart,cu_fld%internal%xstop,1
kern call: compute_cu_code
EndLoop
EndLoop
Loop[]: j= lower=1,SIZE(uold_fld%data, 2),1
Loop[]: i= lower=1,SIZE(uold_fld%data, 1),1
kern call: time_smooth_code
EndLoop
EndLoop
End Schedule
'''
    print sched_str
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
    from gocean1p0 import GOLoop
    goloop = GOLoop(loop_type="inner")
    # Try and generate the code for this loop even though it
    # has no parent schedule and no children
    with pytest.raises(GenerationError):
        goloop.gen_code(None)


def test_goloop_no_children():
    ''' Attempt to generate code for a loop that has no child
    kernel calls '''
    from gocean1p0 import GOLoop, GOSchedule
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
    from gocean1p0 import GOLoop, GOSchedule, GOKern
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
    from gocean1p0 import GOLoop, GOSchedule, GOKern
    gosched = GOSchedule([])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    goiloop = GOLoop(parent=gosched, loop_type="inner")
    gosched.addchild(gojloop)
    gojloop.addchild(goiloop)
    gokern1 = GOKern()
    gokern2 = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern1._index_offset = "offset_ne"
    gokern2._index_offset = "offset_sw"
    goiloop.addchild(gokern1)
    goiloop.addchild(gokern2)
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)

# Parser Tests for the GOcean 1.0 API

def test00p1_kernel_wrong_meta_arg_count():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data has the wrong number of arguments '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.1_invoke_kernel_wrong_meta_arg_count.f90"),
              api="gocean1.0")

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

def test09_kernel_missing_stencil_property():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test09_invoke_kernel_missing_stencil.f90"),
              api="gocean1.0")

def test10_kernel_invalid_stencil_property():
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

