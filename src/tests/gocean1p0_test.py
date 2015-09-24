#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab

from parse import parse
from psyGen import PSyFactory
import os

'''Tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

def test_field():
    ''' Tests that a kernel call with only fields produces correct code '''
    ast, invokeInfo = parse(os.path.join(os.path.\
                                         dirname(os.path.\
                                                 abspath(__file__)),
                                         "test_files", "gocean1p0",
                                         "single_invoke.f90"),
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.join(os.path.\
                                         dirname(os.path.\
                                                 abspath(__file__)),
                                         "test_files", "gocean1p0",
                                         "single_invoke_two_kernels.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
    generated_code = psy.gen

    expected_output = """  MODULE psy_single_invoke_two_kernels
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, unew_fld, uold_fld)
      USE time_smooth_mod, ONLY: time_smooth_code
      USE compute_cu_mod, ONLY: compute_cu_code
      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld, unew_fld, uold_fld
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
          CALL time_smooth_code(i, j, u_fld%data, unew_fld%data, uold_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0
  END MODULE psy_single_invoke_two_kernels"""

    assert str(generated_code).find(expected_output) != -1


def test_grid_property():
    ''' Tests that an invoke containing a kernel call requiring
    a property of the grid produces correct code '''
    ast, invokeInfo = parse(os.path.join(os.path.\
                                         dirname(os.path.\
                                                 abspath(__file__)),
                                         "test_files", "gocean1p0",
                                         "single_invoke_grid_props.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
          CALL next_sshu_code(i, j, cu_fld%data, u_fld%data, u_fld%grid%tmask, u_fld%grid%area_t, u_fld%grid%area_u)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_next_sshu
  END MODULE psy_single_invoke_with_grid_props_test"""

    assert generated_code.find(expected_output) != -1


def test_scalar_int_arg():
    ''' Tests that an invoke containing a kernel call requiring
    an integer, scalar argument produces correct code '''
    ast, invokeInfo = parse(os.path.join(os.path.\
                                         dirname(os.path.\
                                                 abspath(__file__)),
                                         "test_files", "gocean1p0",
                                         "single_invoke_scalar_int_arg.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.join(os.path.\
                                         dirname(os.path.\
                                                 abspath(__file__)),
                                         "test_files", "gocean1p0",
                                         "single_invoke_scalar_float_arg.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test14_ne_offset_cf_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
          CALL compute_vort_code(i, j, vort_fld%data, p_fld%data, u_fld%data, v_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_vort
  END MODULE psy_single_invoke_test"""

    assert generated_code.find(expected_output) != -1


def test_ne_offset_ct_points():
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CT points '''
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test15_ne_offset_ct_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test16_ne_offset_cu_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test17_ne_offset_cv_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test18_ne_offset_cf_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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


def test_sw_offset_all_cf_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CF points '''
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test19_sw_offset_cf_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
          CALL compute_z_code(i, j, zfld%data, pfld%data, ufld%data, vfld%data, pfld%grid%dx, pfld%grid%dy)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_compute_z
  END MODULE psy_single_invoke_test"""

    assert generated_code.find(expected_output) != -1


def test_sw_offset_ct_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CT points '''
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test20_sw_offset_ct_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test21_sw_offset_all_ct_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
          CALL apply_bcs_h_code(i, j, hfld%data, pfld%data, ufld%data, vfld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0_apply_bcs_h
  END MODULE psy_single_invoke_test"""

    assert generated_code.find(expected_output) != -1


def test_sw_offset_all_cu_points():
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CU points '''
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test22_sw_offset_all_cu_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
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
    ast, invokeInfo = parse(os.path.\
                            join(os.path.\
                                 dirname(os.path.\
                                         abspath(__file__)),
                                 "test_files", "gocean1p0",
                                 "test23_sw_offset_all_cv_updated_one_invoke.f90"), 
                            api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(invokeInfo)
    generated_code = str(psy.gen)
    print generated_code

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
