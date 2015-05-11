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

class TestPSyGOcean1p0API:
    ''' Tests for PSy-layer code generation that are specific to the GOcean1.0 api. '''

    def test_field(self):
        ''' Tests that a kernel call with only fields produces correct code '''
        ast, invokeInfo = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)), "test_files", "gocean1p0", "single_invoke.f90"), api="gocean1.0")
        psy = PSyFactory("gocean1.0").create(invokeInfo)
        generated_code = psy.gen

        expected_output = """  MODULE psy_single_invoke_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_compute_cu(cu_fld, p_fld, u_fld)
      USE compute_cu_mod, ONLY: compute_cu_code
      TYPE(r2d_field), intent(inout) :: cu_fld, p_fld, u_fld
      INTEGER j
      INTEGER i
      DO j=cu_fld%internal%ystart,cu_fld%internal%ystop
        DO i=cu_fld%internal%xstart,cu_fld%internal%xstop
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_compute_cu
  END MODULE psy_single_invoke_test"""

        assert str(generated_code).find(expected_output) != -1

    def test_two_kernels(self):
        ''' Tests that an invoke containing two kernel calls with only 
            fields as arguments produces correct code '''
        ast, invokeInfo = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)), "test_files", "gocean1p0", "single_invoke_two_kernels.f90"), 
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
      INTEGER idim1
      INTEGER idim2
      INTEGER j
      INTEGER i
      DO j=cu_fld%internal%ystart,cu_fld%internal%ystop
        DO i=cu_fld%internal%xstart,cu_fld%internal%xstop
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO 
      idim2 = SIZE(uold_fld%data, 2)
      idim1 = SIZE(uold_fld%data, 1)
      DO j=1,idim2
        DO i=1,idim1
          CALL time_smooth_code(i, j, u_fld%data, unew_fld%data, uold_fld%data)
        END DO 
      END DO 
    END SUBROUTINE invoke_0
  END MODULE psy_single_invoke_two_kernels"""

        assert str(generated_code).find(expected_output) != -1

    def test_grid_property(self):
        ''' Tests that an invoke containing a kernel call requiring
            a property of the grid produces correct code '''
        ast, invokeInfo = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)), "test_files", "gocean1p0", "single_invoke_grid_props.f90"), 
                                api="gocean1.0")
        psy = PSyFactory("gocean1.0").create(invokeInfo)
        generated_code = psy.gen

        expected_output = """  MODULE psy_single_invoke_with_grid_props_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_next_sshu(cu_fld, u_fld)
      USE kernel_requires_grid_props, ONLY: next_sshu_code
      TYPE(r2d_field), intent(inout) :: cu_fld, u_fld
      INTEGER j
      INTEGER i
      DO j=cu_fld%internal%ystart,cu_fld%internal%ystop
        DO i=cu_fld%internal%xstart,cu_fld%internal%xstop
          CALL next_sshu_code(i, j, cu_fld%data, u_fld%data, u_fld%grid%tmask, u_fld%grid%area_t, u_fld%grid%area_u)
        END DO 
      END DO 
    END SUBROUTINE invoke_next_sshu
  END MODULE psy_single_invoke_with_grid_props_test"""

        assert str(generated_code).find(expected_output) != -1


    def test_scalar_int_arg(self):
        ''' Tests that an invoke containing a kernel call requiring
            an integer, scalar argument produces correct code '''
        ast, invokeInfo = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)), "test_files", "gocean1p0", "single_invoke_scalar_int_arg.f90"), 
                                api="gocean1.0")
        psy = PSyFactory("gocean1.0").create(invokeInfo)
        generated_code = psy.gen

        expected_output = """  MODULE psy_single_invoke_scalar_int_test
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_bc_ssh(ncycle, ssh_fld)
      USE kernel_scalar_int, ONLY: bc_ssh_code
      TYPE(r2d_field), intent(inout) :: ssh_fld
      INTEGER, intent(inout) :: ncycle
      INTEGER j
      INTEGER i
      DO j=ssh_fld%whole%ystart,ssh_fld%whole%ystop
        DO i=ssh_fld%whole%xstart,ssh_fld%whole%xstop
          CALL bc_ssh_code(i, j, ncycle, ssh_fld%data, ssh_fld%grid%tmask)
        END DO 
      END DO 
    END SUBROUTINE invoke_bc_ssh
  END MODULE psy_single_invoke_scalar_int_test"""
        assert str(generated_code).find(expected_output) != -1
