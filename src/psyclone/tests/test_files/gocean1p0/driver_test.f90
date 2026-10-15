! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM extract_example_with_various_variable_access_patterns

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_driver_test_mod,  only: compute_kernel
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: out_fld, in_out_fld, in_fld, v_fld
  type(r2d_field) :: out_fld_data, out_fld_data_post

  ! This field will potentially create a name clash in the driver:
  ! The kernel takes the 'dx' grid property as parameter, so we
  ! have to test that the driver does not create a local variable 'dx'
  ! for the field AND for the grid property, one of them must be
  ! renamed!
  type(r2d_field) :: dx

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  in_out_fld    = r2d_field(model_grid, GO_T_POINTS)

  in_fld       = r2d_field(model_grid, GO_U_POINTS)
  dummy_in_fld = r2d_field(model_grid, GO_U_POINTS)
  v_fld        = r2d_field(model_grid, GO_V_POINTS)
  out_fld      = r2d_field(model_grid, GO_U_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( compute_kernel(out_fld, in_out_fld, in_fld, dx))

  END DO
  call invoke( compute_kernel(out_fld, out_fld_data, out_fld_data_post, dx))
  call invoke( compute_kernel(out_fld, out_fld_data, out_fld_data_post, dx))
  call invoke( compute_kernel(out_fld, in_out_fld, in_fld, dx),    &
               compute_kernel(out_fld, in_out_fld, in_fld, dx),    &
               compute_kernel(out_fld, in_out_fld, in_fld, dx)      )

  !===================================================

END PROGRAM extract_example_with_various_variable_access_patterns
