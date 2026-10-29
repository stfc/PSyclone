! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM single_invoke_two_kernels

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use compute_cu_mod,  only: compute_cu
  implicit none

  type(grid_type), target :: model_grid
  !> Pressure at current time step
  type(r2d_field) :: p_fld
  !> Velocity in x direction at {current,next,previous} time step
  type(r2d_field) :: u_fld, unew_fld, uold_fld
  !> Mass flux in x direction at current time step
  type(r2d_field) :: cu_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  p_fld    = r2d_field(model_grid, GO_T_POINTS)

  u_fld    = r2d_field(model_grid, GO_U_POINTS)
  unew_fld = r2d_field(model_grid, GO_U_POINTS)
  uold_fld = r2d_field(model_grid, GO_U_POINTS)

  cu_fld    = r2d_field(model_grid, GO_U_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( compute_cu(cu_fld, p_fld, u_fld),      &
                 compute_cu(p_fld, cu_fld, u_fld)   )

  END DO

  !===================================================

END PROGRAM single_invoke_two_kernels
