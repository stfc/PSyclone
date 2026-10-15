! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM dependent_invoke_test

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system. This program
  ! tests psyclones ability to handle multiple dependent
  ! loops within the same invoke

  use kind_params_mod
  use grid_mod
  use field_mod
  use compute_cu_mod,  only: compute_cu
  implicit none

  type(grid_type), target :: model_grid
  !> We create two copies of each field type to use
  !> in a multi-call invoke to create a specific
  !> dependency pattern
  !> Two copies of a "pressure" field
  type(r2d_field) :: p_fld, p2_fld
  !> Two copies of an "x velocity" field
  type(r2d_field) :: u_fld, u2_fld
  !> Two copies of a "x mass flux" field
  type(r2d_field) :: cu_fld, cu_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  p_fld    = r2d_field(model_grid, GO_T_POINTS)

  u_fld    = r2d_field(model_grid, GO_U_POINTS)

  cu_fld    = r2d_field(model_grid, GO_U_POINTS)

  p2_fld  = r2d_field(model_grid, GO_T_POINTS)

  u2_fld = r2d_field(model_grid, GO_U_POINTS)

  cu2_fld = r2d_field(model_grid, GO_U_POINTS)


  !  ** Start of time loop ** 
  DO ncycle=1,100

    call invoke( compute_cu(cu_fld, p_fld, u_fld),    &
                 compute_cu(cu2_fld, p2_fld, u2_fld), &
                 compute_cu(cu2_fld, p2_fld, u2_fld), &
                 compute_cu(cu_fld, p_fld, u_fld) )

  END DO

  !===================================================

END PROGRAM dependent_invoke_test
