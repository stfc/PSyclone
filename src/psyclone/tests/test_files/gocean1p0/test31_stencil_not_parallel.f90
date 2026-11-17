! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM kernel_stencil_test

  ! This Fortran program calls an (invalid) kernel that can't be
  ! parallelised because it writes to a variable with a stencil
  ! access. A valid PSyKAl kernel, by definition, must be safe to
  ! execute in parallel.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_stencil_not_parallel, only: stencil_not_parallel
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: u_fld, v_fld


  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  u_fld    = r2d_field(model_grid, GO_T_POINTS)
  v_fld    = r2d_field(model_grid, GO_T_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( stencil_not_parallel(u_fld, v_fld) )

  END DO

  !===================================================

END PROGRAM kernel_stencil_test
