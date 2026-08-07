! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_test

  ! Fake Fortran program for testing the use of PSyclone with a kernel
  ! that accesses a variable via a use statement.
  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_with_use_mod, only: kernel_with_use
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: oldu_fld, u_fld, cu_fld

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  oldu_fld = r2d_field(model_grid, GO_T_POINTS)
  u_fld = r2d_field(model_grid, GO_U_POINTS)
  cu_fld = r2d_field(model_grid, GO_U_POINTS)

  call invoke( kernel_with_use(oldu_fld, cu_fld, u_fld) )

end program single_invoke_test
