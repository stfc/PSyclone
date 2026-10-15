! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM single_invoke_test

  ! Fake Fortran program for testing PSyclone when algorithm file and
  ! kernel file contain non-ascii characters. This is non-standard Fortran
  ! but most compilers are happy with it.
  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_utf_char_mod, only: kernel_utf_char
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: ufld, vfld, hfld

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  ufld = r2d_field(model_grid, GO_U_POINTS)
  vfld = r2d_field(model_grid, GO_V_POINTS)
  hfld = r2d_field(model_grid, GO_T_POINTS)

  ! Write statement containing a non-ascii char
  write(*,*) 'max reachable coeff. (at the Equator) for e1=1°)'

  call invoke( kernel_utf_char(ufld, vfld, hfld) )

END PROGRAM single_invoke_test
