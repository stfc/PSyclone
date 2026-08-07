! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

PROGRAM single_invoke_with_grid_props_test

  ! Fake Fortran program for testing aspects of the PSyclone code
  ! generation system. Single invoke of two kernels which both require
  ! grid properties.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_requires_grid_props, only: next_sshu
  implicit none

  type(r2d_field) :: u_fld, d_fld
  type(r2d_field) :: cu_fld, du_fld

  call invoke( next_sshu(cu_fld, u_fld), &
               next_sshu(du_fld, d_fld) )

  !===================================================

END PROGRAM single_invoke_with_grid_props_test
