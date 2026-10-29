! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


subroutine const_parameter()

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use field_mod, only : r2d_field
  use kernel_scalar_float, only: bc_ssh, bc_ssh_value
  implicit none

  real            :: real_val
  integer         :: int_val
  type(r2d_field) :: p_fld

  call invoke( bc_ssh(0, p_fld) )
  call invoke( bc_ssh_value(real_val, int_val, p_fld) )
  call invoke( bc_ssh_value(0, 0, p_fld) )

END subroutine CONST_PARAMETER

