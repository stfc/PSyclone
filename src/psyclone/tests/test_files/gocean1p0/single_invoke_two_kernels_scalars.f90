! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_two_kernels_scalars

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_scalar_float, only: bc_ssh_value, bc_ssh
  implicit none

  !> Loop counter for time-stepping loop
  integer :: ncycle
  real :: a_scalar = 1.0

  type(r2d_field) :: ssh_fld


  !  ** Start of time loop ** 
  do ncycle=1,100
    
    call invoke( bc_ssh(a_scalar, ssh_fld), &
                 bc_ssh_value(a_scalar, ncycle, ssh_fld))

  end do

  !===================================================

end PROGRAM single_invoke_two_kernels_scalars
