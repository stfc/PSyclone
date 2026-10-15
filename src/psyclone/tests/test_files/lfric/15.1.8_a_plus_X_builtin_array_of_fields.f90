! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (Y = a + X, DoF-wise addition
  ! of a real scalar value) specified in an invoke call, with the field
  ! being a member of an array of fields.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1
  type(field_type) :: f2(10)
  real(r_def)      :: a
  integer :: i

  a = 0.5

  do i=1, 10
    call invoke( a_plus_X(f2(i), a, f1) )
  enddo

end program single_invoke
