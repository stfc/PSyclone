! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (scale an integer-valued field
  ! by an integer scalar: X = aX) specified in an invoke call.
  use constants_mod,     only: i_def
  use integer_field_mod, only: integer_field_type

  implicit none

  type(integer_field_type) :: f1
  integer(i_def)           :: a_scalar

  call invoke( int_inc_a_times_X(a_scalar, f1) )

end program single_invoke
