! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (X = X - a, DoF-wise subtraction
  ! of an integer scalar value from an integer-valued field) specified in
  ! an invoke call.
  use constants_mod,     only: i_def
  use integer_field_mod, only: integer_field_type

  implicit none

  type(integer_field_type) :: f1
  integer(i_def)           :: a

  a = 3_i_def

  call invoke( int_inc_X_minus_a(f1, a) )

end program single_invoke
