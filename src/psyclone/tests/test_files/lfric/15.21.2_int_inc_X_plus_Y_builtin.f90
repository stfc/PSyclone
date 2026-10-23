! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single built-in operation (increment an integer-valued
  ! field) specified in an invoke call.
  use integer_field_mod, only: integer_field_type

  implicit none

  type(integer_field_type) :: f1, f2

  call invoke( int_inc_X_plus_Y(f1, f2) )

end program single_invoke
