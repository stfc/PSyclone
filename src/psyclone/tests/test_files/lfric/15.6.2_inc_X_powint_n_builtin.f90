! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: point-wise operation (raise field to an integer power)
  ! specified in an invoke call. The power is supplied as a scalar variable,
  ! a literal and as an access to a member of a derived type.
  use constants_mod, only: i_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1
  integer(i_def)   :: i_scalar

  type :: my_type
     integer(i_def) :: a_scalar
  end type my_type
  type(my_type) :: my_var

  call invoke( inc_X_powint_n(f1, i_scalar), &
               inc_X_powint_n(f1, -2_i_def),       &
               inc_X_powint_n(f1, my_var%a_scalar) )

end program single_invoke
