! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (set an integer-valued field
  ! values to an integer scalar) specified in an invoke call.
  use constants_mod,     only: i_def
  use integer_field_mod, only: integer_field_type

  implicit none

  type(integer_field_type) :: f1
  integer(i_def)           :: c

  c = 1.0_i_def
  
  call invoke( int_setval_c(f1, c) )

end program single_invoke
