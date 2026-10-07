! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (conversion of real-valued to
  ! integer-valued field elements) specified in an invoke call.
  use integer_field_mod, only: integer_field_type
  use field_mod,         only: field_type

  implicit none

  type(integer_field_type) :: f2
  type(field_type)         :: f1

  call invoke( real_to_int_X(f2, f1) )

end program single_invoke
