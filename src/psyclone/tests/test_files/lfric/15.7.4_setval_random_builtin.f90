! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (set field to pseudo-random values)
  ! specified in an invoke call.
  use field_mod, only: field_type

  implicit none

  type(field_type) :: f1

  call invoke( setval_random(f1) )

end program single_invoke
