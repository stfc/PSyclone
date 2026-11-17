! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (set field values to another field)
  ! specified in an invoke call.
  use field_mod, only: field_type

  implicit none

  type(field_type) :: f1, f2

  call invoke( setval_X(f2, f1) )

end program single_invoke
