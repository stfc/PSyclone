! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation specified in an invoke call
  ! with scalar passed by value.
  use field_mod, only: field_type

  implicit none

  type(field_type) :: f1, f2, f3
  
  call invoke( aX_plus_bY(f3, 0.5d0, f1, 0.8, f2) )

end program single_invoke
