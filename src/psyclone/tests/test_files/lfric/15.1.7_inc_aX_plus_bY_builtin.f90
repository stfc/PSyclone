! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (X = aX + bY)
  ! specified in an invoke call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1, f2
  real(r_def)      :: a, b

  a = 0.5
  b = 0.8
  
  call invoke( inc_aX_plus_bY(a, f1, b, f2) )

end program single_invoke
