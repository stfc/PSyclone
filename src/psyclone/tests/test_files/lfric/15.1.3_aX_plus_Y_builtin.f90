! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (Z = aX + Y) on real-valued
  ! fields and a real scalar of precision 'r_bl' specified in an invoke
  ! call (the default precision is 'r_def').
  use constants_mod,  only: r_bl
  use r_bl_field_mod, only: r_bl_field_type

  implicit none

  type(r_bl_field_type) :: f1, f2, f3
  real(r_bl)            :: a

  a = 0.5_r_bl

  call invoke( aX_plus_Y(f3, a, f1, f2) )

end program single_invoke
