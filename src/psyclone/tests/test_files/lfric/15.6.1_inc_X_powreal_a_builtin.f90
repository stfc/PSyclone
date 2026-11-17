! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (raise field to a real power)
  ! specified in an invoke call
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1
  real(r_def)      :: a_scalar

  call invoke( inc_X_powreal_a(f1, a_scalar), &
               inc_X_powreal_a(f1, 1.0e-3_r_def) )

end program single_invoke
