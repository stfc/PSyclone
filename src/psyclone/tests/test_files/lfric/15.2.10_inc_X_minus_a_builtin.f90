! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (X = X - a, DoF-wise subtraction
  ! of a real scalar value from a real-valued field) specified in an invoke call.
  use constants_mod,    only: r_tran
  use r_tran_field_mod, only: r_tran_field_type

  implicit none

  type(r_tran_field_type) :: f1
  real(r_tran)            :: a

  a = 1.5_r_tran

  call invoke( inc_X_minus_a(f1, a) )

end program single_invoke
