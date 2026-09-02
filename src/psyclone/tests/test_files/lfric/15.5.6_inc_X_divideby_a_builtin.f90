! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (dividing a real-valued field by
  ! a real scalar: X = X/a) specified in an invoke call.
  use constants_mod,    only: r_def
  use r_tran_field_mod, only: r_tran_field_type

  implicit none

  type(r_tran_field_type) :: f1
  real(r_def)             :: a_scalar

  call invoke( inc_X_divideby_a(f1, a_scalar) )

end program single_invoke
