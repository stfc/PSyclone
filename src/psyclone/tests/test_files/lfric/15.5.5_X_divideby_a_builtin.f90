! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (dividing a real-valued field by
  ! a real scalar: Y = X/a) specified in an invoke call.
  use constants_mod,      only: r_solver
  use field_mod,          only: field_type
  use r_solver_field_mod, only: r_solver_field_type

  implicit none

  type(field_type)          :: f1
  type(r_solver_field_type) :: f2
  real(r_solver)            :: a_scalar

  call invoke( X_divideby_a(f2, f1, a_scalar) )

end program single_invoke
