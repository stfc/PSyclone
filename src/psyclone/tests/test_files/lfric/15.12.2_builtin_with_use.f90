! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise set operation specified in an invoke call
  ! with the scalar value passed by reference.
  ! Forbidden use statement for the built-in operation - built-ins do
  ! not have associated use statements.
  use constants_mod,    only: r_def
  use field_mod,        only: field_type
  use fake_builtin_mod, only: setval_c

  implicit none

  type(field_type) :: f1
  real(r_def)      :: fred

  fred = 20.1_r_def

  call invoke( setval_c(f1, fred) )

end program single_invoke
