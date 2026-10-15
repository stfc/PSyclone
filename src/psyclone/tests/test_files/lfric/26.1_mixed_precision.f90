! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program r_solver_example

  use constants_mod,      only : r_solver
  use r_solver_field_mod, only : r_solver_field_type
  use field_mod,          only : field_type
  use testkern_mod,       only : testkern_type
  
  implicit none

  type(r_solver_FIELD_type)   :: f1, f2
  type(FIELD_type)            :: f3, f4
  real(R_solver)              :: a

  a = 1.0_r_solver
  call invoke(testkern_type(A, f1, f2, f3, f4))

end program r_solver_example
