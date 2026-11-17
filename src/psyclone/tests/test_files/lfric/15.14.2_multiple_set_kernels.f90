! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: multiple point-wise set operations specified in an invoke call
  ! with the scalar values passed by both value and reference.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1, f2, f3
  real(r_def)      :: fred, ginger

  fred = 20.1_r_def
  ginger = 40.5_r_def
  
  call invoke(                  &
       setval_c(f1, fred),      &
       setval_c(f2, 3.0_r_def), &
       setval_c(f3, ginger)     &
          )

end program single_invoke
