! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation specified in an invoke call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: ginger
  
  call invoke(                                &
       testkern_type(ginger, f1, f2, m1, m2), &
       setval_c(f1, 0.0_r_def)                &
          )

end program single_invoke
