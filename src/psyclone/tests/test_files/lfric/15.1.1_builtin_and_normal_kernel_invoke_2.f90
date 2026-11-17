! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: mixed kernel and builtin specified in an invoke call
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: ginger
  
  call invoke(                                &
       ! f1 write w1, f2 read w2, m1 read w2, m2 read w3
       testkern_type(ginger, f1, f2, m1, m2), &
       ! f1 readwrite, f2 read
       inc_aX_plus_Y(0.5_r_def, f1, f2)       &
          )

end program single_invoke
