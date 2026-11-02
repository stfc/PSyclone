! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_1

  ! Multiple kernel calls within an invoke

  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, m1, m2, m3, m4
  real(r_def)      :: a

  call invoke(                           &
       testkern_type(a, f1, f2, m1, m2), &
       testkern_type(a, f3, f4, m3, m4)  &
       )

end program multikernel_invokes_1
