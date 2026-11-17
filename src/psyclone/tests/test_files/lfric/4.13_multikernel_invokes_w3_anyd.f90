! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_w3

  ! Description: multiple kernel calls within an invoke iterating over
  ! discontinuous readwriters on w3 and any_discontinuous_space_1 and
  ! reading from continuous fields
  use constants_mod,               only: r_def
  use field_mod,                   only: field_type
  use testkern_w3_mod,             only: testkern_w3_type
  use testkern_anyd_any_space_mod, only: testkern_anyd_any_space_type

  implicit none

  type(field_type) :: f1, f2, m1, m2, m3
  real(r_def)      :: a

  call invoke(                                  &
       testkern_w3_type(a, f1, f2, m1, m2),     &
       testkern_anyd_any_space_type(m3, f1, m1) &
          )

end program multikernel_invokes_w3
