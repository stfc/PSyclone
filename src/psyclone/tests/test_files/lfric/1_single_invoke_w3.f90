! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_w3

  ! Description: single function iterating over w3 (discontinuous)
  ! specified in an invoke call
  use constants_mod,   only: r_def
  use field_mod,       only: field_type
  use testkern_w3_mod, only: testkern_w3_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke(                             &
       testkern_w3_type(a, f1, f2, m1, m2) &
          )

end program single_invoke_w3
