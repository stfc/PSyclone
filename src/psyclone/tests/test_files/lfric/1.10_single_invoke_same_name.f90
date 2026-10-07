! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function specified in an invoke call with the same
  ! name (f1) being passed in twice. This should make PSyclone raise an
  ! exception
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, m1, m2
  real(r_def)      :: a

  call invoke(                          &
       testkern_type(a, f1, f1, m1, m2) &
          )

end program single_invoke
