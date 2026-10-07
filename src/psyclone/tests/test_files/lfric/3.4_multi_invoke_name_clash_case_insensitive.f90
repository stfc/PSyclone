! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_functions_multi_invokes

  ! Description: multiple invoke calls which are (incorrectly) given the
  ! same name, albeit capitalised differently.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a, b

  call invoke(name="jack",                 &
              testkern_type(a, f1, f2, m1, m2))
  call invoke(name="Jack",                 &
              testkern_type(b, f1, f2, m1 ,m2))

end program multi_functions_multi_invokes
