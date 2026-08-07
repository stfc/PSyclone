! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_functions_multi_invokes

  ! Description: multiple invoke calls, each with a single function
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_mod,        only: testkern_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp

  call invoke(                                       &
       testkern_type(a, f1, f2, m1, m2),             &
       testkern_type(a, f1, f2, m1, m2),             &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr) &
       )

  call invoke(                                        &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr), &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr), &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr)  &
       )

end program multi_functions_multi_invokes
