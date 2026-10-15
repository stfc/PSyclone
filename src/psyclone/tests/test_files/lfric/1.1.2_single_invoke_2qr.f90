! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: two functions specified in an invoke call, each requiring
  ! qr.
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(field_type)           :: g1, g2, n1, n2
  type(quadrature_xyoz_type) :: qr, qr2
  real(r_def)                :: a, b
  integer(i_def)             :: istp

  call invoke(                                        &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr), &
       testkern_qr_type(g1, g2, n1, b, n2, istp, qr2) &
          )

end program single_invoke
