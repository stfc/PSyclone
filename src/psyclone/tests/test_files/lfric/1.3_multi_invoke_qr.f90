! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_invoke_qr

  ! Description: three kernels specified in an invoke call, two of which
  ! require XYoZ quadrature.

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type
  use testkern_mod,        only: testkern_type

  implicit none

  type(field_type)           :: f1, f2, f3, m1, m2, m3
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp

  call invoke(                                        &
       testkern_qr_type(f1, f2, m1, a, m2, istp, qr), &
       testkern_type(a, f1, f2, m1, m3),              &
       testkern_qr_type(f1, f3, m1, a, m2, istp, qr)  &
       )

end program multi_invoke_qr
