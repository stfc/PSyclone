! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_4

  ! Multiple kernel calls with quadrature within an Invoke where a field is
  ! passed first as a continuous writer and then as a continuous reader

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f1, f2, f3, f4
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp

  call invoke(                                        &
       testkern_qr_type(f1, f2, f3, a, f4, istp, qr), &
       testkern_qr_type(f2, f1, f3, a, f4, istp, qr)  &
       )

end program multikernel_invokes_4
