! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program qr_field_array

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f0, f1, f2, f3, f4
  type(quadrature_xyoz_type) :: qr0(2,2), qr1(2,2)
  real(r_def)                :: ascal
  integer(i_def)             :: i, j, k(2), l

  call invoke(                                                  &
       testkern_qr_type(f1, f2, f3, ascal, f4, l, qr0(i,j)),    &
       testkern_qr_type(f1, f2, f3, ascal, f4, l, qr0(i,j+1)),  &
       testkern_qr_type(f1, f2, f3, ascal, f4, l, qr1(i,k(l))), &
       testkern_qr_type(f0, f2, f3, ascal, f4, l, qr0(i,j)) )

end program qr_field_array
