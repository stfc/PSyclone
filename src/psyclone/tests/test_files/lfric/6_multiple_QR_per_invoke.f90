! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_qr_per_invoke

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f0, f1, f2, f3, f4
  type(quadrature_xyoz_type) :: qr0, qr1
  real(r_def)                :: ascalar
  integer(i_def)             :: iscalar

  call invoke(                                                  &
       testkern_qr_type(f1, f2, f3, ascalar, f4, iscalar, qr0), &
       testkern_qr_type(f1, f2, f3, ascalar, f4, iscalar, qr1), &
       testkern_qr_type(f0, f2, f3, ascalar, f4, iscalar, qr0) )

end program multi_qr_per_invoke
