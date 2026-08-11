! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


program int_real_literal_scalar

  ! Description: invoke a single kernel with scalar constants
  ! as parameters, that are using a precision
  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use testkern_qr_mod,     only: testkern_qr_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_xyoz_type) :: qr

  call invoke(                                       &
       testkern_qr_type(f1, f2, m1, 1.0_r_def, m2, 2_i_def, qr) &
          )

end program int_real_literal_scalar
