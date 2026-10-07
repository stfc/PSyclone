! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: kernel with incorrect shape specified in metadata
  use constants_mod,               only: r_def, i_def
  use field_mod,                   only: field_type
  use quadrature_xyoz_mod,         only: quadrature_xyoz_type
  use testkern_wrong_shape_qr_mod, only: testkern_wrong_shape_qr_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: a
  integer(i_def)             :: istp

  call invoke( testkern_wrong_shape_qr_type(f1, f2, m1, a, m2, istp, qr) )

end program single_invoke
