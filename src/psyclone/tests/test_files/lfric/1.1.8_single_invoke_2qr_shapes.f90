! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026, Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: two kernels specified in an invoke call, each requiring
  ! quadrature but of different shapes.
  use constants_mod,         only: r_def, i_def
  use testkern_qr_mod,       only: testkern_qr_type
  use testkern_qr_faces_mod, only: testkern_qr_faces_type
  use field_mod,             only: field_type
  use quadrature_xyoz_mod,   only: quadrature_xyoz_type
  use quadrature_face_mod,   only: quadrature_face_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  type(field_type) :: g1, g2, n1, n2
  type(quadrature_xyoz_type) :: qr
  type(quadrature_face_type) :: qrf
  real(r_def) :: a, b
  integer(i_def) :: istp

  call invoke( testkern_qr_type(f1, f2, m1, a, m2, istp, qr),   &
               testkern_qr_faces_type(f1, f2, m1, m2, qrf) )

end program single_invoke
