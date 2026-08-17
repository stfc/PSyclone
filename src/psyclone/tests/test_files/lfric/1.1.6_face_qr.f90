! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single function requiring face quadrature specified in
  ! an invoke call
  use field_mod,             only: field_type
  use quadrature_face_mod,   only: quadrature_face_type
  use testkern_qr_faces_mod, only: testkern_qr_faces_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_face_type) :: qr

  call invoke( testkern_qr_faces_type(f1, f2, m1, m2, qr) )

end program single_invoke
