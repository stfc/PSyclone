! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel requiring both face and edge quadrature
  ! specified in a single invoke call.
  use field_mod,           only: field_type
  use quadrature_face_mod, only: quadrature_face_type
  use quadrature_edge_mod, only: quadrature_edge_type
  use testkern_2qr_mod,    only: testkern_2qr_type

  implicit none

  type(field_type)           :: f1, f2, m1, m2
  type(quadrature_face_type) :: qr_face
  type(quadrature_edge_type) :: qr_edge

  call invoke( testkern_2qr_type(f1, f2, m1, m2, qr_face, qr_edge) )

end program single_invoke
