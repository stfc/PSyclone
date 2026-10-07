! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel requiring properties from both the mesh
  ! and face quadrature.
  use constants_mod, only: r_def
  use field_mod, only: field_type
  use quadrature_face_mod, only: quadrature_face_type
  use testkern_mesh_prop_face_qr_mod, only: testkern_mesh_prop_face_qr_type

  implicit none

  type(field_type) :: f1
  real(r_def) :: a
  type(quadrature_face_type) :: qr

  call invoke( testkern_mesh_prop_face_qr_type(a,f1,qr) )

end program single_invoke
