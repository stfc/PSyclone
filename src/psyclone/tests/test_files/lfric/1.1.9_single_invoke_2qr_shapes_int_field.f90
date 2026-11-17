! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel with integer fields and requiring both XYoZ and
  ! face quadrature specified in a single invoke call
  use constants_mod,              only: i_def
  use integer_field_mod,          only: integer_field_type
  use quadrature_xyoz_mod,        only: quadrature_xyoz_type
  use quadrature_face_mod,        only: quadrature_face_type
  use testkern_2qr_int_field_mod, only: testkern_2qr_int_field_type

  implicit none

  type(integer_field_type)   :: f1, f2(3), f3
  type(quadrature_xyoz_type) :: qr_xyoz
  type(quadrature_face_type) :: qr_face
  integer(i_def)             :: istp

  call invoke( testkern_2qr_int_field_type(f1, f2, f3, istp, qr_xyoz, qr_face) )

end program single_invoke
