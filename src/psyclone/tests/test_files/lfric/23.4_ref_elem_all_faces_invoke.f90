! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel (requiring all normals and outward normals to
  ! faces of the reference element with the common number of faces) specified
  ! in an invoke call
  use constants_mod,                   only: r_def
  use field_mod,                       only: field_type
  use testkern_ref_elem_all_faces_mod, only: testkern_ref_elem_all_faces_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke( testkern_ref_elem_all_faces_type(a, f1, f2, m1, m2) )

end program single_invoke
