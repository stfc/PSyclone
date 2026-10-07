! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel requiring properties from both the mesh
  ! and the reference element.
  use constants_mod, only: r_def
  use field_mod, only: field_type
  use testkern_mesh_ref_elem_props_mod, only: testkern_mesh_ref_elem_props_type

  implicit none

  type(field_type) :: f1
  real(r_def) :: a

  call invoke( testkern_mesh_ref_elem_props_type(a,f1) )

end program single_invoke
