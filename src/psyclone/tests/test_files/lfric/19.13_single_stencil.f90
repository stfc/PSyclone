! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single stencil specified in an invoke call with field
  ! names that will clash with internal PSy-layer names
  use constants_mod,        only: i_def
  use field_mod,            only: field_type
  use testkern_stencil_mod, only: testkern_stencil_type

  implicit none

  type(field_type) :: f2_stencil_map, f2, f3, f3_stencil_map, &
                      f2_stencil_dofmap, stencil_cross,f3_stencil_dofmap
  integer(i_def)   :: f2_extent = 1, f3_stencil_size = 1

  call invoke(                                                    &
       testkern_stencil_type(f2_stencil_map, f2, f2_extent,       &
                             f2_stencil_dofmap, stencil_cross),   &
       testkern_stencil_type(f3_stencil_map, f3, f3_stencil_size, &
                             f3_stencil_dofmap, stencil_cross)    &
       )

end program single_stencil
