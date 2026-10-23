! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single stencil with multiple extents having different
  ! values
  ! Note: it is currently not possible to specify kind for an integer
  ! literal stencil depth in a kernel call. This will be enabled when
  ! addressing issue #1618.
  use constants_mod,              only: i_def
  use field_mod,                  only: field_type
  use flux_direction_mod,         only: y_direction
  use testkern_stencil_multi_mod, only: testkern_stencil_multi_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: f2_extent = 2, f3_extent = 1
  integer(i_def)   :: f3_direction = y_direction

  call invoke(                                                       &
       testkern_stencil_multi_type(f1, f2, f2_extent, f3, f3_extent, &
                                   f3_direction, f4, 1)              &
       )

end program single_stencil
