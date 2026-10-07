! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multiple_stencil
  ! Description: multiple kernel calls with the same and different
  ! extent and direction names.
  use constants_mod,                only: i_def
  use field_mod,                    only: field_type
  use flux_direction_mod,           only: x_direction, y_direction
  use testkern_stencil_xory1d_mod,  only: testkern_stencil_xory1d_type
  use testkern_stencil_multi_mod,   only: testkern_stencil_multi_type
  use testkern_stencil_multi_2_mod, only: testkern_stencil_multi_2_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: extent = 2,    &
                      f2_extent = 1, &
                      f3_extent = 1
  integer(i_def)   :: direction = y_direction, &
                      f3_direction = x_direction

  call invoke(                                                   &
        testkern_stencil_xory1d_type(f1,                         &
                                     f2, f2_extent, x_direction, &
                                     f3, f4),                    &
        testkern_stencil_multi_type(f1, f2, f2_extent,           &
                                    f3, f3_extent, f3_direction, &
                                    f4, 1),                      &
        testkern_stencil_multi_2_type(f1,                        &
                                      f2, extent, direction,     &
                                      f3, extent, direction,     &
                                      f4, extent, direction)     &
       )

end program multiple_stencil
