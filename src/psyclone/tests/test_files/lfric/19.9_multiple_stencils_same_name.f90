! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single kernel call with multiple directions having
  ! the same name
  use constants_mod,                only: i_def
  use field_mod,                    only: field_type
  use flux_direction_mod,           only: y_direction
  use testkern_stencil_multi_2_mod, only: testkern_stencil_multi_2_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: extent = 2
  integer(i_def)   :: direction = y_direction

  call invoke(                                              &
       testkern_stencil_multi_2_type(f1,                    &
                                     f2, extent, direction, &
                                     f3, extent, direction, &
                                     f4, extent, direction) &
       )

end program single_stencil
