! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single invoke calling a kernel with multiple stencils accesses
  ! and different values of stencil extents for integer fields.
  ! Note: it is currently not possible to specify kind for an integer
  ! literal stencil depth in a kernel call. This will be enabled when
  ! addressing issue #753.
  use constants_mod,       only: i_def
  use integer_field_mod,   only: integer_field_type
  use flux_direction_mod,  only: x_direction
  use testkern_stencil_multi_int_field_mod, &
                           only: testkern_stencil_multi_int_field_type

  implicit none

  type(integer_field_type) :: f1, f2, f3, f4
  integer(i_def)           :: f2_extent = 1, f3_extent = 2
  integer(i_def)           :: f3_direction = x_direction

  call invoke(                                                    &
       testkern_stencil_multi_int_field_type(f1, f2, f2_extent,   &
                              f3, f3_extent, f3_direction, f4, 2) &
       )

end program single_stencil
