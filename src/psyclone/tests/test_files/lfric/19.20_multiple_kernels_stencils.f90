! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multiple_stencil
  ! Description: multiple kernels in an invoke with each stencil
  ! access being to a different field. Also shared and individual extents.
  ! f2b and f2c have the same stencil dofmap.
  use constants_mod,        only: i_def
  use field_mod,            only: field_type
  use flux_direction_mod,   only: x_direction, y_direction
  use testkern_stencil_mod, only: testkern_stencil_type

  implicit none

  type(field_type) :: f1, f2a, f2b, f2c, f3, f4
  integer(i_def)   :: extent = 2, f2a_extent = 1

  call invoke(                                              &
        testkern_stencil_type(f1, f2a, f2a_extent, f3, f4), &
        testkern_stencil_type(f1, f2b, extent, f3, f4),     &
        testkern_stencil_type(f1, f2c, extent, f3, f4)      &
       )

end program multiple_stencil
