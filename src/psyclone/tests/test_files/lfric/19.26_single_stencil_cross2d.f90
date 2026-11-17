! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil_cross2d
  ! Description: single 2D stencil specified in an invoke call
  use constants_mod,                only: i_def
  use field_mod,                    only: field_type
  use testkern_stencil_cross2d_mod, only: testkern_stencil_cross2d_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: f2_extent=1

  call invoke(                                          &
       testkern_stencil_cross2d_type(f1, f2, f2_extent, f3, f4) &
       )

end program single_stencil_cross2d
