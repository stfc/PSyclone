! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single stencil with an xory1d stencil specified in
  ! the metadata which therefore requires a direction argument.
  use constants_mod,               only: i_def
  use field_mod,                   only: field_type
  use flux_direction_mod,          only: x_direction
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: f2_extent = 1
  integer(i_def)   :: f2_direction = x_direction

  call invoke(                                                               &
       testkern_stencil_xory1d_type(f1, f2, f2_extent, f2_direction, f3, f4) &
       )

end program single_stencil
