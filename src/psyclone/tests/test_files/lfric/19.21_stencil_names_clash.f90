! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single stencil with an xory1d stencil specified in
  ! the metadata which therefore requires a direction argument. Check
  ! that name clashes are avoided for extent and direction arguments.
  use constants_mod,               only: i_def
  use field_mod,                   only: field_type
  use flux_direction_mod,          only: x_direction
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: f2_stencil_size = 1
  integer(i_def)   :: nlayers_f1 = x_direction

  ! Rename "f2_extent" to "f2_stencil_size" as this is an internally
  ! generated name in the PSy-layer. Similarly, rename "f2_direction" to
  ! "nlayers_f1" as this is an internally generated name in the PSy-layer.

  call invoke(                                                                &
    testkern_stencil_xory1d_type(f1, f2, f2_stencil_size, nlayers_f1, f3, f4) &
       )

end program single_stencil
