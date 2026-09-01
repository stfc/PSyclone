! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_stencil
  ! Description: single stencil with an xory1d stencil specified in
  ! the metadata which therefore requires a direction argument. Check
  ! that unique names are produced in the PSy layer for the extent and
  ! direction arguments when we use array indices.
  use constants_mod,               only: i_def
  use field_mod,                   only: field_type
  use flux_direction_mod,          only: x_direction
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: f2_info(2,2)

  ! Access extent and direction arguments via an array with different
  ! indices in the same invoke call. The generated extent and
  ! direction names in the PSy-layer should be unique.

  call invoke(                                                                   &
       testkern_stencil_xory1d_type(f1, f2, f2_info(1,1), f2_info(1,2), f3, f4), &
       testkern_stencil_xory1d_type(f1, f2, f2_info(2,1), f2_info(2,2), f3, f4)  &
       )

end program single_stencil
