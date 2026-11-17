! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_kernel_multi_field_same_stencil
  ! Description: an example where the same stencil is used by different
  ! fields in a single kernel (f1 and f2 are the same and f3 and f4 are
  ! the same). Therefore we should only generate a single stencil dofmap
  ! for each.
  use constants_mod,      only: i_def
  use field_mod,          only: field_type
  use flux_direction_mod, only: y_direction
  use testkern_multi_field_same_stencil_mod, &
                          only: testkern_multi_field_same_stencil_type

  implicit none

  type(field_type) :: f0, f1, f2, f3, f4
  integer(i_def)   :: extent = 2
  integer(i_def)   :: direction = y_direction

  call invoke(                                                       &
       testkern_multi_field_same_stencil_type(f0,                    &
                                              f1, extent, f2,extent, &
                                              f3, extent, direction, &
                                              f4, extent, direction) &
       )

end program single_kernel_multi_field_same_stencil
