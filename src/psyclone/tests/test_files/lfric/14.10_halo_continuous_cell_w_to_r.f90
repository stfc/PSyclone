! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_continuous_cell_w_to_r

  ! Description: dependency between a field being written to in one
  ! loop and read in a following loop, where the field is a continuous
  ! field and both loops iterate over cells. In this case, when the
  ! field is written to, it must compute in the first level halo in
  ! order to compute valid values for owned cells. In general the
  ! outermost halo level will be invalid and must therefore remain
  ! dirty.

  use field_mod,       only: field_type
  use testkern_w0_mod, only: testkern_w0_type

  implicit none

  type(field_type) :: f1, f2, f3

  call invoke(                    &
       testkern_w0_type(f1, f2),  &
       testkern_w0_type(f3, f1)   &
          )

end program halo_continuous_cell_w_to_r
