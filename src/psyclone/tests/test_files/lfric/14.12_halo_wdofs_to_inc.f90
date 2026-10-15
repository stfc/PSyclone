! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_dofs_write_to_inc

  ! Description: dependency between a field being written to in one
  ! loop and read in a following loop, where the field is a continuous
  ! field, the first loop iterates over dofs and the second iterates
  ! over cells.

  use constants_mod,   only: r_def
  use field_mod,       only: field_type
  use testkern_w0_mod, only: testkern_w0_type

  implicit none

  type(field_type) :: f1, f2

  call invoke(                    &
       setval_c(f1, 0.0_r_def),   &
       testkern_w0_type(f1, f2)   &
          )

end program halo_dofs_write_to_inc
