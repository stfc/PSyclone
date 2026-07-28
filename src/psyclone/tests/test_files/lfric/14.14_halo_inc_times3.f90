! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_inc_times3

  ! Description: three kernel calls where the associated loops iterate
  ! over cells. Each kernel call increments a continuous field and
  ! reads from a contrinuous field. Field f1 has gh_inc to gh_read and
  ! gh_read to gh_inc dependencies. Field f3 has unknown to gh_inc and
  ! gh_inc to unknown dependencies.

  use field_mod,       only: field_type
  use testkern_w0_mod, only: testkern_w0_type

  implicit none

  type(field_type) :: f1, f2, f3, f4

  call invoke(                    &
       testkern_w0_type(f1, f2),  &
       testkern_w0_type(f3, f1),  &
       testkern_w0_type(f1, f4)   &
          )

end program halo_inc_times3
