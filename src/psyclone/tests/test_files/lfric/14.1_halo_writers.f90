! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_writers

  ! Description: single function specified in an invoke call checking that
  ! "halo_dirty" calls are generated only for "write" fields (write,
  ! readwrite and inc access) but not for "read" fields.
  use field_mod,            only: field_type
  use testkern_writers_mod, only: testkern_writers_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, f5, f6, f7, f8

  call invoke(                                               &
       testkern_writers_type(f1, f2, f3, f4, f5, f6, f7, f8) &
          )

end program halo_writers
