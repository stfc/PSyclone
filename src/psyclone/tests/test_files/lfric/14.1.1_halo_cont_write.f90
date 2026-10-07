! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program halo_writers

  ! Description: single function specified in an invoke call checking that
  ! "halo_dirty" call is generated for a field on a continuous function
  ! space with 'GH_WRITE' access.
  use field_mod,              only: field_type
  use testkern_write_any_mod, only: testkern_write_any_type

  implicit none

  type(field_type) :: f1, f2

  call invoke( testkern_write_any_type(f1, f2) )

end program halo_writers
