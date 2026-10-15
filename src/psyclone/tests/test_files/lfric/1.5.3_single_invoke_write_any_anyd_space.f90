! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_fs

  ! Description: single function that writes to fields on any_space (continuous)
  ! and any_discontinuous_space
  use field_mod,                   only: field_type
  use testkern_write_any_anyd_mod, only: testkern_write_any_anyd_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, m1, m2, m3

  call invoke( testkern_write_any_anyd_type(f1, m1, m2, f2, f3, f4, m3) )

end program single_invoke_fs
