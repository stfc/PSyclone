! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: call a kernel that reads from a chi field
  use testkern_chi_read_mod, only: testkern_chi_read_type
  use field_mod,             only: field_type

  implicit none

  type(field_type) :: f1, f2(3)

  call invoke(testkern_chi_read_type(f1, f2))

end program single_invoke
