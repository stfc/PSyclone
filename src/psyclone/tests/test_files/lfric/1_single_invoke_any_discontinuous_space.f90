! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_anyd1

  ! Description: single function in an invoke iterating over
  ! any_discontinuous_space_1 and reading from any_space_1
  ! and any_w2 (continuous)
  use testkern_anyd_any_space_mod, only: testkern_anyd_any_space_type
  use inf,                         only: field_type

  implicit none

  type(field_type) :: f1, f2, f3

  call invoke(                                  &
       testkern_anyd_any_space_type(f1, f2, f3) &
             )

end program single_invoke_anyd1
