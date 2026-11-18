! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: one reduction builtin followed by an access to 
  ! the reduction value.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_write_any_anyd_mod, only: testkern_write_any_anyd_type
  use testkern_write_any_mod, only: testkern_write_any_type
  implicit none

  type(field_type) :: f1, f2, f3, f4, f5, f6, f7
  real(r_def)      :: asum

  call invoke( testkern_write_any_anyd_type(f1, f2, f3, f4, f5, f6, f7), &
               testkern_write_any_type(f1, f2), &
               testkern_write_any_anyd_type(f1, f2, f3, f4, f5, f6, f7))

end program single_invoke
