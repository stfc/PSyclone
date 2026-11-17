! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: multiply-field point-wise operation specified in an invoke call
  ! where the supplied fields can be deduced to be on the same space.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_fs,   only: testkern_fs_type

  implicit none

  type(field_type) :: f2, f3, f4, f5,f6, f7, f8
  real(r_def)      :: a

  a = 0.5

  call invoke(                                             &
              testkern_fs_type(f2, f3, f4, f5,f6, f7, f8), &
              X_times_Y(f3, a, f4)                         &
             )

end program single_invoke
