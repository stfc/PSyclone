! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_fs

  ! Description: single function specified in an invoke call using
  ! all function spaces with one continuous writer (w1) and one
  ! discontinuous writer (w2broken)
  use field_mod,                      only: field_type
  use testkern_write_w2broken_w1_mod, only: testkern_write_w2broken_w1_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, f5, f6, &
                      m1, m2, m3, m4, m5, m6

  call invoke(                                                 &
       testkern_write_w2broken_w1_type(f1, f2, m1, m2, f3, f4, &
                                       m3, m4, f5, f6, m5, m6) &
             )

end program single_invoke_fs
