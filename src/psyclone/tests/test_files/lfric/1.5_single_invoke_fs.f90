! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_fs

  ! Description: single function specified in an invoke call using all
  ! function spaces
  use field_mod,       only: field_type
  use testkern_fs_mod, only: testkern_fs_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, f5, f6, &
                      m1, m2, m3, m4, m5, m6, m7

  call invoke(                                      &
       testkern_fs_type(f1, f2, m1, m2, f3, f4,     &
                        m3, m4, f5, f6, m5, m6, m7) &
          )

end program single_invoke_fs
