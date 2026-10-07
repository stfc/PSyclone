! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_fs_int_field

  ! Single function specified in an invoke call using all function spaces
  ! with with two continuous (w2 and any_space_1) and two discontinuous
  ! (w2broken and any_discontinuous_space_1) writers on integer-valued fields
  use integer_field_mod,         only: integer_field_type
  use testkern_fs_int_field_mod, only: testkern_fs_int_field_type

  implicit none

  type(integer_field_type) :: f1, f2, f3, f4, f5, f6, f7, f8, &
                              m1, m2, m3, m4, m5, m6, m7

  call invoke( testkern_fs_int_field_type(f1, f2, m1, m2, &
                                          f3, f4, m3, m4, &
                                          f5, f6, m5, m6, &
                                          f7, f8, m7)     &
             )

end program single_invoke_fs_int_field
