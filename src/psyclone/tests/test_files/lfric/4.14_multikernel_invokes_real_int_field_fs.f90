! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_real_int_field_fs

  ! Description: two kernel calls using all supported function spaces
  ! with the first kernel operating on integer-valued fields and the
  ! second kernel operating on real-valued fields
  use field_mod,                 only: field_type
  use integer_field_mod,         only: integer_field_type
  use testkern_fs_int_field_mod, only: testkern_fs_int_field_type
  use testkern_fs_mod,           only: testkern_fs_type

  implicit none

  type(integer_field_type) :: i1, i2, i3, i4, i5, i6, i7, i8, &
                              n1, n2, n3, n4, n5, n6, n7
  type(field_type) :: f1, f2, f3, f4, f5, f6, &
                      m1, m2, m3, m4, m5, m6, m7

  call invoke(name = "Integer_and_real_field",            &
       testkern_fs_int_field_type(i1, i2, n1, n2, i3, i4, &
                                  n3, n4, i5, i6, n5, n6, &
                                  i7, i8, n7),            &
       testkern_fs_type(f1, f2, m1, m2, f3, f4, m3, m4,   &
                        f5, f6, m5, m6, m7)               &
             )

end program multikernel_invokes_real_int_field_fs
