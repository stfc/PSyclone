! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: four kernels specified in an invoke call where the
  ! one integer argument is obtained from different components of two
  ! different objects
  use field_mod,                   only: field_type
  use testkern_one_int_scalar_mod, only: testkern_one_int_scalar_type
  use my_types,                    only: some_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  type(some_type)  :: obj_a, obj_b 

  call invoke(                                                          &
       testkern_one_int_scalar_type(f1, obj_a%iflag, f2, m1, m2),       &
       testkern_one_int_scalar_type(f1, obj_b%iflag, f2, m1, m2),       &
       testkern_one_int_scalar_type(f1, obj_a%obj_b%iflag, f2, m1, m2), &
       testkern_one_int_scalar_type(f1, obj_b%obj_a%iflag, f2, m1, m2)  &
          )

end program single_invoke
