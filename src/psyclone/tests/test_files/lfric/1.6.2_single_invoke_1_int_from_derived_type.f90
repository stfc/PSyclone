! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: three kernels specified in an invoke call where the
  ! one integer is pulled out of a derived type for the first and
  ! is obtained from a type-bound routine in the second and third.
  ! In the third the type-bound routine takes an argument and in the
  ! fourth this argument is itself obtained by dereferencing another
  ! derived type.
  use constants_mod,               only: i_def
  use field_mod,                   only: field_type
  use testkern_one_int_scalar_mod, only: testkern_one_int_scalar_type
  use my_types,                    only: some_type, some_type2

  implicit none

  type(field_type) :: f1, f2, m1, m2
  type(some_type)  :: my_obj 
  type(some_type2) :: int_wrapper
  integer(i_def)   :: switch = 4

  call invoke(                                                                         &
       testkern_one_int_scalar_type(f1, my_obj%iflag, f2, m1, m2),                     &
       testkern_one_int_scalar_type(f1, my_obj%get_flag(), f2, m1, m2),                &
       testkern_one_int_scalar_type(f1, my_obj%get_flag(switch), f2, m1, m2),          &
       testkern_one_int_scalar_type(f1, my_obj%get_flag(int_wrapper%data), f2, m1, m2) &
          )

end program single_invoke
