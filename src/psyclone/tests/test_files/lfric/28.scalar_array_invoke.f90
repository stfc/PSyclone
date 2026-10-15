! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program scalar_array_invoke

  ! Description: single function specified in an invoke call to test the
  ! ScalarArray functionality
  use constants_mod,                 only: i_def, r_def, l_def
  use field_mod,                     only: field_type
  use testkern_scalar_array_mod,     only: testkern_scalar_array_type
  use testkern_two_int_scalars_mod,  only: testkern_two_int_scalars_type

  implicit none

  type(field_type)                            :: f1, f2, f3, f4
  real(kind=r_def),    dimension(50, 100)     :: real_array
  logical(kind=l_def), dimension(10)          :: logical_array
  integer(kind=i_def), dimension(2, 5, 10, 8) :: integer_array
  integer(kind=i_def)                         :: a, b, dims_integer_array

  ! Include dims_integer_array as a scalar value to check that the
  ! generated code names do not clash
  call invoke(                                                                                       &
       testkern_scalar_array_type(f1, real_array, logical_array, integer_array, dims_integer_array), &
       testkern_two_int_scalars_type(a, f1, f2, f3, f4, b)                                           &
          )

end program scalar_array_invoke
