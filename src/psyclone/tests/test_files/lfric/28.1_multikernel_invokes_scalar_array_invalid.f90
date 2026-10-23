! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_scalar_array_invalid

  ! Description: two kernel calls with the first kernel using a real
  ! ScalarArray and the second kernel usign a logical ScalarArray
  ! incorrectly passing a ScalarArray that would need to be both real
  ! and logical. This is provided from a module to stop PSyclone's type
  ! checking from raising an exception.
  use constants_mod,             only: i_def, r_def, l_def
  use field_mod,                 only: field_type
  use testkern_scalar_array_mod, only: testkern_scalar_array_type

  use unknown_mod, only : b

  implicit none

  type(field_type)                       :: afield
  real(r_def),    dimension(50, 100)     :: real_array
  logical(l_def), dimension(10)          :: logical_array
  integer(i_def), dimension(2, 5, 10, 8) :: integer_array
  integer(i_def)                         :: a_scalar

  call invoke(name = "real_and_logical_scalars",                                  &
       testkern_scalar_array_type(afield,b,logical_array,integer_array,a_scalar), &
       testkern_scalar_array_type(afield,real_array,b,integer_array,a_scalar)     &
             )

end program multikernel_invokes_scalar_array_invalid
