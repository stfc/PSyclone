! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: A very long assignment statement with continuation characters
  use constants_mod, only: i_def, r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  integer(i_def)   :: my_very_long_index
  integer(i_def)   :: my_very_long_lookup_name(10)
  real(r_def)      :: my_very_long_value_name
  real(r_def)      :: my_very_long_variable_name(10)

  my_very_long_variable_name(my_very_long_lookup_name(my_very_long_index)) = &
  my_very_long_variable_name(my_very_long_lookup_name(my_very_long_index)) + &
  my_very_long_value_name

  call invoke(                          &
       testkern_type(a, f1, f2, m1, m2) &
          )

end program single_invoke
