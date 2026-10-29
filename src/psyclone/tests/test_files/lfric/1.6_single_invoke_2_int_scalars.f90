! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: kernel that has two integer, scalar arguments
  ! specified in an invoke call. One called by value.
  use constants_mod,                only: i_def
  use field_mod,                    only: field_type
  use testkern_two_int_scalars_mod, only: testkern_two_int_scalars_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  integer(i_def)   :: iflag, istep

  call invoke(                                                      &
       testkern_two_int_scalars_type(iflag, f1, f2, m1, m2, istep), &
       testkern_two_int_scalars_type(1, f1, f2, m1, m2, iflag)   )

end program single_invoke
