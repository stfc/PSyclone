! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_wtheta_only_vector

  ! Description: two functions in an invoke iterating over and
  ! reading from wtheta field vectors (discontinuous)
  use field_mod,                       only: field_type
  use testkern_wtheta_only_vector_mod, only: testkern_wtheta_only_vector_type

  implicit none

  type(field_type) :: f1(3), f2(3), f3(3)

  call invoke(                                   &
       testkern_wtheta_only_vector_type(f1, f2), &
       ! Field f1 readwrite to read dependence but no halo exchange
       ! required as wtheta is discontinuous
       testkern_wtheta_only_vector_type(f3, f1)  &
          )

end program single_invoke_wtheta_only_vector
