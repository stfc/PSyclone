! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_wtheta_only_vector

  ! Description: single function in an invoke iterating over and
  ! reading from wtheta field vectors (discontinuous)
  use field_mod,                       only: field_type
  use field_real64_mod,                only: field_real64_type
  use testkern_wtheta_only_vector_mod, only: testkern_wtheta_only_vector_type

  implicit none

  type(field_type) :: f1(3)
  type(field_real64_type) :: f2(3)

  call invoke(                                  &
       testkern_wtheta_only_vector_type(f1, f2) &
          )

end program single_invoke_wtheta_only_vector
