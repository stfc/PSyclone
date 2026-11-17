! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_wtheta_w3

  ! Description: single function in an invoke iterating over wtheta and
  ! reading from w3 (both discontinuous)
  use field_mod,           only: field_type
  use testkern_wtheta_mod, only: testkern_wtheta_type

  implicit none

  type(field_type) :: f1, f2

  call invoke(                      &
       testkern_wtheta_type(f1, f2) &
          )

end program single_invoke_wtheta_w3
