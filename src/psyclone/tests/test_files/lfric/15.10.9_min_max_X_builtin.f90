! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: three point-wise operations (setval, min and max of field
  ! elements) specified in an invoke call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1
  real(r_def)      :: amin, amax

  call invoke( setval_C(f1, 1.0_r_def), &
               minval_X(amin, f1),      &
               maxval_X(amax, f1) )

end program single_invoke
