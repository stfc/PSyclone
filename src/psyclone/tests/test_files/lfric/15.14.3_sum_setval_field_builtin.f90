! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: three point-wise operations specified in an invoke call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1
  real(r_def)      :: asum = 1.0

  call invoke( setval_c(f1, asum), &
               sum_X(asum, f1),    &
               setval_c(f1, asum) ) 

end program single_invoke
