! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: two different builtin reductions specified in an invoke call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1, f2
  real(r_def)      :: asum, bsum

  call invoke( X_innerproduct_Y(asum, f1, f2), &
               sum_X(bsum, f1) )

end program single_invoke
