! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single point-wise operation (inner product of one field by
  ! itself) specified in an invoke call of double precision.
  use constants_mod, only: r_double
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1
  real(r_double)     :: asum

  call invoke( X_innerproduct_X(asum, f1) )

end program single_invoke
