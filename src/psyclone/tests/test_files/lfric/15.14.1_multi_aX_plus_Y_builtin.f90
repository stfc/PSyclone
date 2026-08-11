! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: multi aX_plus_Y point-wise operations
  ! specified in an invoke call.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  type(field_type) :: f1, f2(7), f3
  real(r_def)      :: a

  a = 0.5

  call invoke(                             &
              aX_plus_Y(f2(1), a, f1, f3), &
              aX_plus_Y(f2(2), a, f1, f3), &
              aX_plus_Y(f2(3), a, f1, f3), &
              aX_plus_Y(f3, a, f1, f2(4)), &
              aX_plus_Y(f2(5), a, f1, f3), &
              aX_plus_Y(f2(6), a, f1, f3), &
              aX_plus_Y(f2(7), a, f1, f3)  &
             )

end program single_invoke
