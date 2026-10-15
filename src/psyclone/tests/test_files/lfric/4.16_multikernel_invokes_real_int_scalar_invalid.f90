! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_real_int_scalar_invalid

  ! Description: two kernel calls with the first kernel using two real
  ! scalars and the second kernel using two integer scalars but
  ! incorrectly passing a scalar that would need to be both real and
  ! integer. This is provided from a module to stop PSyclone's type
  ! checking from raising an exception.

  use constants_mod,                 only: r_def, i_def
  use field_mod,                     only: field_type
  use testkern_two_real_scalars_mod, only: testkern_two_real_scalars_type
  use testkern_two_int_scalars_mod,  only: testkern_two_int_scalars_type

  use unknown_mod, only : b

  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  integer(i_def)   :: iflag

  call invoke(name = "real_and_integer_scalars",               &
       testkern_two_real_scalars_type(a, f1, f2, m1, m2, b),   &
       testkern_two_int_scalars_type(iflag, f1, f2, m1, m2, b) &
             )

end program multikernel_invokes_real_int_scalar_invalid
