! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Description: Test that the precision variable of an integer scalar
! is added to the PSy-layer if it does not already exist. As i_def is
! added by default we need to provide a different precision name (and
! choose roo_def).

program integer_scalar_precision

  use constants_mod,               only: roo_def
  use field_mod,                   only: field_type
  use testkern_one_int_scalar_mod, only: testkern_one_int_scalar_type

  implicit none

  type(field_type) :: f1, f2, m1, m2
  integer(roo_def) :: iflag

  call invoke( testkern_one_int_scalar_type(f1, iflag, f2, m1, m2) )

end program integer_scalar_precision
