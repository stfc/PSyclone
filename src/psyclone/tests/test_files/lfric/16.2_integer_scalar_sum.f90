! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: single kernel, single int scalar sum & field reader argument.
  ! Tests that using incorrect meta-data to perform a reduction into an
  ! integer variable raises the expected error.
  use constants_mod, only: i_def
  use field_mod,     only: field_type

  implicit none

  integer(i_def)   :: isum
  type(field_type) :: f1, f2

  call invoke( X_innerproduct_Y(isum, f1, f2) )

end program single_invoke
