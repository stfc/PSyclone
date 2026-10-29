! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: invokes a single built-in kernel that claims to perform
  ! two reduction operations (forbidden in the LFRic API) and then
  ! write to a field. Must be used with the fake kernel meta-data in
  ! multi_reduction_builtins_mod.f90.
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

  real(r_def)      :: rsum1, rsum2
  type(field_type) :: f1

  call invoke( X_innerproduct_Y(f1, rsum1, rsum2) )

end program single_invoke
