! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_7

  ! Multiple kernel calls within a named invoke

  use constants_mod,       only: r_def, i_def
  use field_mod,           only: field_type
  use quadrature_xyoz_mod, only: quadrature_xyoz_type
  use ru_kernel_mod,       only: ru_kernel_type
  use testkern_mod,        only: testkern_type

  implicit none

  type(field_type)           :: invoke_a, b, c, d, e(3), f, g
  real(r_def)                :: ascalar, rdt
  integer(i_def)             :: istp
  type(quadrature_xyoz_type) :: qr

  call invoke( ru_kernel_type(invoke_a, b, istp, rdt, d, e, qr), &
               name="a",                                         &
               testkern_type(ascalar, f, b, c, g) )

end program multikernel_invokes_7
