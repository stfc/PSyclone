! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_7

  ! Multiple kernel calls within an invoke where the kernels update a
  ! field with INC access

  use constants_mod,       only : r_def, i_def
  use field_mod,           only : field_type
  use quadrature_xyoz_mod, only : quadrature_xyoz_type
  use ru_kernel_mod,       only : ru_kernel_type

  implicit none

  type(field_type)           :: a, b, d, e(3), f
  integer(i_def)             :: istp
  real(r_def)                :: rdt
  type(quadrature_xyoz_type) :: qr

  call invoke( ru_kernel_type(a, b, istp, rdt, d, e, qr), &
               ru_kernel_type(f, b, istp, rdt, d, e, qr) )

end program multikernel_invokes_7
