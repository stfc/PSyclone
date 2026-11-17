! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_5

  ! Multiple kernel calls within an invoke where the kernels require
  ! operator information

  use constants_mod,         only : i_def
  use field_mod,             only : field_type
  use operator_mod,          only : operator_type
  use quadrature_xyoz_mod,   only : quadrature_xyoz_type
  use testkern_operator_mod, only : testkern_operator_type

  implicit none

  type(field_type)           :: f1(3)
  type(operator_type)        :: op
  type(quadrature_xyoz_type) :: qr
  integer(i_def)             :: a, b

  a = 1.0_i_def
  b = 2.0_i_def

  call invoke(                                &
       testkern_operator_type(op, f1, a, qr), &
       testkern_operator_type(op, f1, b, qr)  &
       )

end program multikernel_invokes_5
