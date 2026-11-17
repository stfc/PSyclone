! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_6

  ! Multiple kernel calls within an invoke where the kernels are specified
  ! as any_space

  use constants_mod,            only : r_def
  use field_mod,                only : field_type
  use operator_mod,             only : operator_type
  use quadrature_xyoz_mod,      only : quadrature_xyoz_type
  use testkern_any_space_1_mod, only : testkern_any_space_1_type
  use testkern_any_space_2_mod, only : testkern_any_space_2_type

  implicit none

  type(field_type)           :: f1, f2, f3(3)
  type(operator_type)        :: op
  type(quadrature_xyoz_type) :: qr
  real(r_def)                :: rdt

  call invoke(                                         &
       testkern_any_space_1_type(f1, rdt, f2, f3, qr), &
       testkern_any_space_1_type(f2, rdt, f1, f3, qr), &
       testkern_any_space_2_type(f1, f2, op, istep),   &
       testkern_any_space_2_type(f2, f1, op, istep)    &
       )

end program multikernel_invokes_6
