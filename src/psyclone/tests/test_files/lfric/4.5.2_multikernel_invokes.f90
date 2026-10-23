! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multikernel_invokes_452

  ! Multiple kernel calls within an invoke where the arguments are specified
  ! as any_space

  use constants_mod,            only : i_def, r_def
  use field_mod,                only : field_type
  use operator_mod,             only : operator_type
  use quadrature_xyoz_mod,      only : quadrature_xyoz_type
  use testkern_any_space_1_mod, only : testkern_any_space_1_type
  use testkern_any_space_2_mod, only : testkern_any_space_2_type
  use testkern_any_space_3_mod, only : testkern_any_space_3_type
  use testkern_any_space_4_mod, only : testkern_any_space_4_type

  implicit none

  type(field_type)           :: f1, f2, f3(3)
  type(operator_type)        :: op, op2, op3, op4, op5
  type(quadrature_xyoz_type) :: qr
  integer(i_def)             :: scalar
  real(r_def)                :: rdt

  call invoke(                                             &
       ! any1, any2, W0
       testkern_any_space_1_type(f1, rdt, f2, f3, qr),     &
       ! any1, any1, any1-any1
       testkern_any_space_2_type(f1, f2, op, scalar),      &
       ! any1, any1, any1-any1
       testkern_any_space_2_type(f2, f1, op, scalar),      &
       ! any1-any2
       testkern_any_space_3_type(op),                      &
       ! any5, any1-any2, any3-any2, any4-any4, any3-any5, any4
       testkern_any_space_4_type(f2,op2,op3,op4,op5,f1,qr) &
       )

end program multikernel_invokes_452
