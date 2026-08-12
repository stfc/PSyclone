! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program any_discontinuous_space_op_example_2

  ! Description: single kernel call in an invoke where the arguments
  ! are specified as any_discontinuous_space, with basis/differential
  ! basis functions
  use field_mod,                                 only : field_type
  use operator_mod,                              only : operator_type
  use quadrature_xyoz_mod,                       only : quadrature_xyoz_type
  use testkern_any_discontinuous_space_op_2_mod, only : &
                  testkern_any_discontinuous_space_op_2_type

  implicit none

  type(field_type)           :: f1
  type(operator_type)        :: op1, op2
  type(quadrature_xyoz_type) :: qr

  call invoke(testkern_any_discontinuous_space_op_2_type(f1, op1, op2, qr))

end program any_discontinuous_space_op_example_2
