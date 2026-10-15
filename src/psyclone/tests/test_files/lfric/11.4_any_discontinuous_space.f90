! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program any_discontinuous_space_op_example_1

  ! Description: single kernel call in an invoke where the arguments are
  ! specified as any_discontinuous_space
  use constants_mod,                             only : r_def
  use field_mod,                                 only : field_type
  use operator_mod,                              only : operator_type
  use testkern_any_discontinuous_space_op_1_mod, only : &
                  testkern_any_discontinuous_space_op_1_type

  implicit none

  type(field_type)      :: f1(3), f2
  type(operator_type)   :: op3, op4
  real(r_def)           :: rdt

  call invoke(testkern_any_discontinuous_space_op_1_type(f1, f2, op3, op4, rdt))

end program any_discontinuous_space_op_example_1
