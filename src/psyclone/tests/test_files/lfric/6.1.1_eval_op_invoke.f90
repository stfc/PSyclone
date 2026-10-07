! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of a kernel that
  ! requires an evaluator
  use field_mod,            only: field_type
  use operator_mod,         only: operator_type
  use testkern_eval_op_mod, only: testkern_eval_op_type

  implicit none

  type(field_type)    :: f1
  type(operator_type) :: op1

  call invoke( testkern_eval_op_type(op1, f1) )

end program eval_invoke
