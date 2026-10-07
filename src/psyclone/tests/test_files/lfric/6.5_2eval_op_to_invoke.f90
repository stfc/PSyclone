! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of two kernels, each requiring
  ! an evaluator but writing to different types (field and operator) on
  ! different spaces. One of those spaces is the 'to' space of the operator.
  use field_mod,               only: field_type
  use operator_mod,            only: operator_type
  use testkern_eval_mod,       only: testkern_eval_type
  use testkern_eval_op_to_mod, only: testkern_eval_op_to_type

  implicit none

  type(field_type)    :: f0, f1, f2, f3
  type(operator_type) :: op1
  
  call invoke(                            &
       testkern_eval_type(f0, f1),        &
       testkern_eval_op_to_type(op1, f2)  &
       )

end program eval_invoke
