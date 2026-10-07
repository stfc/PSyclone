! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program eval_invoke

  ! Test program containing a single invoke of two kernels, each requiring
  ! an evaluator for basis/diff-basis functions on W2 and W3. However, the
  ! quantity written to by each kernel is on a different space and therefore
  ! the basis/diff-basis functions must be evaluated on different nodal
  ! points for each kernel.
  !
  use field_mod,                  only: field_type
  use operator_mod,               only: operator_type
  use testkern_eval_op_to_w0_mod, only: testkern_eval_op_to_w0_type
  use testkern_eval_op_to_mod,    only: testkern_eval_op_to_type

  implicit none

  type(field_type)    :: f0, f1, f2
  type(operator_type) :: op1, op2
  
  call invoke(                                  &
       testkern_eval_op_to_type(op2, f1),       &
       testkern_eval_op_to_w0_type(op1, f0, f2) &
       )

end program eval_invoke
