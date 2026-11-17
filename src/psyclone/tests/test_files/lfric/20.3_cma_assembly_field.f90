! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_cma

  ! Description: single CMA assembly kernel (with a field argument)
  ! specified in an invoke call
  use field_mod,                          only: field_type
  use operator_mod,                       only: operator_type
  use columnwise_operator_mod,            only: columnwise_operator_type
  use columnwise_op_asm_field_kernel_mod, only: columnwise_op_asm_field_kernel_type

  implicit none

  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1
  type(field_type)               :: afield

  call invoke( columnwise_op_asm_field_kernel_type(afield, lma_op1, cma_op1) )

end program single_invoke_cma
