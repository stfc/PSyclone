! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_invoke_cma

  ! Description: invoke containing multiple CMA-related kernels
  use field_mod,                          only: field_type
  use operator_mod,                       only: operator_type
  use columnwise_operator_mod,            only: columnwise_operator_type
  use columnwise_op_asm_field_kernel_mod, only: columnwise_op_asm_field_kernel_type
  use columnwise_op_app_kernel_mod,       only: columnwise_op_app_kernel_type
  use columnwise_op_mul_kernel_mod,       only: columnwise_op_mul_kernel_type

  implicit none

  type(operator_type) :: lma_op1
  type(columnwise_operator_type) :: cma_op1, cma_opb, cma_opc
  type(field_type) :: afield, field_a, field_b

  call invoke( columnwise_op_asm_field_kernel_type(afield, lma_op1, cma_op1), &
               columnwise_op_app_kernel_type(field_a, field_b, cma_op1),      &
               columnwise_op_mul_kernel_type(cma_op1, cma_opb, cma_opc))

end program multi_invoke_cma
