! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_cma

  ! Description: single CMA-assembly kernel specified in an invoke, assembled
  ! from LMA representation of the operator mapping between two horizontally
  ! discontinuous spaces (both any_discontinuous_space_*)

  use field_mod,                    only: field_type
  use columnwise_operator_mod,      only: columnwise_operator_type
  use columnwise_op_asm_kernel_mod, only: columnwise_op_asm_kernel_type

  implicit none

  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1

  call invoke(                                            &
          columnwise_op_asm_kernel_type(lma_op1, cma_op1) &
          )

end program single_invoke_cma
