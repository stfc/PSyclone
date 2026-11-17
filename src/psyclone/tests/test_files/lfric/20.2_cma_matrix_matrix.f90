! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_cma

  ! Description: single function specified in an invoke call performing
  ! a matrix-matrix calculation with CMA operations

  use columnwise_operator_mod,      only: columnwise_operator_type
  use columnwise_op_mul_kernel_mod, only: columnwise_op_mul_kernel_type

  implicit none

  type(columnwise_operator_type) :: cma_opa, cma_opb, cma_opc

  call invoke(                                                     &
          columnwise_op_mul_kernel_type(cma_opa, cma_opb, cma_opc) &
          )

end program single_invoke_cma
