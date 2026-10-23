! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_cma

  ! Description: single CMA-apply kernel specified in an invoke call

  use field_mod,                    only: field_type
  use columnwise_operator_mod,      only: columnwise_operator_type
  use columnwise_op_app_kernel_mod, only: columnwise_op_app_kernel_type

  implicit none

  type(field_type)               :: field_a, field_b
  type(columnwise_operator_type) :: cma_op1

  call invoke( columnwise_op_app_kernel_type(field_a, field_b, cma_op1) )

end program single_invoke_cma
