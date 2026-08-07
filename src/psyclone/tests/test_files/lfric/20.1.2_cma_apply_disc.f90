! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invokes_cma_discontinuous

  ! Description: two single invokes containing multiple CMA-related kernels
  ! on discontinuous spaces ANY_DISCONTINUOUS_SPACE_1 and W2V

  use field_mod,               only: field_type
  use columnwise_operator_mod, only: columnwise_operator_type
  use columnwise_op_app_anydspace_kernel_mod, &
                               only: columnwise_op_app_anydspace_kernel_type
  use columnwise_op_app_w2v_kernel_mod,       &
                               only: columnwise_op_app_w2v_kernel_type

  implicit none

  type(field_type)               :: field_a, field_b
  type(field_type)               :: field_c, field_d
  type(columnwise_operator_type) :: cma_op1, cma_op2

  call invoke( &
         columnwise_op_app_anydspace_kernel_type(field_a, field_b, cma_op1) )
  call invoke( &
         columnwise_op_app_w2v_kernel_type(field_c, field_d, cma_op2) )

end program single_invokes_cma_discontinuous
