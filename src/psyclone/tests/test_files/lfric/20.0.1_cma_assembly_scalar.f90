! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke_cma

  ! Description: single CMA assembly kernel which includes a scalar
  ! argument
  use constants_mod,                       only: r_def
  use operator_mod,                        only: operator_type
  use columnwise_operator_mod,             only: columnwise_operator_type
  use columnwise_op_asm_kernel_scalar_mod, only: columnwise_op_asm_kernel_scalar_type

  implicit none

  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1
  real(kind=r_def)               :: my_scalar = 1.0_r_def

  call invoke( &
          ! Perversely name the scalar argument so as to deliberately clash
          ! with what we might expect to generate for the name of the 'alpha'
          ! parameter of the CMA argument.
          columnwise_op_asm_kernel_scalar_type(lma_op1, cma_op1, cma_op1_alpha) &
          )

end program single_invoke_cma
