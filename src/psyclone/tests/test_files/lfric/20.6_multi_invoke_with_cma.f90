! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program multi_invoke_cma

  ! Description: invoke containing multiple kernels, one of which is
  ! CMA related
  use constants_mod,                      only: r_def
  use field_mod,                          only: field_type
  use operator_mod,                       only: operator_type
  use columnwise_operator_mod,            only: columnwise_operator_type
  use columnwise_op_asm_field_kernel_mod, only: columnwise_op_asm_field_kernel_type
  use testkern_two_real_scalars_mod,      only: testkern_two_real_scalars_type
  
  implicit none

  type(field_type)               :: afield, bfield, cfield, dfield
  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1, cma_opb, cma_opc
  real(kind=r_def)               :: scalar1, scalar2

  call invoke(                                                        &
       columnwise_op_asm_field_kernel_type(afield, lma_op1, cma_op1), &
       testkern_two_real_scalars_type(scalar1, afield, bfield, cfield, dfield, scalar2) )

end program multi_invoke_cma
