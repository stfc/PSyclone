! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program single_invoke

  ! Description: invoke containing multiple kernels, the first 
  ! of which is a kernel that operates on the domain instead of a
  ! cell-column and the last of which is a CMA kernel.
  use constants_mod,                only: r_def
  use field_mod,                    only: field_type
  use columnwise_operator_mod,      only: columnwise_operator_type
  use columnwise_op_asm_kernel_mod, only: columnwise_op_asm_kernel_type
  use testkern_domain_mod,          only: testkern_domain_type
  use testkern_mod,                 only: testkern_type

  implicit none

  real(kind=r_def) :: b, c
  type(field_type) :: f1, f2, f3, f4
  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1

  call invoke(                                           &
               ! Read-write f1 (W3)
               testkern_domain_type(b, f1),              &
               ! Read from f1
               testkern_type(b, f2, f3, f4, f1),         &
               ! Write cma_op1
               columnwise_op_asm_kernel_type(lma_op1, cma_op1) )

end program single_invoke
