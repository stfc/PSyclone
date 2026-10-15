! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program access_tests

  ! Description: this program contains two invokes that test read and
  ! write declaration of CMA.
  use field_mod,                    only: field_type
  use operator_mod,                 only: operator_type
  use columnwise_operator_mod,      only: columnwise_operator_type
  use columnwise_op_asm_kernel_mod, only: columnwise_op_asm_kernel_type
  use columnwise_op_app_same_fs_kernel_mod, &
                                    only: columnwise_op_app_same_fs_kernel_type

  implicit none

  type(field_type)               :: f1, f2
  type(operator_type)            :: lma_op1
  type(columnwise_operator_type) :: cma_op1

  ! This kernel has a cma thas is read:
  call invoke( columnwise_op_app_same_fs_kernel_type(f1, f2, cma_op1), name="read")
  ! This kernel has a cma that is written:
  call invoke( columnwise_op_asm_kernel_type(lma_op1, cma_op1), name="write")

end program access_tests
