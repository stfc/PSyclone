! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program enforce_operator_bc_kernel_example

  ! Users can add this kernel when they want to enforce boundary
  ! conditions to operators

  use enforce_operator_bc_kernel_mod, only : enforce_operator_bc_kernel_type
  use operator_mod,                   only : operator_type

  type(operator_type) :: op_a
  
  call invoke(enforce_operator_bc_kernel_type(op_a))

end program enforce_operator_bc_kernel_example
