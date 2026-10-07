! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program enforce_bc_kernel_example

  ! This boundary condition kernel has been created as a temporary measure
  ! as boundary layer information is not currently described
  ! in the API. Therefore, for the moment, users can add this kernel
  ! when they want to enforce boundary conditions
  use field_mod, only : field_type
  use enforce_bc_kernel_mod, only : enforce_bc_kernel_type
  type(field_type) :: a

  call invoke(enforce_bc_kernel_type(a))

end program enforce_bc_kernel_example
