! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------
module non_dof_anyspace_mod

  use argument_mod
  use kernel_mod

  !> field1 = ascalar
  type, public, extends(kernel_type) :: non_dof_anyspace_kern
     private
     type(arg_type) :: meta_args(2) = (/                              &
          arg_type(GH_FIELD,  GH_REAL, GH_INC, ANY_SPACE_1),          &
          arg_type(GH_SCALAR, GH_REAL, GH_READ            )           &
          /)
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, nopass :: non_dof_anyspace_kern_code
  end type non_dof_anyspace_kern

contains

  subroutine non_dof_anyspace_kern_code(f1_data, scalar, undf_aspc1)
      implicit none

      integer(kind=i_def), intent(in) :: undf_aspc1
      real(kind=r_def), intent(inout), dimension(undf_aspc1) :: f1_data
      real(kind=r_def), intent(in) :: scalar
  end subroutine non_dof_anyspace_kern_code

end module non_dof_anyspace_mod
