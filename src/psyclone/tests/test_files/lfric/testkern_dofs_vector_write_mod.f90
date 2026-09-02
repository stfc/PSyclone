! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Test kernel that operates on DoFs and updates a field vector.

module testkern_dofs_vector_write_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_dofs_vector_write_type
     type(arg_type), dimension(1) :: meta_args =          &
          (/ arg_type(gh_field*3, gh_real, gh_write,  w1) /)

     integer :: operates_on = DOF
   contains
     procedure, nopass :: code => testkern_dofs_vector_write_code
  end type testkern_dofs_vector_write_type

contains

  subroutine testkern_dofs_vector_write_code(&
                                field_vec_1, &
                                field_vec_2, &
                                field_vec_3)
    implicit none

    real(kind=r_def), intent(in)    :: field_vec_1, field_vec_2, field_vec_3

  end subroutine testkern_dofs_vector_write_code

end module testkern_dofs_vector_write_mod
