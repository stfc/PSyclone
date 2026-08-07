! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_dofs_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_dofs_type
     type(arg_type), dimension(6) :: meta_args =          &
          (/ arg_type(gh_field,   gh_real, gh_write, w1), &
             arg_type(gh_field,   gh_real, gh_read,  w1), &
             arg_type(gh_field,   gh_real, gh_read,  w1), &
             arg_type(gh_field,   gh_real, gh_read,  w1), &
             arg_type(gh_field*3, gh_real, gh_read,  w1), &
             arg_type(gh_scalar,  gh_real, gh_read)       &
           /)

     integer :: operates_on = DOF
   contains
     procedure, nopass :: code => testkern_dofs_code
  end type testkern_dofs_type

contains

  subroutine testkern_dofs_code(a, b, c, d,  &
                                field_vec_1, &
                                field_vec_2, &
                                field_vec_3, &
                                scalar_arg)
    implicit none

    real(kind=r_def), intent(inout) :: a
    real(kind=r_def), intent(in)    :: b, c, d
    real(kind=r_def), intent(in)    :: field_vec_1, field_vec_2, field_vec_3
    real(kind=r_def), intent(in)    :: scalar_arg

  end subroutine testkern_dofs_code

end module testkern_dofs_mod
