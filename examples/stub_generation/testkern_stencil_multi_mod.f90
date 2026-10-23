! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_stencil_multi_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_stencil_multi_type
     type(arg_type), dimension(4) :: meta_args =                        &
          (/ arg_type(gh_field, gh_real, gh_inc,  w1),                  &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(cross)),  &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(xory1d)), &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(x1d))     &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_stencil_multi_code
  end type testkern_stencil_multi_type

contains

  subroutine testkern_stencil_multi_code()
  end subroutine testkern_stencil_multi_code

end module testkern_stencil_multi_mod
