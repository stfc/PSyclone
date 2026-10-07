! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module setop_random_kernel_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: setop_random_kernel_type
     type(arg_type), dimension(1) :: meta_args =                  &
          (/ arg_type(gh_operator, gh_real, gh_write, any_space_1, any_space_2) &
             !arg_type(gh_field*3,  gh_real,    gh_read,  w0),     &
             !arg_type(gh_scalar,   gh_integer, gh_read)           &
          /)
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, nopass :: code => setop_random_kernel_code
  end type setop_random_kernel_type

contains

  subroutine setop_random_kernel_code(cell, nlayers, ncell_3d, &
                                      local_stencil, ndf_aspc1, ndf_aspc2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: ndf_aspc1, ndf_aspc2
    real(kind=r_def), intent(inout), dimension(ncell_3d,ndf_aspc1,ndf_aspc2) :: local_stencil
    ! local variables
    integer(kind=i_def) :: k, ik

    do k = 0, nlayers-1
      ik = (cell-1)*nlayers + k + 1
      call random_number(local_stencil(:,:,ik))
    end do

  end subroutine setop_random_kernel_code

end module setop_random_kernel_mod
