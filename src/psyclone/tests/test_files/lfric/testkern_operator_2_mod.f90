! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_operator_2_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_operator_2_type
     type(arg_type), dimension(1) :: meta_args = &
          (/ arg_type(gh_operator, gh_real, gh_write, w2, w3) /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_operator_2_code
  end type testkern_operator_2_type

contains

  subroutine testkern_operator_2_code(cell, nlayers, ncell_3d, &
                                      local_stencil, ndf_w2, ndf_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    real(kind=r_def), intent(inout), dimension(ncell_3d,ndf_w2,ndf_w3) :: local_stencil

  end subroutine testkern_operator_2_code

end module testkern_operator_2_mod
