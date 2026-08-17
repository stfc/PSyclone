! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_w2_only_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_w2_only_type
     type(arg_type), dimension(2) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w2), &
             arg_type(gh_field, gh_real, gh_read, w2)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_w2_only_code
  end type testkern_w2_only_type

contains

  subroutine testkern_w2_only_code(nlayers, fld1, fld2, &
                                   ndf_w2, undf_w2, map_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    real(kind=r_def), intent(inout), dimension(undf_w2) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w2)    :: fld2

  end subroutine testkern_w2_only_code

end module testkern_w2_only_mod
