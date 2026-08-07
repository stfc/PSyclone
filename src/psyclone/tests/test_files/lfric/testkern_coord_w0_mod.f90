! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_coord_w0_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_coord_w0_type
     type(arg_type), dimension(3) :: meta_args =         &
          (/ arg_type(gh_field,   gh_real, gh_inc,  w0), &
             arg_type(gh_field*3, gh_real, gh_inc,  w0), &
             arg_type(gh_field,   gh_real, gh_read, w0)  &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_coord_w0_code
  end type testkern_coord_w0_type

contains

  subroutine testkern_coord_w0_code(nlayers, field1,      &
                                    field2_v1, field2_v2, &
                                    field2_v3, field3,    &
                                    ndf_w0, undf_w0, map_w0)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: undf_w0
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field1
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field2_v1
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field2_v2
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field2_v3
    real(kind=r_def), intent(in), dimension(undf_w0)    :: field3

  end subroutine testkern_coord_w0_code

end module testkern_coord_w0_mod
