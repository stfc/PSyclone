! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_coord_w0_2_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none
  
  type, extends(kernel_type) :: testkern_coord_w0_2_type
     type(arg_type), dimension(2) :: meta_args =        &
          (/ arg_type(gh_field*3, gh_real, gh_inc, w0), &
             arg_type(gh_field,   gh_real, gh_inc, w0)  &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_coord_w0_2_code
  end type testkern_coord_w0_2_type

contains

  subroutine testkern_coord_w0_2_code(nlayers, field1_v1, &
                                      field1_v2, field1_v3, &
                                      field2, ndf_w0, undf_w0, map_w0)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: undf_w0
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field1_v1
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field1_v2
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field1_v3
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field2

  end subroutine testkern_coord_w0_2_code

end module testkern_coord_w0_2_mod
