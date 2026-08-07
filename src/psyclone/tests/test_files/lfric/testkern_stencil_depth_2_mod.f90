! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_stencil_depth_2_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_stencil_depth_2_type
     type(arg_type), dimension(4) :: meta_args =                       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w0),                 &
             arg_type(gh_field, gh_real, gh_read, w1, stencil(cross)), &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(cross)), &
             arg_type(gh_field, gh_real, gh_read, w3, stencil(cross))  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_stencil_depth_2_code
  end type testkern_stencil_depth_2_type

contains

  subroutine testkern_stencil_depth_2_code(nlayers, fld1,           &
                                           fld2, fld2_st_size,      &
                                           fld2_st_dofmap,          &
                                           fld3, fld3_st_size,      &
                                           fld3_st_dofmap,          &
                                           fld4, fld4_st_size,      &
                                           fld4_st_dofmap,          &
                                           ndf_w0, undf_w0, map_w0, &
                                           ndf_w1, undf_w1, map_w1, &
                                           ndf_w2, undf_w2, map_w2, &
                                           ndf_w3, undf_w3, map_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w0, undf_w1, undf_w2, undf_w3
    integer(kind=i_def), intent(in) :: fld2_st_size, fld3_st_size, fld4_st_size
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in), dimension(ndf_w1,fld2_st_size) :: fld2_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,fld3_st_size) :: fld3_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w3,fld4_st_size) :: fld4_st_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w0) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w1)    :: fld2
    real(kind=r_def), intent(in), dimension(undf_w2)    :: fld3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: fld4

  end subroutine testkern_stencil_depth_2_code

end module testkern_stencil_depth_2_mod
