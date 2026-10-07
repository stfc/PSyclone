! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_multi_field_same_stencil_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_multi_field_same_stencil_type
     type(arg_type), dimension(5) :: meta_args =                        &
          (/ arg_type(gh_field, gh_real, gh_inc,  w1),                  &
             arg_type(gh_field, gh_real, gh_read, w1, stencil(cross)),  &
             arg_type(gh_field, gh_real, gh_read, w1, stencil(cross)),  &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(xory1d)), &
             arg_type(gh_field, gh_real, gh_read, w2, stencil(xory1d))  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_multi_field_same_stencil_code
  end type testkern_multi_field_same_stencil_type

contains

  subroutine testkern_multi_field_same_stencil_code(nlayers, fld1,                  &
                                                    fld2, fld2_st_size,             &
                                                    fld2_st_dofmap,                 &
                                                    fld3, fld3_st_size,             &
                                                    fld3_st_dofmap,                 &
                                                    fld4, fld4_st_size,             &
                                                    fld4_direction, fld4_st_dofmap, &
                                                    fld5, fld5_st_size,             &
                                                    fld5_direction, fld5_st_dofmap, &
                                                    ndf_w1, undf_w1, map_w1,        &
                                                    ndf_w2, undf_w2, map_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2
    integer(kind=i_def), intent(in) :: fld2_st_size, fld3_st_size, &
                                       fld4_st_size, fld5_st_size
    integer(kind=i_def), intent(in) :: fld4_direction, fld5_direction
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w1,fld2_st_size) :: fld2_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w1,fld3_st_size) :: fld3_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,fld4_st_size) :: fld4_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,fld5_st_size) :: fld5_st_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w1)    :: fld2
    real(kind=r_def), intent(in), dimension(undf_w1)    :: fld3
    real(kind=r_def), intent(in), dimension(undf_w2)    :: fld4
    real(kind=r_def), intent(in), dimension(undf_w2)    :: fld5

  end subroutine testkern_multi_field_same_stencil_code

end module testkern_multi_field_same_stencil_mod
