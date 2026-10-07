! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_stencil_vector_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_stencil_vector_type
     type(arg_type), dimension(2) :: meta_args =                        &
          (/ arg_type(gh_field*3, gh_real, gh_inc,  w0),                &
             arg_type(gh_field*4, gh_real, gh_read, w3, stencil(cross)) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_stencil_vector_code
  end type testkern_stencil_vector_type

contains

  subroutine testkern_stencil_vector_code(                &
                      nlayers, fld1_v1, fld1_v2, fld1_v3, &
                      fld2_v1, fld2_v2, fld2_v3, fld2_v4, &
                      fld2_st_size, fld2_st_dofmap,       &
                      ndf_w0, undf_w0, map_w0,            &
                      ndf_w3, undf_w3, map_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w0, undf_w3
    integer(kind=i_def), intent(in) :: fld2_st_size
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3,fld2_st_size) :: fld2_st_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w0) :: fld1_v1
    real(kind=r_def), intent(inout), dimension(undf_w0) :: fld1_v2
    real(kind=r_def), intent(inout), dimension(undf_w0) :: fld1_v3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: fld2_v1
    real(kind=r_def), intent(in), dimension(undf_w3)    :: fld2_v2
    real(kind=r_def), intent(in), dimension(undf_w3)    :: fld2_v3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: fld2_v4

  end subroutine testkern_stencil_vector_code

end module testkern_stencil_vector_mod
