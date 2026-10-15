! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_anyw2_stencil_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_anyw2_stencil_type
     type(arg_type), dimension(3) :: meta_args =                           &
          (/ arg_type(gh_field, gh_real, gh_inc,  any_w2),                 &
             arg_type(gh_field, gh_real, gh_read, any_w2, stencil(cross)), &
             arg_type(gh_field, gh_real, gh_read, any_w2, stencil(cross))  &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_anyw2_stencil_code
  end type testkern_anyw2_stencil_type

contains

  subroutine testkern_anyw2_stencil_code(nlayers, fld1,                 &
                                    fld2, fld2_st_size, fld2_st_dofmap, &
                                    fld3, fld3_st_size, fld3_st_dofmap, &
                                    ndf_any_w2, undf_any_w2, map_any_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in) :: fld2_st_size, fld3_st_size
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_any_w2,fld2_st_size) :: fld2_st_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_any_w2,fld3_st_size) :: fld3_st_dofmap
    real(kind=r_def), intent(inout), dimension(undf_any_w2) :: fld1
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: fld2
    real(kind=r_def), intent(in), dimension(undf_any_w2) :: fld3

  end subroutine testkern_anyw2_stencil_code

end module testkern_anyw2_stencil_mod
