! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Example of stencils over the same any_discontinuous_space
module testkern_same_any_dscnt_space_stencil_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_same_any_dscnt_space_stencil_type
    type(arg_type), dimension(3) :: meta_args = (/                        &
         arg_type(gh_field, gh_real, gh_write, wtheta),                   &
         arg_type(gh_field, gh_real, gh_read,  any_discontinuous_space_1, &
                                               stencil(cross)),           &
         arg_type(gh_field, gh_real, gh_read,  any_discontinuous_space_1, &
                                               stencil(cross))            &
         /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => testkern_same_any_dscnt_space_stencil_code
  end type testkern_same_any_dscnt_space_stencil_type

contains

  subroutine testkern_same_any_dscnt_space_stencil_code(                         &
                             nlayers,                                            &
                             field1,                                             &
                             field2, field2_stencil_size, field2_stencil_dofmap, &
                             field3, field3_stencil_size, field3_stencil_dofmap, &
                             ndf_wtheta, undf_wtheta, map_wtheta,                &
                             ndf_adspc1, undf_adspc1, map_adspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta, undf_adspc1
    integer(kind=i_def), intent(in) :: field2_stencil_size, field3_stencil_size
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_adspc1,field2_stencil_size) :: field2_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_adspc1,field3_stencil_size) :: field3_stencil_dofmap
    real(kind=r_def), intent(inout), dimension(undf_wtheta) :: field1
    real(kind=r_def), intent(in), dimension(undf_adspc1)    :: field2
    real(kind=r_def), intent(in), dimension(undf_adspc1)    :: field3

  end subroutine testkern_same_any_dscnt_space_stencil_code

end module testkern_same_any_dscnt_space_stencil_mod
