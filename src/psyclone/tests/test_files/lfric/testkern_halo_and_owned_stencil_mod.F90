! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Test kernel that operates on owned and halo cells and updates a field on
! a horizontally discontinuous function space while also having one field
! argument with a stencil access..

module testkern_halo_and_owned_stencil_mod

  use argument_mod, only: arg_type, gh_scalar, gh_field, gh_real, &
       gh_read, gh_write, stencil, region, owned_and_halo_cell_column
  use fs_continuity_mod
  use kernel_mod, only: kernel_type
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_halo_and_owned_stencil_type
     type(arg_type), dimension(5) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read),     &
             arg_type(gh_field,  gh_real, gh_write, w3),&
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w2, STENCIL(REGION)), &
             arg_type(gh_field,  gh_real, gh_read, w3)  &
           /)
     integer :: operates_on = owned_and_halo_cell_column
   contains
     procedure, nopass :: code => testkern_halo_and_owned_stencil_code
  end type testkern_halo_and_owned_stencil_type

contains

  subroutine testkern_halo_and_owned_stencil_code(nlayers, ascalar,        &
                                          fld1, fld2, fld3, stencil_len, &
                                          stencil_map, fld4,  &
                                          ndf_w3, undf_w3, map_w3, &
                                          ndf_w2, undf_w2, map_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w2, undf_w3
    integer(kind=i_def), intent(in) :: stencil_len
    integer(kind=i_def), intent(in) :: stencil_map(ndf_w2, 1:stencil_len)
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w3) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w2)  :: fld2
    real(kind=r_def), intent(in), dimension(undf_w2)  :: fld3
    real(kind=r_def), intent(in), dimension(undf_w3)  :: fld4

  end subroutine testkern_halo_and_owned_stencil_code

end module testkern_halo_and_owned_stencil_mod
