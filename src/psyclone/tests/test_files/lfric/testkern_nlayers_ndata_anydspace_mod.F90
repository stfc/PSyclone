! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> An example LFRic kernel which has arguments with non-default values
!! of NLEVELS and NDATA on ANY_DISCONTINUOUS_SPACE.
module testkern_nlayers_ndata_anydspace_mod

  use argument_mod, only: arg_type, gh_real, gh_scalar, gh_field, &
       gh_read, gh_inc, cell_column, &
       ANY_DISCONTINUOUS_SPACE_1, ANY_DISCONTINUOUS_SPACE_2
  use fs_continuity_mod, only: w1
  use kernel_mod, only: kernel_type
  use constants_mod, only: r_def, i_def

  implicit none

  type, extends(kernel_type) :: testkern_nlayers_ndata_anydspace_type
     type(arg_type), dimension(4) :: meta_args =                           &
          (/ arg_type(gh_scalar, gh_real, gh_read),                        &
             arg_type(gh_field,  gh_real, gh_inc,  w1),                    &
             ! Non-default number of levels.
             arg_type(gh_field,  gh_real, gh_read, ANY_DISCONTINUOUS_SPACE_1, nlayers="shallow"), &
             ! Non-default number of levels but same as previous arg. so
             ! has same dof map.
             arg_type(gh_field,  gh_real, gh_read, ANY_DISCONTINUOUS_SPACE_2, ndata="precip")  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_nlayers_ndata_anydspace_code
  end type testkern_nlayers_ndata_anydspace_type

contains

  subroutine testkern_nlayers_ndata_anydspace_code( &
       nlayers, nlayers_fld2, ndata_fld3,           &
       ascalar, fld1, fld2, fld3,                   &
       ndf_w1, undf_w1, map_w1,                     &
       ndf_fld2, undf_fld2, map_fld2,               &
       ndf_fld3, undf_fld3, map_fld3)
    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nlayers_fld2
    integer(kind=i_def), intent(in) :: ndata_fld3
    integer(kind=i_def), intent(in) :: ndf_w1, ndf_fld2, ndf_fld3
    integer(kind=i_def), intent(in) :: undf_w1, undf_fld2, undf_fld3
    integer(kind=i_def), intent(in), dimension(ndf_w1)   :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_fld2) :: map_fld2
    integer(kind=i_def), intent(in), dimension(ndf_fld3) :: map_fld3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    real(kind=r_def), intent(in), dimension(undf_fld2)  :: fld2
    real(kind=r_def), intent(in), dimension(undf_fld3)  :: fld3

  end subroutine testkern_nlayers_ndata_anydspace_code

end module testkern_nlayers_ndata_anydspace_mod
