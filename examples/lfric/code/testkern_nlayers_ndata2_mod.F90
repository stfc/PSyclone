! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> An example LFRic kernel which has arguments with non-default values
!! of NLAYERS and NDATA.
module testkern_nlayers_ndata2_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_nlayers_ndata2_type
     type(arg_type), dimension(4) :: meta_args = (/                        &
             arg_type(gh_field,  gh_real, gh_inc,  w1),                    &
             ! Non-default number of layers.
             arg_type(gh_field,  gh_real, gh_read, w2, nlayers="shallow"), &
             ! Non-default number of data values per dof.
             arg_type(gh_field,  gh_real, gh_read, w2, ndata="precip"),    &
             ! Although we have seen the values of ndata and nlayers before
             ! (individually), the dof map will be different.
             arg_type(gh_field,  gh_real, gh_read, w2, ndata="precip",     &
                      nlayers="shallow")                                   &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_nlayers_ndata2_code
  end type testkern_nlayers_ndata2_type

contains

  subroutine testkern_nlayers_ndata2_code(          &
       nlayers, nlayers_shallow, ndata_precip,      &
       fld1, fld2, fld3, fld4,                      &
       ndf_w1, undf_w1, map_w1,                     &
       ndf_fld2, undf_fld2, map_w2_fld2,            &
       ndf_fld3, undf_fld3, map_w2_fld3,            &
       ndf_fld4, undf_fld4, map_w2_fld4)
    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nlayers_shallow
    integer(kind=i_def), intent(in) :: ndata_precip
    integer(kind=i_def), intent(in) :: ndf_w1, ndf_fld2, ndf_fld3, ndf_fld4
    integer(kind=i_def), intent(in) :: undf_w1, undf_fld2, undf_fld3, &
                                       undf_fld4
    integer(kind=i_def), intent(in), dimension(ndf_w1)   :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_fld2) :: map_w2_fld2
    integer(kind=i_def), intent(in), dimension(ndf_fld3) :: map_w2_fld3
    integer(kind=i_def), intent(in), dimension(ndf_fld4) :: map_w2_fld4
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    real(kind=r_def), intent(in), dimension(undf_fld2)  :: fld2
    real(kind=r_def), intent(in), dimension(undf_fld3)  :: fld3
    real(kind=r_def), intent(in), dimension(undf_fld4)  :: fld4

  end subroutine testkern_nlayers_ndata2_code

end module testkern_nlayers_ndata2_mod
