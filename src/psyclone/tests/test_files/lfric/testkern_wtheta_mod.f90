! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_wtheta_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Description: discontinuous field writer (wtheta) and reader
  ! (any_discontinuous_space_1)
  type, extends(kernel_type) :: testkern_wtheta_type
     type(arg_type), dimension(2) :: meta_args = (/                        &
          arg_type(gh_field, gh_real, gh_write, wtheta),                   &
          arg_type(gh_field, gh_real, gh_read,  any_discontinuous_space_1) &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_wtheta_code
  end type testkern_wtheta_type

contains

  subroutine testkern_wtheta_code(nlayers, field1, field2,             &
                                  ndf_wtheta, undf_wtheta, map_wtheta, &
                                  ndf_adspc1, undf_adspc1, map_adspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: undf_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    real(kind=r_def), intent(inout), dimension(undf_wtheta) :: field1
    real(kind=r_def), intent(in), dimension(undf_adspc1)    :: field2

  end subroutine testkern_wtheta_code

end module testkern_wtheta_mod
