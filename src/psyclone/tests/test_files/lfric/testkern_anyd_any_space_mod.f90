! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_anyd_any_space_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  ! Description: discontinuous field readwriter (any_discontinuous_space_1)
  ! and continuous readers (any_space_1 and any_w2)
  type, public, extends(kernel_type) :: testkern_anyd_any_space_type
     private
     type(arg_type), dimension(3) :: meta_args = (/                             &
          arg_type(gh_field, gh_real, gh_readwrite, any_discontinuous_space_1), &
          arg_type(gh_field, gh_real, gh_read,      any_space_1),               &
          arg_type(gh_field, gh_real, gh_read,      any_w2)                     &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, public, nopass :: code => testkern_anyd_any_space_code
  end type testkern_anyd_any_space_type

contains

  subroutine testkern_anyd_any_space_code(nlayers, field1, field2, field3,     &
                                          ndf_adspc1, undf_adspc1, map_adspc1, &
                                          ndf_aspc1, undf_aspc1, map_aspc1,    &
                                          ndf_any_w2, undf_any_w2, map_any_w2)


    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_adspc1, &
                                       undf_aspc1, undf_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_any_w2
    real(kind=r_def), intent(inout), dimension(undf_adspc1) :: field1
    real(kind=r_def), intent(in), dimension(undf_aspc1)     :: field2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: field3

  end subroutine testkern_anyd_any_space_code

end module testkern_anyd_any_space_mod
