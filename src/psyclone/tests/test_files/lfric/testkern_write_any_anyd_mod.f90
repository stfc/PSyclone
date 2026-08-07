! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! A kernel which writes to two fields, one on any_space and one on
! any_discontinuous_space_1. The generated loop bounds in the PSy layer
! must therefore be for the 'worst case' which is the continuous space
! (because we have to allow for the any_space space being continuous).
module testkern_write_any_anyd_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Description: function spaces with one continuous (any_space_1)
  ! and one discontinuous (any_discontinuous_space_1) field writer
  type, extends(kernel_type) :: testkern_write_any_anyd_type
     type(arg_type), dimension(7) :: meta_args = (/                         &
          arg_type(gh_field, gh_real, gh_inc,   any_space_1),               &
          arg_type(gh_field, gh_real, gh_read,  w2),                        &
          arg_type(gh_field, gh_real, gh_read,  w1),                        &
          arg_type(gh_field, gh_real, gh_write, any_discontinuous_space_1), &
          arg_type(gh_field, gh_real, gh_read,  wtheta),                    &
          arg_type(gh_field, gh_real, gh_read,  w2h),                       &
          arg_type(gh_field, gh_real, gh_read,  w2v)                        &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_write_any_anyd_code
  end type testkern_write_any_anyd_type

contains

  subroutine testkern_write_any_anyd_code(nlayers,                             &
                                          field1, field2,                      &
                                          field3, field4,                      &
                                          field5, field6, field7,              &
                                          ndf_aspc1, undf_aspc1, map_aspc1,    &
                                          ndf_w2, undf_w2, map_w2,             &
                                          ndf_w1, undf_w1, map_w1,             &
                                          ndf_adspc1, undf_adspc1, map_adspc1, &
                                          ndf_wtheta, undf_wtheta, map_wtheta, &
                                          ndf_w2h, undf_w2h, map_w2h,          &
                                          ndf_w2v, undf_w2v, map_w2v)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_aspc1, undf_w2,  &
                                       undf_w1, undf_adspc1, &
                                       undf_wtheta, undf_w2h, undf_w2v
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)  :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_w1)     :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2)     :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2h)    :: map_w2h
    integer(kind=i_def), intent(in), dimension(ndf_w2v)    :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    real(kind=r_def), intent(inout), dimension(undf_aspc1)  :: field1
    real(kind=r_def), intent(in), dimension(undf_w2)        :: field2
    real(kind=r_def), intent(in), dimension(undf_w1)        :: field3
    real(kind=r_def), intent(inout), dimension(undf_adspc1) :: field4
    real(kind=r_def), intent(in), dimension(undf_wtheta)    :: field5
    real(kind=r_def), intent(in), dimension(undf_w2h)       :: field6
    real(kind=r_def), intent(in), dimension(undf_w2v)       :: field7

  end subroutine testkern_write_any_anyd_code

end module testkern_write_any_anyd_mod
