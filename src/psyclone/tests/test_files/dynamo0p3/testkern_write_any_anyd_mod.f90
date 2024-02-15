! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

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
