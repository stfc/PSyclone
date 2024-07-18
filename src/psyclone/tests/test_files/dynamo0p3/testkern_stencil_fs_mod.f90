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

module testkern_stencil_fs_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  ! Example of stencils on all supported function space identifiers
  type, extends(kernel_type) :: testkern_stencil_fs_type
     type(arg_type), dimension(16) :: meta_args = (/                         &
          arg_type(gh_field, gh_real, gh_inc,  w1),                          &
          arg_type(gh_field, gh_real, gh_read, w0,          stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w1,          stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2,          stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w3,          stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, wtheta,      stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2h,         stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2v,         stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2broken,    stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2trace,     stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2htrace,    stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, w2vtrace,    stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, wchi,        stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, any_w2,      stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, any_space_9, stencil(cross)), &
          arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_1,    &
                                               stencil(cross))               &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_stencil_fs_code
  end type testkern_stencil_fs_type
contains

  subroutine testkern_stencil_fs_code(nlayers, fld1,                             &
                                      fld2, fld2_stsize, fld2_stdofmap,          &
                                      fld3, fld3_stsize, fld3_stdofmap,          &
                                      fld4, fld4_stsize, fld4_stdofmap,          &
                                      fld5, fld5_stsize, fld5_stdofmap,          &
                                      fld6, fld6_stsize, fld6_stdofmap,          &
                                      fld7, fld7_stsize, fld7_stdofmap,          &
                                      fld8, fld8_stsize, fld8_stdofmap,          &
                                      fld9, fld9_stsize, fld9_stdofmap,          &
                                      fld10, fld10_stsize, fld10_stdofmap,       &
                                      fld11, fld11_stsize, fld11_stdofmap,       &
                                      fld12, fld12_stsize, fld12_stdofmap,       &
                                      fld13, fld13_stsize, fld13_stdofmap,       &
                                      fld14, fld14_stsize, fld14_stdofmap,       &
                                      fld15, fld15_stsize, fld15_stdofmap,       &
                                      fld16, fld16_stsize, fld16_stdofmap,       &
                                      ndf_w1, undf_w1, map_w1,                   &
                                      ndf_w0, undf_w0, map_w0,                   &
                                      ndf_w2, undf_w2, map_w2,                   &
                                      ndf_w3, undf_w3, map_w3,                   &
                                      ndf_wtheta, undf_wtheta, map_wtheta,       &
                                      ndf_w2h, undf_w2h, map_w2h,                &
                                      ndf_w2v, undf_w2v, map_w2v,                &
                                      ndf_w2broken, undf_w2broken, map_w2broken, &
                                      ndf_w2trace, undf_w2trace, map_w2trace,    &
                                      ndf_w2htrace, undf_w2htrace, map_w2htrace, &
                                      ndf_w2vtrace, undf_w2vtrace, map_w2vtrace, &
                                      ndf_wchi, undf_wchi, map_wchi,             &
                                      ndf_anyw2, undf_anyw2, map_anyw2,          &
                                      ndf_aspc9, undf_aspc9, map_aspc9,          &
                                      ndf_adspc1, undf_adspc1, map_adspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: ndf_aspc9
    integer(kind=i_def), intent(in) :: ndf_anyw2
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w2broken
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2htrace
    integer(kind=i_def), intent(in) :: ndf_w2trace
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_w2vtrace
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: ndf_wchi
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_w1, undf_w0, undf_w2,      &
                                       undf_w3, undf_wtheta, undf_w2h, &
                                       undf_w2v, undf_w2broken,        &
                                       undf_w2trace, undf_w2htrace,    &
                                       undf_w2vtrace, undf_wchi,       &
                                       undf_anyw2, undf_aspc9,         &
                                       undf_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_adspc1)   :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc9)    :: map_aspc9
    integer(kind=i_def), intent(in), dimension(ndf_anyw2)    :: map_anyw2
    integer(kind=i_def), intent(in), dimension(ndf_w0)       :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w1)       :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2)       :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2broken) :: map_w2broken
    integer(kind=i_def), intent(in), dimension(ndf_w2h)      :: map_w2h
    integer(kind=i_def), intent(in), dimension(ndf_w2htrace) :: map_w2htrace
    integer(kind=i_def), intent(in), dimension(ndf_w2trace)  :: map_w2trace
    integer(kind=i_def), intent(in), dimension(ndf_w2v)      :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_w2vtrace) :: map_w2vtrace
    integer(kind=i_def), intent(in), dimension(ndf_w3)       :: map_w3
    integer(kind=i_def), intent(in), dimension(ndf_wchi)     :: map_wchi
    integer(kind=i_def), intent(in), dimension(ndf_wtheta)   :: map_wtheta
    integer(kind=i_def), intent(in) :: fld2_stsize, fld3_stsize, fld4_stsize,    &
                                       fld5_stsize, fld6_stsize, fld7_stsize,    &
                                       fld8_stsize, fld9_stsize, fld10_stsize,   &
                                       fld11_stsize, fld12_stsize, fld13_stsize, &
                                       fld14_stsize, fld15_stsize, fld16_stsize
    integer(kind=i_def), intent(in), dimension(ndf_w0,fld2_stsize)        :: fld2_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w1,fld3_stsize)        :: fld3_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,fld4_stsize)        :: fld4_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w3,fld5_stsize)        :: fld5_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_wtheta,fld6_stsize)    :: fld6_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2h,fld7_stsize)       :: fld7_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2v,fld8_stsize)       :: fld8_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2broken,fld9_stsize)  :: fld9_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2trace,fld10_stsize)  :: fld10_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2htrace,fld11_stsize) :: fld11_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2vtrace,fld12_stsize) :: fld12_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_wchi,fld13_stsize)     :: fld13_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_anyw2,fld14_stsize)    :: fld14_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_aspc9,fld15_stsize)    :: fld15_stdofmap
    integer(kind=i_def), intent(in), dimension(ndf_adspc1,fld16_stsize)   :: fld16_stdofmap
    real(kind=r_def), intent(inout), dimension(undf_w1)       :: fld1
    real(kind=r_def), intent(in),    dimension(undf_w0)       :: fld2
    real(kind=r_def), intent(in),    dimension(undf_w1)       :: fld3
    real(kind=r_def), intent(in),    dimension(undf_w2)       :: fld4
    real(kind=r_def), intent(in),    dimension(undf_w3)       :: fld5
    real(kind=r_def), intent(in),    dimension(undf_wtheta)   :: fld6
    real(kind=r_def), intent(in),    dimension(undf_w2h)      :: fld7
    real(kind=r_def), intent(in),    dimension(undf_w2v)      :: fld8
    real(kind=r_def), intent(in),    dimension(undf_w2broken) :: fld9
    real(kind=r_def), intent(in),    dimension(undf_w2trace)  :: fld10
    real(kind=r_def), intent(in),    dimension(undf_w2htrace) :: fld11
    real(kind=r_def), intent(in),    dimension(undf_w2vtrace) :: fld12
    real(kind=r_def), intent(in),    dimension(undf_wchi)     :: fld13
    real(kind=r_def), intent(in),    dimension(undf_anyw2)    :: fld14
    real(kind=r_def), intent(in),    dimension(undf_aspc9)    :: fld15
    real(kind=r_def), intent(in),    dimension(undf_adspc1)   :: fld16

  end subroutine testkern_stencil_fs_code

end module testkern_stencil_fs_mod
