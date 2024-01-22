! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council
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
! Author I. Kavcic, Met Office

module testkern_fs_int_field_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Integer-valued fields on all function spaces with two continuous
  ! (w2 and any_space_1) and two discontinuous (w2broken and
  ! any_discontinuous_space_1) field writers
  type, public, extends(kernel_type) :: testkern_fs_int_field_type
     private
     type(arg_type), dimension(15) :: meta_args = (/                 &
          arg_type(gh_field, gh_integer, gh_read,      w1),          &
          arg_type(gh_field, gh_integer, gh_inc,       w2),          &
          arg_type(gh_field, gh_integer, gh_read,      w0),          &
          arg_type(gh_field, gh_integer, gh_read,      w3),          &
          arg_type(gh_field, gh_integer, gh_write,     wtheta),      &
          arg_type(gh_field, gh_integer, gh_read,      w2h),         &
          arg_type(gh_field, gh_integer, gh_read,      w2v),         &
          arg_type(gh_field, gh_integer, gh_read,      w2broken),    &
          arg_type(gh_field, gh_integer, gh_read,      w2trace),     &
          arg_type(gh_field, gh_integer, gh_read,      w2htrace),    &
          arg_type(gh_field, gh_integer, gh_read,      w2vtrace),    &
          arg_type(gh_field, gh_integer, gh_read,      wchi),        &
          arg_type(gh_field, gh_integer, gh_read,      any_w2),      &
          arg_type(gh_field, gh_integer, gh_inc,       any_space_1), &
          arg_type(gh_field, gh_integer, gh_readwrite,               &
                                         any_discontinuous_space_1)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, public, nopass :: code => testkern_fs_int_field_code
  end type testkern_fs_int_field_type

contains

  subroutine testkern_fs_int_field_code(nlayers, field1, field2, field3,           &
                                        field4, field5, field6, field7,            &
                                        field8, field9, field10, field11,          &
                                        field12, field13, field14, field15,        &
                                        ndf_w1, undf_w1, map_w1,                   &
                                        ndf_w2, undf_w2, map_w2,                   &
                                        ndf_w0, undf_w0, map_w0,                   &
                                        ndf_w3, undf_w3, map_w3,                   &
                                        ndf_wtheta, undf_wtheta, map_wtheta,       &
                                        ndf_w2h, undf_w2h, map_w2h,                &
                                        ndf_w2v, undf_w2v, map_w2v,                &
                                        ndf_w2broken, undf_w2broken, map_w2broken, &
                                        ndf_w2trace, undf_w2trace, map_w2trace,    &
                                        ndf_w2htrace, undf_w2htrace, map_w2htrace, &
                                        ndf_w2vtrace, undf_w2vtrace, map_w2vtrace, &
                                        ndf_wchi, undf_wchi, map_wchi,             &
                                        ndf_any_w2, undf_any_w2, map_any_w2,       &
                                        ndf_aspc1, undf_aspc1, map_aspc1,          &
                                        ndf_adspc1, undf_adspc1, map_adspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_w2broken
    integer(kind=i_def), intent(in) :: ndf_w2trace
    integer(kind=i_def), intent(in) :: ndf_w2htrace
    integer(kind=i_def), intent(in) :: ndf_w2vtrace
    integer(kind=i_def), intent(in) :: ndf_wchi
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w0,    &
                                       undf_w3, undf_wtheta,         &
                                       undf_w2h, undf_w2v,           &
                                       undf_w2broken, undf_w2trace,  &
                                       undf_w2htrace, undf_w2vtrace, &
                                       undf_wchi, undf_any_w2,       &
                                       undf_aspc1, undf_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_w1)       :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2)       :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w0)       :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w3)       :: map_w3
    integer(kind=i_def), intent(in), dimension(ndf_wtheta)   :: map_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_w2h)      :: map_w2h
    integer(kind=i_def), intent(in), dimension(ndf_w2v)      :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_w2broken) :: map_w2broken
    integer(kind=i_def), intent(in), dimension(ndf_w2trace)  :: map_w2trace
    integer(kind=i_def), intent(in), dimension(ndf_w2htrace) :: map_w2htrace
    integer(kind=i_def), intent(in), dimension(ndf_w2vtrace) :: map_w2vtrace
    integer(kind=i_def), intent(in), dimension(ndf_wchi)     :: map_wchi
    integer(kind=i_def), intent(in), dimension(ndf_any_w2)   :: map_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)    :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_adspc1)   :: map_adspc1
    integer(kind=i_def), intent(in),    dimension(undf_w1)       :: field1
    integer(kind=i_def), intent(inout), dimension(undf_w2)       :: field2
    integer(kind=i_def), intent(in),    dimension(undf_w0)       :: field3
    integer(kind=i_def), intent(in),    dimension(undf_w3)       :: field4
    integer(kind=i_def), intent(inout), dimension(undf_wtheta)   :: field5
    integer(kind=i_def), intent(in),    dimension(undf_w2h)      :: field6
    integer(kind=i_def), intent(in),    dimension(undf_w2v)      :: field7
    integer(kind=i_def), intent(in),    dimension(undf_w2broken) :: field8
    integer(kind=i_def), intent(in),    dimension(undf_w2trace)  :: field9
    integer(kind=i_def), intent(in),    dimension(undf_w2htrace) :: field10
    integer(kind=i_def), intent(in),    dimension(undf_w2vtrace) :: field11
    integer(kind=i_def), intent(in),    dimension(undf_wchi)     :: field12
    integer(kind=i_def), intent(in),    dimension(undf_any_w2)   :: field13
    integer(kind=i_def), intent(inout), dimension(undf_aspc1)    :: field14
    integer(kind=i_def), intent(inout), dimension(undf_adspc1)   :: field15

  end subroutine testkern_fs_int_field_code

end module testkern_fs_int_field_mod
