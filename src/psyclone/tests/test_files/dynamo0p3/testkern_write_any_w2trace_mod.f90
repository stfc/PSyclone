! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018-2024, Science and Technology Facilities Council
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

! A kernel which writes to two fields, one on any space and one on W2trace
! (both continuous). The generated loop bounds in the PSy layer must therefore
! be for the 'worst case' which is the continuous space.
module testkern_write_any_w2trace_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_write_any_w2trace_type
     type(arg_type), dimension(8) :: meta_args = (/          &
          arg_type(gh_field, gh_real, gh_inc,  any_space_1), &
          arg_type(gh_field, gh_real, gh_read, w0),          &
          arg_type(gh_field, gh_real, gh_read, w1),          &
          arg_type(gh_field, gh_real, gh_inc,  w2trace),     &
          arg_type(gh_field, gh_real, gh_read, wtheta),      &
          arg_type(gh_field, gh_real, gh_read, w2h),         &
          arg_type(gh_field, gh_real, gh_read, w2v),         &
          arg_type(gh_field, gh_real, gh_read, w2htrace)     &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_write_any_w2trace_code
  end type testkern_write_any_w2trace_type

contains

  subroutine testkern_write_any_w2trace_code(                           &
                                nlayers, fld1, fld2, fld3,              &
                                fld4, fld5, fld6, fld7, fld8,           &
                                ndf_aspc1, undf_aspc1, map_aspc1,       &
                                ndf_w0, undf_w0, map_w0,                &
                                ndf_w1, undf_w1, map_w1,                &
                                ndf_w2trace, undf_w2trace, map_w2trace, &
                                ndf_wtheta, undf_wtheta, map_wtheta,    &
                                ndf_w2h, undf_w2h, map_w2h,             &
                                ndf_w2v, undf_w2v, map_w2v,             &
                                ndf_w2htrace, undf_w2htrace, map_w2htrace)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2htrace
    integer(kind=i_def), intent(in) :: ndf_w2trace
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_aspc1, undf_w0, undf_w1, &
                                       undf_w2trace, undf_wtheta,    &
                                       undf_w2h, undf_w2v, undf_w2htrace
    integer(kind=i_def), intent(in), dimension(ndf_aspc1)    :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_w0)       :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w1)       :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2h)      :: map_w2h
    integer(kind=i_def), intent(in), dimension(ndf_w2htrace) :: map_w2htrace
    integer(kind=i_def), intent(in), dimension(ndf_w2trace)  :: map_w2trace
    integer(kind=i_def), intent(in), dimension(ndf_w2v)      :: map_w2v
    integer(kind=i_def), intent(in), dimension(ndf_wtheta)   :: map_wtheta
    real(kind=r_def), intent(inout), dimension(undf_aspc1)    :: fld1
    real(kind=r_def), intent(in),    dimension(undf_w0)       :: fld2
    real(kind=r_def), intent(in),    dimension(undf_w1)       :: fld3
    real(kind=r_def), intent(inout), dimension(undf_w2trace)  :: fld4
    real(kind=r_def), intent(in),    dimension(undf_wtheta)   :: fld5
    real(kind=r_def), intent(in),    dimension(undf_w2h)      :: fld6
    real(kind=r_def), intent(in),    dimension(undf_w2v)      :: fld7
    REAL(KIND=r_def), intent(in),    dimension(undf_w2htrace) :: fld8

  end subroutine testkern_write_any_w2trace_code

end module testkern_write_any_w2trace_mod
