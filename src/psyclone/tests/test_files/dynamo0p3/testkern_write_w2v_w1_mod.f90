! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2018, Science and Technology Facilities Council
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
! Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic Met Office

! A kernel which writes to two fields, one on W2V (discontinuous) and one on
! W1 (continuous). The generated loop bounds in the PSy layer must therefore
! be for the 'worst case' which is the continuous space.
module testkern_write_w2v_w1_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_write_w2v_w1_type
     type(arg_type), dimension(7) :: meta_args =   &
          (/ arg_type(gh_field, gh_read,  w3),     &
             arg_type(gh_field, gh_read,  w0),     &
             arg_type(gh_field, gh_read,  w2),     &
             arg_type(gh_field, gh_write, w1),     &
             arg_type(gh_field, gh_read,  wtheta), &
             arg_type(gh_field, gh_read,  w2h),    &
             arg_type(gh_field, gh_write, w2v)     &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_write_w2v_w1_code
  end type testkern_write_w2v_w1_type

contains

  SUBROUTINE testkern_write_w2v_w1_code(nlayers,                             &
                                        field_1_w3, field_2_w0,              &
                                        field_3_w2, field_4_w1,              &
                                        field_5_wtheta, field_6_w2h,         &
                                        field_7_w2v,                         &
                                        ndf_w3, undf_w3, map_w3,             &
                                        ndf_w0, undf_w0, map_w0,             &
                                        ndf_w2, undf_w2, map_w2,             &
                                        ndf_w1, undf_w1, map_w1,             &
                                        ndf_wtheta, undf_wtheta, map_wtheta, &
                                        ndf_w2h, undf_w2h, map_w2h,          &
                                        ndf_w2v, undf_w2v, map_w2v)

    IMPLICIT NONE

    INTEGER, intent(in) :: nlayers
    INTEGER, intent(in) :: ndf_w3
    INTEGER, intent(in) :: undf_w3
    INTEGER, intent(in) :: ndf_w0
    INTEGER, intent(in) :: undf_w0
    INTEGER, intent(in) :: ndf_w2
    INTEGER, intent(in) :: undf_w2
    INTEGER, intent(in) :: ndf_w1
    INTEGER, intent(in) :: undf_w1
    INTEGER, intent(in) :: ndf_wtheta
    INTEGER, intent(in) :: undf_wtheta
    INTEGER, intent(in) :: ndf_w2h
    INTEGER, intent(in) :: undf_w2h
    INTEGER, intent(in) :: ndf_w2v
    INTEGER, intent(in) :: undf_w2v
    REAL(KIND=r_def), intent(in), dimension(undf_w3) :: field_1_w3
    REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_2_w0
    REAL(KIND=r_def), intent(in), dimension(undf_w2) :: field_3_w2
    REAL(KIND=r_def), intent(out), dimension(undf_w1) :: field_4_w1
    REAL(KIND=r_def), intent(in), dimension(undf_wtheta) :: field_5_wtheta
    REAL(KIND=r_def), intent(in), dimension(undf_w2h) :: field_6_w2h
    REAL(KIND=r_def), intent(out), dimension(undf_w2v) :: field_7_w2v
    INTEGER, intent(in), dimension(ndf_w3) :: map_w3
    INTEGER, intent(in), dimension(ndf_w0) :: map_w0
    INTEGER, intent(in), dimension(ndf_w2) :: map_w2
    INTEGER, intent(in), dimension(ndf_w1) :: map_w1
    INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta
    INTEGER, intent(in), dimension(ndf_w2h) :: map_w2h
    INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v

  END SUBROUTINE testkern_write_w2v_w1_code

end module testkern_write_w2v_w1_mod
