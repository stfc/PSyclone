! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018, Science and Technology Facilities Council
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
! Author I. Kavcic Met Office

module testkern_w2v_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Description: discontinuous field writer (w2v) and reader (wtheta)
  type, extends(kernel_type) :: testkern_w2v_type
     type(arg_type), dimension(2) :: meta_args =  &
          (/ arg_type(gh_field, gh_write, w2v),   &
             arg_type(gh_field, gh_read,  wtheta) &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_w2v_code
  end type testkern_w2v_type

contains

  SUBROUTINE testkern_w2v_code(nlayers,                    &
                               field_1_w2v,                &
                               field_2_wtheta,             &
                               ndf_w2v, undf_w2v, map_w2v, &
                               ndf_wtheta, undf_wtheta, map_wtheta)

    IMPLICIT NONE

    INTEGER, intent(in) :: nlayers
    INTEGER, intent(in) :: ndf_w2v
    INTEGER, intent(in) :: undf_w2v
    INTEGER, intent(in) :: ndf_wtheta
    INTEGER, intent(in) :: undf_wtheta
    REAL(KIND=r_def), intent(out), dimension(undf_w2v) :: field_1_w2v
    REAL(KIND=r_def), intent(in), dimension(undf_wtheta) :: field_2_wtheta
    INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v
    INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta

  END SUBROUTINE testkern_w2v_code

end module testkern_w2v_mod
