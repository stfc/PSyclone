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
! Authors R. Ford and A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic Met Office

module testkern_wtheta_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Description: discontinuous field writer (wtheta) and reader (w3)
  type, extends(kernel_type) :: testkern_wtheta_type
     type(arg_type), dimension(2) :: meta_args =   &
          (/ arg_type(gh_field, gh_write, wtheta), &
             arg_type(gh_field, gh_read,  w3)      &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_wtheta_code
  end type testkern_wtheta_type

contains

  SUBROUTINE testkern_wtheta_code(nlayers,                             &
                                  field_1_wtheta,                      &
                                  field_2_w3,                          &
                                  ndf_wtheta, undf_wtheta, map_wtheta, &
                                  ndf_w3, undf_w3, map_w3)

    IMPLICIT NONE

    INTEGER, intent(in) :: nlayers
    INTEGER, intent(in) :: ndf_wtheta
    INTEGER, intent(in) :: undf_wtheta
    INTEGER, intent(in) :: ndf_w3
    INTEGER, intent(in) :: undf_w3
    REAL(KIND=r_def), intent(out), dimension(undf_wtheta) :: field_1_wtheta
    REAL(KIND=r_def), intent(in), dimension(undf_w3) :: field_2_w3
    INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta
    INTEGER, intent(in), dimension(ndf_w3) :: map_w3

  END SUBROUTINE testkern_wtheta_code

end module testkern_wtheta_mod
