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
! Author I. Kavcic, Met Office

module testkern_wtheta_only_vector_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Description: discontinuous field vector writer reader (wtheta)
  type, extends(kernel_type) :: testkern_wtheta_only_vector_type
     type(arg_type), dimension(2) :: meta_args =                   &
          (/  arg_type(gh_field*3, gh_real, gh_readwrite, wtheta), &
              arg_type(gh_field*3, gh_real, gh_read,      wtheta)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_wtheta_only_vector_code
  end type testkern_wtheta_only_vector_type

contains

  subroutine testkern_wtheta_only_vector_code(nlayers,     &
                                              field1_v1,   &
                                              field1_v2,   &
                                              field1_v3,   &
                                              field2_v1,   &
                                              field2_v2,   &
                                              field2_v3,   &
                                              ndf_wtheta,  &
                                              undf_wtheta, &
                                              map_wtheta)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: undf_wtheta
    integer(kind=i_def), intent(in), dimension(ndf_wtheta) :: map_wtheta
    real(kind=r_def), intent(inout), dimension(undf_wtheta) :: field1_v1
    real(kind=r_def), intent(inout), dimension(undf_wtheta) :: field1_v2
    real(kind=r_def), intent(inout), dimension(undf_wtheta) :: field1_v3
    real(kind=r_def), intent(in), dimension(undf_wtheta) :: field2_v1
    real(kind=r_def), intent(in), dimension(undf_wtheta) :: field2_v2
    real(kind=r_def), intent(in), dimension(undf_wtheta) :: field2_v3

  end subroutine testkern_wtheta_only_vector_code

end module testkern_wtheta_only_vector_mod
