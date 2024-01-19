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
!-------------------------------------------------------------------------------
! Author R. W. Ford STFC Daresbury Lab
!        C. M. Maynard Met Office/University of Reading
! Modified I. Kavcic Met Office

module testkern_two_real_scalars_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_two_real_scalars_type
     type(arg_type), dimension(6) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read    ), &
             arg_type(gh_field,  gh_real, gh_inc,  w1), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w3), &
             arg_type(gh_scalar, gh_real, gh_read    )  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_two_real_scalars_code
  end type testkern_two_real_scalars_type

contains

  subroutine testkern_two_real_scalars_code(nlayers, a, f1data,        &
                                            f2data, f3data, f4data, b, &
                                            ndf_w1, undf_w1, map_w1,   &
                                            ndf_w2, undf_w2, map_w2,   &
                                            ndf_w3, undf_w3, map_w3 )

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1, ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    integer(kind=i_def), dimension(ndf_w1), intent(in)    :: map_w1
    integer(kind=i_def), dimension(ndf_w2), intent(in)    :: map_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in)    :: map_w3
    real(kind=r_def), intent(in)    :: a, b
    real(kind=r_def), dimension(undf_w1),   intent(inout) :: f1data
    real(kind=r_def), dimension(undf_w2),   intent(in)    :: f2data
    real(kind=r_def), dimension(undf_w2),   intent(in)    :: f3data
    real(kind=r_def), dimension(undf_w3),   intent(in)    :: f4data

  end subroutine testkern_two_real_scalars_code

end module testkern_two_real_scalars_mod
