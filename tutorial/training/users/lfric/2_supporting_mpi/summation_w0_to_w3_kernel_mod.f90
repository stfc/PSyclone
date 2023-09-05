! BSD 3-Clause License
!
! Copyright (c) 2017-2021, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author R. W. Ford, STFC Daresbury Lab
! Modified by J. Henrichs, Bureau of Meteorology
! Modified by I. Kavcic, Met Office

module summation_w0_to_w3_kernel_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod, only: w0, W3

  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: summation_w0_to_w3_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args =            &
          (/ arg_type(gh_field, gh_real, gh_readwrite, w3), &
             arg_type(gh_field, gh_real, gh_read,      w0)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => summation_w0_to_w3_kernel_code
  end type summation_w0_to_w3_kernel_type

  public :: summation_w0_to_w3_kernel_code

contains

    ! This kernel adds all 8 values from the field on W0 to the corresponding
    ! element in W3. It assumes lowest order finite elements.

    SUBROUTINE summation_w0_to_w3_kernel_code(nlayers, field_w3, field_w0, ndf_w3, &
                                              undf_w3, map_w3, ndf_w0, undf_w0, map_w0)
      USE constants_mod
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in)                     :: nlayers
      INTEGER(KIND=i_def), intent(in)                     :: ndf_w0
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0)  :: map_w0
      INTEGER(KIND=i_def), intent(in)                     :: ndf_w3
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3)  :: map_w3
      INTEGER(KIND=i_def), intent(in)                     :: undf_w3, undf_w0
      REAL(KIND=r_def), intent(inout), dimension(undf_w3) :: field_w3
      REAL(KIND=r_def), intent(in), dimension(undf_w0)    :: field_w0

      integer(kind=i_def)                                 :: i, k

      do k=0, nlayers-1
        do i=1, ndf_w0
          field_w3(map_w3(1)+k) = field_w3(map_w3(1)+k) + field_w0(map_w0(i)+k)
        end do
      end do

    END SUBROUTINE summation_w0_to_w3_kernel_code

end module summation_w0_to_w3_kernel_mod
