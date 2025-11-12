! BSD 3-Clause License
!
! Copyright (c) 2024-2025, Science and Technology Facilities Council
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
! Author J. Henrichs, Bureau of Meteorology

module update_w0_with_w0_kernel_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod, only: w0

  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: update_w0_with_w0_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w0), &
             arg_type(gh_field, gh_real, gh_read, w0)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => update_w0_with_w0_kernel_code
  end type update_w0_with_w0_kernel_type

  public :: update_w0_with_w0_kernel_code

contains

    ! This kernel adds all 8 values from the field on W0 to the corresponding
    ! element in W3. It assumes lowest order finite elements.

    SUBROUTINE update_w0_with_w0_kernel_code(nlayers, field_update, field_in, ndf_w0, &
                                              undf_w0, map_w0)
      USE constants_mod
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in)                     :: nlayers
      INTEGER(KIND=i_def), intent(in)                     :: ndf_w0
      INTEGER(KIND=i_def), intent(in)                     :: undf_w0
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0)  :: map_w0
      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: field_update
      REAL(KIND=r_def), intent(in), dimension(undf_w0)    :: field_in

      integer(kind=i_def)                                 :: i, k

      do k=0, nlayers-1
        do i=1, ndf_w0
          field_update(map_w0(i)+k) = field_update(map_w0(i)+k) + field_in(map_w0(i)+k)
        end do
      end do

    END SUBROUTINE update_w0_with_w0_kernel_code

end module update_w0_with_w0_kernel_mod
