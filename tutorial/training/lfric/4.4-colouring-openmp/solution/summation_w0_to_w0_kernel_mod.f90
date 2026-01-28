! BSD 3-Clause License
!
! Copyright (c) 2023-2026, Science and Technology Facilities Council
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

module summation_w0_to_w0_kernel_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod, only: W0

  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: summation_w0_to_w0_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w0), &
             arg_type(gh_field, gh_real, gh_read, w0)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => summation_w0_to_w0_code
  end type summation_w0_to_w0_kernel_type

  public :: summation_w0_to_w0_code

contains

  !> This kernel adds the values from the second field to the first field.
  !> Note that each of these summation can be executed more than once: a vertex
  !> is shared among up to 8 finite elements. So, a vertex value (i.e. on W0)
  !> will be summed up to 8 times (if the vertex has 8 3D-finite-elements).
  !>
  !> @param[in] nlayers Integer the number of layers.
  !> @param[in,out] fld1 The field on W0 to store the sum.
  !> @param[in] fld2 The field on W0 which is added.
  !> @param[in] ndf_w0 Integer the number of degrees of freedom in W0
  !> @param[in] undf_w0 Integer value indicating the number of unique degrees
  !>                    of freedom in W0
  !> @param[in] map_w0 Integer array holding the dofmap for the cell at the
  !>                   base of the column for W0
  subroutine summation_w0_to_w0_code(nlayers, fld1, fld2, ndf_w0, undf_w0, map_w0)

    implicit none

    integer(kind=i_def), intent(in)                     :: nlayers
    integer(kind=i_def)                                 :: ndf_w0, undf_w0
    real(kind=r_def), dimension(undf_w0), intent(inout) :: fld1
    real(kind=r_def), dimension(undf_w0), intent(in)    :: fld2
    integer(kind=i_def), dimension(ndf_w0)              :: map_w0

    integer(kind=i_def)                                 :: i, k

    do k=0, nlayers-1
      do i=1, ndf_w0
        fld1(map_w0(i)+k) = fld1(map_w0(i)+k) + fld2(map_w0(i)+k)
      end do
    end do

  end subroutine summation_w0_to_w0_code

end module summation_w0_to_w0_kernel_mod
