!-------------------------------------------------------------------------------
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
! Authors: A. R. Porter and R. W. Ford, STFC Daresbury Lab
! Modified: I. Kavcic, Met Office

module setop_random_kernel_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: setop_random_kernel_type
     type(arg_type), dimension(1) :: meta_args =                  &
          (/ arg_type(gh_operator, gh_real, gh_write, any_space_1, any_space_2) &
             !arg_type(gh_field*3,  gh_real,    gh_read,  w0),     &
             !arg_type(gh_scalar,   gh_integer, gh_read)           &
          /)
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, nopass :: code => setop_random_kernel_code
  end type setop_random_kernel_type

contains

  subroutine setop_random_kernel_code(cell, nlayers, ncell_3d, &
                                      local_stencil, ndf_aspc1, ndf_aspc2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: ndf_aspc1, ndf_aspc2
    real(kind=r_def), intent(inout), dimension(ndf_aspc1,ndf_aspc2,ncell_3d) :: local_stencil
    ! local variables
    integer(kind=i_def) :: k, ik

    do k = 0, nlayers-1
      ik = (cell-1)*nlayers + k + 1
      call random_number(local_stencil(:,:,ik))
    end do

  end subroutine setop_random_kernel_code

end module setop_random_kernel_mod
