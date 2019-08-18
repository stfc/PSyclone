! -----------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Copyright (c) 2018-2019, Science and Technology Facilities Council
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
! Author A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module restrict_test_kernel_mod

  use constants_mod
  use kernel_mod
  use argument_mod

  implicit none

  type, public, extends(kernel_type) :: restrict_test_kernel_type
     private
     type(arg_type) :: meta_args(2) = (/                                &
          arg_type(GH_FIELD, GH_INC,  ANY_SPACE_1, mesh_arg=GH_COARSE), &
          arg_type(GH_FIELD, GH_READ, ANY_SPACE_2, mesh_arg=GH_FINE   ) &
          /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: restrict_test_kernel_code
  end type restrict_test_kernel_type

  public :: restrict_test_kernel_code

contains

  subroutine restrict_test_kernel_code(nlayers,                &
                                       cell_map,               &
                                       ncell_f_per_c,          &
                                       ncell_f,                &
                                       coarse, fine,           &
                                       undf_any1, dofmap_any1, &
                                       ndf_any2, undf_any2, dofmap_any2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_f_per_c
    integer(kind=i_def), dimension(ncell_f_per_c), intent(in) :: cell_map
    integer(kind=i_def), intent(in) :: ncell_f
    integer(kind=i_def), intent(in) :: ndf_any2
    integer(kind=i_def), dimension(ndf_any2, ncell_f), intent(in) :: dofmap_any2
    integer(kind=i_def), dimension(ndf_any2), intent(in) :: dofmap_any1
    integer(kind=i_def), intent(in) :: undf_any2, undf_any1
    real(kind=r_def), dimension(undf_any1), intent(inout) :: coarse
    real(kind=r_def), dimension(undf_any2), intent(in) :: fine

  end subroutine restrict_test_kernel_code

end module restrict_test_kernel_mod
