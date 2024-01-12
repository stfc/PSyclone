! -----------------------------------------------------------------------------
!
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
! Author A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module prolong_vec_kernel_mod

  use constants_mod
  use argument_mod
  use kernel_mod
  use fs_continuity_mod

  implicit none

  private

  type, public, extends(kernel_type) :: prolong_vec_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args = (/                      &
          arg_type(GH_FIELD*3, GH_REAL, GH_INC,  W1, mesh_arg=GH_FINE),  &
          arg_type(GH_FIELD*3, GH_REAL, GH_READ, W2, mesh_arg=GH_COARSE) &
          /)
     integer :: operates_on = CELL_COLUMN
   contains
     procedure, nopass :: code => prolong_vec_kernel_code
  end type prolong_vec_kernel_type

  public :: prolong_vec_kernel_code

contains

  subroutine prolong_vec_kernel_code(nlayers,                      &
                                     cell_map,                     &
                                     ncell_f_per_c_x,              &
                                     ncell_f_per_c_y,              &
                                     ncell_f,                      &
                                     fine_1, fine_2, fine_3,       &
                                     coarse_1, coarse_2, coarse_3, &
                                     ndf_w1, undf_w1, dofmap_w1,   &
                                     undf_w2, dofmap_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ncell_f_per_c_x, ncell_f_per_c_y
    integer(kind=i_def), dimension(ncell_f_per_c_x, ncell_f_per_c_y), &
                         intent(in) :: cell_map
    integer(kind=i_def), intent(in) :: ncell_f
    integer(kind=i_def), intent(in) :: ndf_w1, undf_w1, undf_w2
    integer(kind=i_def), dimension(ndf_w1, ncell_f), intent(in) :: dofmap_w1
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: dofmap_w2
    real(kind=r_def), dimension(undf_w1), intent(inout) :: fine_1, fine_2, fine_3
    real(kind=r_def), dimension(undf_w2), intent(in) :: coarse_1, coarse_2, coarse_3

  end subroutine prolong_vec_kernel_code

end module prolong_vec_kernel_mod
