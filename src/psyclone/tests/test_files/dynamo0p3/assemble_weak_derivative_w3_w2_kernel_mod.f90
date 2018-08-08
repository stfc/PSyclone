!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017, Science and Technology Facilities Council
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

module assemble_weak_derivative_w3_w2_kernel_mod
  use constants_mod
  use argument_mod
  use kernel_mod
  IMPLICIT NONE

  type, public, extends(kernel_type) :: assemble_weak_derivative_w3_w2_kernel_type
    private
    type(arg_type) :: meta_args(2) = (/                        &
         ! Operator maps *to* FS W3 *from* FS W2
         arg_type(GH_OPERATOR, GH_WRITE, W3, W2),              &
         arg_type(GH_FIELD*3,  GH_READ,  W0)                   &
         /)
    type(func_type) :: meta_funcs(3) = (/                      &
         func_type(W0, GH_DIFF_BASIS),                         &
         func_type(W3, GH_BASIS),                              &
         func_type(W2, GH_DIFF_BASIS, GH_ORIENTATION)          &
         /)
    integer :: iterates_over = CELLS
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: assemble_weak_derivative_w3_w2_kernel_code
  end type
!
CONTAINS
!
  subroutine assemble_weak_derivative_w3_w2_kernel_code(cell, nlayers, &
       ncell_3d, local_stencil, xdata, ydata, zdata, ndf_w3, basis_w3, &
       ndf_w2, diff_basis_w2, orientation_w2, ndf_w0, undf_w0, map_w0, &
       diff_basis_w0, np_xy, np_z, weights_xy, weights_z)
    implicit none
    integer :: cell, nlayers, ncell_3d, ndf_w3, ndf_w2, ndf_w0, undf_w0
    integer :: np_xy, np_z
    integer, dimension(:) :: map_w0, orientation_w2
    real(kind=r_def), dimension(:,:,:) :: local_stencil
    real(kind=r_def), dimension(:) :: xdata, ydata, zdata, weights_xy, weights_z
    real(kind=r_def), dimension(:,:,:,:) :: basis_w3, diff_basis_w2, &
                                            diff_basis_w0
  end subroutine assemble_weak_derivative_w3_w2_kernel_code
!
end module assemble_weak_derivative_w3_w2_kernel_mod
