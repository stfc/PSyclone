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

module testkern_any_space_1_mod
  use argument_mod
  use kernel_mod
  use constants_mod

! test for any_space producing correct code where there are a) more than one any_space declarations, 
! 2) an existing space as another argument (W0 in this case), 3) func_type basis functions on any_space.

type, public, extends(kernel_type) ::testkern_any_space_1_type
  private
  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1),                     &
       arg_type(GH_REAL,    GH_READ),                                  &
       ARG_TYPE(GH_FIELD,   GH_READ, ANY_SPACE_2),                     &
       ARG_TYPE(GH_FIELD*3, GH_READ, W0)                               &
       /)
  type(func_type) :: meta_funcs(3) = (/                                &
       func_type(ANY_SPACE_1, GH_BASIS),                               &
       FUNC_TYPE(ANY_SPACE_2, GH_BASIS),                               &
       FUNC_TYPE(W0,          GH_DIFF_BASIS)                           &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = gh_quadrature_XYoZ

contains
  procedure, public, nopass :: testkern_any_space_1_code
end type testkern_any_space_1_type
!
contains
  
  subroutine testkern_any_space_1_code(               &
       nlayers, flda, rdt, fldb, fldc1, fldc2, fldc3, &
       ndf_any_space_1, undf_any_space_1, map_any_space_1, basis_any_space_1, &
       ndf_any_space_2, undf_any_space_2, map_any_space_2, basis_any_space_2, &
       ndf_w0, undf_w0, map_w0, diff_basis_w0, np_xy, np_z, &
       weights_xy, weights_z)
    implicit none
    integer :: cell, nlayers, ncell_3d, ndf_w0, undf_w0
    integer :: ndf_any_space_1, undf_any_space_1, ndf_any_space_2, &
         undf_any_space_2
    integer :: np_xy, np_z
    integer, dimension(:) :: map_w0, map_any_space_1, map_any_space_2
    real(kind=r_def) :: rdt
    real(kind=r_def), dimension(:) :: flda, fldb, fldc1, fldc2, fldc3
    real(kind=r_def), dimension(:) :: weights_xy, weights_z
    real(kind=r_def), dimension(:,:,:,:) :: basis_any_space_1, &
         basis_any_space_2, diff_basis_w0

end subroutine testkern_any_space_1_code
!
end module testkern_any_space_1_mod
