!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
! Modified I. Kavcic, Met Office

module testkern_any_space_1_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod, only : r_def, i_def

  implicit none

   ! Test for any_space producing correct code where there are
   ! 1) more than one any_space declarations,
   ! 2) an existing space as another argument (W0 in this case),
   ! 3) func_type basis functions on any_space.
  type, extends(kernel_type) :: testkern_any_space_1_type
    type(arg_type) :: meta_args(4) = (/                       &
         arg_type(GH_FIELD,   GH_REAL, GH_INC,  ANY_SPACE_1), &
         arg_type(GH_SCALAR,  GH_REAL, GH_READ),              &
         arg_type(GH_FIELD,   GH_REAL, GH_READ, ANY_SPACE_2), &
         arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)           &
         /)
    type(func_type) :: meta_funcs(3) = (/                     &
         func_type(ANY_SPACE_1, GH_BASIS),                    &
         func_type(ANY_SPACE_2, GH_BASIS),                    &
         func_type(W0,          GH_DIFF_BASIS)                &
       /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: testkern_any_space_1_code
  end type testkern_any_space_1_type

contains
  
  subroutine testkern_any_space_1_code(                              &
                      nlayers, flda, rdt, fldb, fldc1, fldc2, fldc3, &
                      ndf_aspc1, undf_aspc1, map_aspc1, basis_aspc1, &
                      ndf_aspc2, undf_aspc2, map_aspc2, basis_aspc2, &
                      ndf_w0, undf_w0, map_w0, diff_basis_w0,        &
                      np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_aspc1, undf_aspc1
    integer(kind=i_def), intent(in) :: ndf_aspc2, undf_aspc2
    integer(kind=i_def), intent(in) :: ndf_w0, undf_w0
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_aspc1) :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_aspc2) :: map_aspc2
    integer(kind=i_def), intent(in), dimension(ndf_w0)    :: map_w0
    real(kind=r_def), intent(in) :: rdt
    real(kind=r_def), intent(inout), dimension(undf_aspc1) :: flda
    real(kind=r_def), intent(inout), dimension(undf_aspc2) :: fldb
    real(kind=r_def), intent(inout), dimension(undf_w0)    :: fldc1, fldc2, fldc3
    real(kind=r_def), intent(in), dimension(:,:,:,:) :: basis_aspc1, &
                                                        basis_aspc2, &
                                                        diff_basis_w0
    real(kind=r_def), intent(in), dimension(:) :: weights_xy, weights_z

  end subroutine testkern_any_space_1_code

end module testkern_any_space_1_mod
