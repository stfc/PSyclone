!-------------------------------------------------------------------------------
! Copyright (c) 2018-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2018-2021, Science and Technology Facilities
! Council.
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
! Modified by R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified by I. Kavcic, Met Office
!
module ru_kernel_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: ru_kernel_type
    private
    type(arg_type) :: meta_args(6) = (/                                  &
         arg_type(GH_FIELD,   GH_REAL,    GH_INC,  W2),                  &
         arg_type(GH_FIELD,   GH_REAL,    GH_READ, W3),                  &
         arg_type(GH_SCALAR,  GH_INTEGER, GH_READ),                      &
         arg_type(GH_SCALAR,  GH_REAL,    GH_READ),                      &
         arg_type(GH_FIELD,   GH_REAL,    GH_READ, W0),                  &
         arg_type(GH_FIELD*3, GH_REAL,    GH_READ, W0)                   &
         /)
    type(func_type) :: meta_funcs(3) = (/                                &
         func_type(W2, GH_BASIS, GH_DIFF_BASIS),                         &
         func_type(W3, GH_BASIS),                                        &
         func_type(W0, GH_BASIS, GH_DIFF_BASIS)                          &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: ru_code
  end type

  public ru_code

contains

  subroutine ru_code(nlayers, field1, field2,         &
                     iscalar, rscalar, field5,        &
                     field6_v1, field6_v2, field6_v3, &
                     ndf_w2, undf_w2, map_w2,         &
                     basis_w2, diff_basis_w2,         &
                     ndf_w3, undf_w3, map_w3,         &
                     basis_w3,                        &
                     ndf_w0, undf_w0, map_w0,         &
                     basis_w0, diff_basis_w0,         &
                     np_xy, np_z, weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: undf_w0
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in) :: iscalar
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(inout), dimension(undf_w2) :: field1
    real(kind=r_def), intent(in), dimension(undf_w3)    :: field2
    real(kind=r_def), intent(in) :: rscalar
    real(kind=r_def), intent(in), dimension(undf_w0) :: field5
    real(kind=r_def), intent(in), dimension(undf_w0) :: field6_v1
    real(kind=r_def), intent(in), dimension(undf_w0) :: field6_v2
    real(kind=r_def), intent(in), dimension(undf_w0) :: field6_v3
    real(kind=r_def), intent(in), dimension(3,ndf_w2,np_xy,np_z) :: basis_w2
    real(kind=r_def), intent(in), dimension(1,ndf_w2,np_xy,np_z) :: diff_basis_w2
    real(kind=r_def), intent(in), dimension(1,ndf_w3,np_xy,np_z) :: basis_w3
    real(kind=r_def), intent(in), dimension(1,ndf_w0,np_xy,np_z) :: basis_w0
    real(kind=r_def), intent(in), dimension(3,ndf_w0,np_xy,np_z) :: diff_basis_w0
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine ru_code

end module ru_kernel_mod
