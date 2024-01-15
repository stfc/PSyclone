! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Laboratory
! Modified: I. Kavcic, Met Office

!> Test kernel requiring both face quadrature and the adjacent faces in the mesh.
module testkern_mesh_prop_face_qr_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_mesh_prop_face_qr_type
     type(arg_type), dimension(2) :: meta_args =    &
          (/ arg_type(gh_scalar, gh_real, gh_read), &
             arg_type(gh_field,  gh_real, gh_inc,  w1) /)
     type(func_type), dimension(1) :: meta_funcs = &
          (/ func_type(w1, gh_basis) /)
     type(mesh_data_type), dimension(1) :: meta_mesh = &
          (/ mesh_data_type(adjacent_face) /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_face
   contains
     procedure, nopass :: code => testkern_mesh_prop_face_qr_code
  end type testkern_mesh_prop_face_qr_type

contains

  subroutine testkern_mesh_prop_face_qr_code(nlayers, ascalar, fld1,           &
                                     ndf_w1, undf_w1, map_w1,                  &
                                     basis_w1_qr_face, nfaces_re_h, adj_faces, &
                                     nfaces_qr_face, nqp_qr_face, wqp_qr_face)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in) :: undf_w1
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    integer(kind=i_def), intent(in) :: nfaces_re_h
    integer(kind=i_def), intent(in), dimension(nfaces_re_h) :: adj_faces
    integer(kind=i_def), intent(in) :: nfaces_qr_face
    integer(kind=i_def), intent(in) :: nqp_qr_face
    real(kind=r_def), dimension(nqp_qr_face, nfaces_qr_face) :: wqp_qr_face
    real(kind=r_def), dimension(3,ndf_w1,nqp_qr_face,nfaces_qr_face) :: basis_w1_qr_face

  end subroutine testkern_mesh_prop_face_qr_code

end module testkern_mesh_prop_face_qr_mod
