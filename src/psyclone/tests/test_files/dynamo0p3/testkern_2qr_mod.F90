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
! Author A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module testkern_2qr_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod, only: r_def, i_def

  implicit none

  type, extends(kernel_type) :: testkern_2qr_type
     type(arg_type), dimension(4) :: meta_args =       &
          (/ arg_type(gh_field, gh_real, gh_inc,  w1), &
             arg_type(gh_field, gh_real, gh_read, w2), &
             arg_type(gh_field, gh_real, gh_read, w2), &
             arg_type(gh_field, gh_real, gh_read, w3)  &
           /)
     type(func_type), dimension(3) :: meta_funcs =     &
          (/ func_type(w1, gh_basis),                  &
             func_type(w2, gh_diff_basis),             &
             func_type(w3, gh_basis, gh_diff_basis)    &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape(2) = (/ gh_quadrature_face, gh_quadrature_edge /)
   contains
     procedure, nopass :: code => testkern_2qr_code
  end type testkern_2qr_type

contains

  subroutine testkern_2qr_code(                                              &
          nlayers, f1, f2, f3, f4,                                           &
          ndf_w1, undf_w1, map_w1, basis_w1_faces, basis_w1_edges,           &
          ndf_w2, undf_w2, map_w2, diff_basis_w2_faces, diff_basis_w2_edges, &
          ndf_w3, undf_w3, map_w3, basis_w3_faces, basis_w3_edges,           &
          diff_basis_w3_faces, diff_basis_w3_edges,                          &
          nfaces, nqp_faces, wqp_faces,                                      &
          nedges, nqp_edges, wqp_edges)

    implicit none

    integer(kind=i_def) :: nlayers
    integer(kind=i_def) :: ndf_w1, undf_w1, ndf_w2, &
                           undf_w2, ndf_w3, undf_w3
    integer(kind=i_def) :: nqp_faces, nfaces, nqp_edges, nedges
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w1) :: f1
    real(kind=r_def), intent(in), dimension(undf_w2)    :: f2
    real(kind=r_def), intent(in), dimension(undf_w2)    :: f3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: f4
    real(kind=r_def), dimension(nqp_faces,nfaces) :: wqp_faces
    real(kind=r_def), dimension(nqp_edges,nedges) :: wqp_edges
    real(kind=r_def), dimension(3,ndf_w1,nqp_faces,nfaces) :: basis_w1_faces
    real(kind=r_def), dimension(3,ndf_w2,nqp_faces,nfaces) :: diff_basis_w2_faces
    real(kind=r_def), dimension(1,ndf_w3,nqp_faces,nfaces) :: basis_w3_faces
    real(kind=r_def), dimension(1,ndf_w3,nqp_faces,nfaces) :: diff_basis_w3_faces
    real(kind=r_def), dimension(3,ndf_w1,nqp_edges,nedges) :: basis_w1_edges
    real(kind=r_def), dimension(3,ndf_w2,nqp_edges,nedges) :: diff_basis_w2_edges
    real(kind=r_def), dimension(1,ndf_w3,nqp_edges,nedges) :: basis_w3_edges
    real(kind=r_def), dimension(1,ndf_w3,nqp_edges,nedges) :: diff_basis_w3_edges

  end subroutine testkern_2qr_code

end module testkern_2qr_mod
