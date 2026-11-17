! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
