! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_ref_elem_mp_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_ref_elem_mp_type
     type(arg_type), dimension(1) :: meta_args =        &
          (/ arg_type(gh_field,  gh_real, gh_inc,  w1)  &
             /)
     type(reference_element_data_type), dimension(2) ::                &
          meta_reference_element =                                     &
          (/ reference_element_data_type(normals_to_horizontal_faces), &
             reference_element_data_type(normals_to_vertical_faces) /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_ref_elem_mp_code
  end type testkern_ref_elem_mp_type

contains

  subroutine testkern_ref_elem_mp_code(nlayers, fld1, ndf_w1, undf_w1, map_w1, &
               nfaces_re_h, nfaces_re_v, horiz_face_normals, vert_face_normals)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in) :: undf_w1
    real(kind=r_solver), intent(inout), dimension(undf_w1) :: fld1
    integer(kind=i_def), intent(in) :: nfaces_re_h
    integer(kind=i_def), intent(in) :: nfaces_re_v
    real(kind=r_def), intent(in), dimension(3,nfaces_re_h) :: horiz_face_normals
    real(kind=r_def), intent(in), dimension(3,nfaces_re_v) :: vert_face_normals

  end subroutine testkern_ref_elem_mp_code

end module testkern_ref_elem_mp_mod
