! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_ref_elem_all_faces_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_ref_elem_all_faces_type
     type(arg_type), dimension(5) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read),     &
             arg_type(gh_field,  gh_real, gh_inc,  w1), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w3)  &
             /)
     type(reference_element_data_type), dimension(2) ::             &
          meta_reference_element =                                  &
          (/ reference_element_data_type(outward_normals_to_faces), &
             reference_element_data_type(normals_to_faces) /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_ref_elem_all_faces_code
  end type testkern_ref_elem_all_faces_type

contains

  subroutine testkern_ref_elem_all_faces_code(nlayers, ascalar,        &
                                              fld1, fld2, fld3, fld4,  &
                                              ndf_w1, undf_w1, map_w1, &
                                              ndf_w2, undf_w2, map_w2, &
                                              ndf_w3, undf_w3, map_w3, &
                                              nfaces_re,               &
                                              out_normals, normals)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    real(kind=r_def), intent(in) :: ascalar
    real(kind=r_def), intent(inout), dimension(undf_w1) :: fld1
    real(kind=r_def), intent(in), dimension(undf_w2) :: fld2
    real(kind=r_def), intent(in), dimension(undf_w2) :: fld3
    real(kind=r_def), intent(in), dimension(undf_w3) :: fld4
    integer(kind=i_def), intent(in) :: nfaces_re
    real(kind=r_def), intent(in), dimension(3,nfaces_re) :: out_normals
    real(kind=r_def), intent(in), dimension(3,nfaces_re) :: normals

  end subroutine testkern_ref_elem_all_faces_code

end module testkern_ref_elem_all_faces_mod
