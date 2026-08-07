! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_w3_only_vector_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Description: discontinuous field vector writer and reader (w3)
  type, extends(kernel_type) :: testkern_w3_only_vector_type
     type(arg_type), dimension(2) :: meta_args =           &
          (/  arg_type(gh_field*3, gh_real, gh_write, w3), &
              arg_type(gh_field*3, gh_real, gh_read,  w3)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_w3_only_vector_code
  end type testkern_w3_only_vector_type

contains

  subroutine testkern_w3_only_vector_code(nlayers,   &
                                          field1_v1, &
                                          field1_v2, &
                                          field1_v3, &
                                          field2_v1, &
                                          field2_v2, &
                                          field2_v3, &
                                          ndf_w3, undf_w3, map_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field1_v1
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field1_v2
    real(kind=r_def), intent(inout), dimension(undf_w3) :: field1_v3
    real(kind=r_def), intent(in), dimension(undf_w3)  :: field2_v1
    real(kind=r_def), intent(in), dimension(undf_w3)  :: field2_v2
    real(kind=r_def), intent(in), dimension(undf_w3)  :: field2_v3

  end subroutine testkern_w3_only_vector_code

end module testkern_w3_only_vector_mod
