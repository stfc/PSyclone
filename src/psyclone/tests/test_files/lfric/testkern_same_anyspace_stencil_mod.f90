! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_same_anyspace_stencil_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_same_anyspace_stencil_type
    type(arg_type), dimension(3) :: meta_args = (/                          &
         arg_type(gh_field, gh_real, gh_inc,  w1),                          &
         arg_type(gh_field, gh_real, gh_read, any_space_1, stencil(cross)), &
         arg_type(gh_field, gh_real, gh_read, any_space_1, stencil(cross))  &
         /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => testkern_same_anyspace_stencil_code
  end type testkern_same_anyspace_stencil_type

contains

  subroutine testkern_same_anyspace_stencil_code(nlayers, field1,             &
                                                 field2, field2_stencil_size, &
                                                 field2_stencil_dofmap,       &
                                                 field3, field3_stencil_size, &
                                                 field3_stencil_dofmap,       &
                                                 ndf_w1, undf_w1, map_w1,     &
                                                 ndf_aspc1, undf_aspc1, map_aspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_aspc1
    integer(kind=i_def), intent(in) :: undf_w1, undf_aspc1
    integer(kind=i_def), intent(in) :: field2_stencil_size, field3_stencil_size
    integer(kind=i_def), intent(in), dimension(ndf_aspc1) :: map_aspc1
    integer(kind=i_def), intent(in), dimension(ndf_w1)    :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_aspc1,field2_stencil_size) :: field2_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_aspc1,field3_stencil_size) :: field3_stencil_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w1)  :: field1
    real(kind=r_def), intent(in), dimension(undf_aspc1)  :: field2
    real(kind=r_def), intent(in), dimension(undf_aspc1)  :: field3

  end subroutine testkern_same_anyspace_stencil_code

end module testkern_same_anyspace_stencil_mod
