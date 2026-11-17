! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_stencil_multi_2_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_stencil_multi_2_type
     type(arg_type), dimension(4) :: meta_args = (/                       &
          arg_type(gh_field, gh_real, gh_inc,  w1),                       &
          arg_type(gh_field, gh_real, gh_read, w1, stencil(xory1d)),      &
          arg_type(gh_field, gh_real, gh_read, w2, stencil(xory1d)),      &
          arg_type(gh_field, gh_real, gh_read, any_discontinuous_space_1, &
                                                   stencil(xory1d))       &
          /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_stencil_multi_2_code
  end type testkern_stencil_multi_2_type

contains

  subroutine testkern_stencil_multi_2_code(nlayers,                                 &
                                           field1,                                  &
                                           field2, field2_stencil_size,             &
                                           field2_direction, field2_stencil_dofmap, &
                                           field3, field3_stencil_size,             &
                                           field3_direction, field3_stencil_dofmap, &
                                           field4, field4_stencil_size,             &
                                           field4_direction, field4_stencil_dofmap, &
                                           ndf_w1, undf_w1, map_w1,                 &
                                           ndf_w2, undf_w2, map_w2,                 &
                                           ndf_adspc1, undf_adspc1, map_adspc1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_adspc1
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_adspc1
    integer(kind=i_def), intent(in) :: field2_stencil_size, field3_stencil_size, field4_stencil_size
    integer(kind=i_def), intent(in) :: field2_direction, field3_direction, field4_direction
    integer(kind=i_def), intent(in), dimension(ndf_w1)     :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2)     :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_adspc1) :: map_adspc1
    integer(kind=i_def), intent(in), dimension(ndf_w1,field2_stencil_size) :: field2_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,field3_stencil_size) :: field3_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_adspc1,field4_stencil_size) :: &
                                                                              field4_stencil_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w1)  :: field1
    real(kind=r_def), intent(in), dimension(undf_w1)     :: field2
    real(kind=r_def), intent(in), dimension(undf_w2)     :: field3
    real(kind=r_def), intent(in), dimension(undf_adspc1) :: field4

  end subroutine testkern_stencil_multi_2_code

end module testkern_stencil_multi_2_mod
