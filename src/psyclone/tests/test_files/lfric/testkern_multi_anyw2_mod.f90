! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_multi_anyw2_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  ! Test that multiple read and write arguments on any_w2 space
  ! produce correct code.
  type, public, extends(kernel_type) :: testkern_multi_anyw2_type
    private
    type(arg_type), dimension(3) :: meta_args = (/     &
         arg_type(gh_field, gh_real, gh_inc,  any_w2), &
         arg_type(gh_field, gh_real, gh_read, any_w2), &
         arg_type(gh_field, gh_real, gh_read, any_w2)  &
         /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => testkern_multi_anyw2_code
  end type testkern_multi_anyw2_type

contains

  subroutine testkern_multi_anyw2_code(nlayers, field_1_any_w2,        &
                                       field_2_any_w2, field_3_any_w2, &
                                       ndf_any_w2, undf_any_w2, map_any_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    real(kind=r_def), intent(inout), dimension(undf_any_w2) :: field_1_any_w2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: field_2_any_w2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: field_3_any_w2

  end subroutine testkern_multi_anyw2_code

end module testkern_multi_anyw2_mod
