! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_two_real_scalars_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_two_real_scalars_type
     type(arg_type), dimension(6) :: meta_args =        &
          (/ arg_type(gh_scalar, gh_real, gh_read    ), &
             arg_type(gh_field,  gh_real, gh_inc,  w1), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w2), &
             arg_type(gh_field,  gh_real, gh_read, w3), &
             arg_type(gh_scalar, gh_real, gh_read    )  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_two_real_scalars_code
  end type testkern_two_real_scalars_type

contains

  subroutine testkern_two_real_scalars_code(nlayers, a, f1data,        &
                                            f2data, f3data, f4data, b, &
                                            ndf_w1, undf_w1, map_w1,   &
                                            ndf_w2, undf_w2, map_w2,   &
                                            ndf_w3, undf_w3, map_w3 )

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1, ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    integer(kind=i_def), dimension(ndf_w1), intent(in)    :: map_w1
    integer(kind=i_def), dimension(ndf_w2), intent(in)    :: map_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in)    :: map_w3
    real(kind=r_def), intent(in)    :: a, b
    real(kind=r_def), dimension(undf_w1),   intent(inout) :: f1data
    real(kind=r_def), dimension(undf_w2),   intent(in)    :: f2data
    real(kind=r_def), dimension(undf_w2),   intent(in)    :: f3data
    real(kind=r_def), dimension(undf_w3),   intent(in)    :: f4data

  end subroutine testkern_two_real_scalars_code

end module testkern_two_real_scalars_mod
