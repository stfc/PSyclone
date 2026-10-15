! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_one_int_scalar_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_one_int_scalar_type
     type(arg_type), dimension(5) :: meta_args =           &
          (/ arg_type(gh_field,  gh_real,    gh_inc,  w1), &
             arg_type(gh_scalar, gh_integer, gh_read    ), &
             arg_type(gh_field,  gh_real,    gh_read, w2), &
             arg_type(gh_field,  gh_real,    gh_read, w2), &
             arg_type(gh_field,  gh_real,    gh_read, w3)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_one_int_scalar_code
  end type testkern_one_int_scalar_type

contains

  subroutine testkern_one_int_scalar_code(nlayers, afield1, iflag,   &
                                          afield2, afield3, afield4, &
                                          ndf_w1, undf_w1, map_w1,   &
                                          ndf_w2, undf_w2, map_w2,   &
                                          ndf_w3, undf_w3, map_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2, undf_w3
    integer(kind=i_def), intent(in) :: iflag
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3
    real(kind=r_def), intent(inout), dimension(undf_w1) :: afield1
    real(kind=r_def), intent(in), dimension(undf_w2)    :: afield2
    real(kind=r_def), intent(in), dimension(undf_w2)    :: afield3
    real(kind=r_def), intent(in), dimension(undf_w3)    :: afield4

  end subroutine testkern_one_int_scalar_code

end module testkern_one_int_scalar_mod
