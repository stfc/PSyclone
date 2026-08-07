! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_scalar_array_mod

  use constants_mod
  use argument_mod
  use fs_continuity_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_scalar_array_type
     type(arg_type), dimension(5) :: meta_args =                  &
          (/ arg_type(gh_field,        gh_real,    gh_inc,  w1),  &
             arg_type(gh_scalar_array, gh_real,    gh_read, 2 ),  &
             arg_type(gh_scalar_array, gh_logical, gh_read, 1 ),  &
             arg_type(gh_scalar_array, gh_integer, gh_read, 4 ),  &
             arg_type(gh_scalar,       gh_integer, gh_read    )   &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_scalar_array_code
  end type testkern_scalar_array_type

contains

  subroutine testkern_scalar_array_code(nlayers, afield,            &
                                        dims_rarray, real_array,    &
                                        dims_larray, logical_array, &
                                        dims_iarray, integer_array, &
                                        a_scalar, ndf_w1, undf_w1,  &
                                        map_w1)
    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in), dimension(2) :: dims_rarray
    real(kind=r_def),    intent(in), dimension(dims_rarray(1),dims_rarray(2)) :: real_array
    integer(kind=i_def), intent(in), dimension(1) :: dims_larray
    logical(kind=l_def), intent(in), dimension(dims_larray(1)) :: logical_array
    integer(kind=i_def), intent(in), dimension(4) :: dims_iarray
    integer(kind=i_def), intent(in), dimension(dims_iarray(1),dims_iarray(2),dims_iarray(3),dims_iarray(4)) :: integer_array
    integer(kind=i_def), intent(in) :: a_scalar
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: undf_w1
    real(kind=r_def),    intent(inout), dimension(undf_w1) :: afield
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1

  end subroutine testkern_scalar_array_code

end module testkern_scalar_array_mod
