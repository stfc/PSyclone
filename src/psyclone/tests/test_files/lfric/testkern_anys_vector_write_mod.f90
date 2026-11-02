! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module testkern_anys_vector_write_mod

  use argument_mod
  use kernel_mod
  use constants_mod

  implicit none

  type, extends(kernel_type) :: testkern_anys_vector_write_type
     type(arg_type), dimension(3) :: meta_args =             &
          (/ arg_type(gh_field*2, gh_real, gh_write,  any_space_1), &
             arg_type(gh_field,   gh_real, gh_read, any_w2), &
             arg_type(gh_field,   gh_real, gh_read, any_w2)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_anys_vector_write_code
  end type testkern_anys_vector_write_type
contains

  subroutine testkern_anys_vector_write_code(nlayers, fld1_v1, fld1_v2, fld2, &
                            fld3, ndf_as1_f1, undf_as1_f1, map_as1_f1, &
                            ndf_any_w2, undf_any_w2, map_any_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_as1_f1
    integer(kind=i_def), intent(in) :: undf_as1_f1
    integer(kind=i_def), intent(in) :: ndf_any_w2
    integer(kind=i_def), intent(in) :: undf_any_w2
    integer(kind=i_def), intent(in), dimension(ndf_as1_f1) :: map_as1_f1
    integer(kind=i_def), intent(in), dimension(ndf_any_w2) :: map_any_w2
    real(kind=r_def), intent(inout), dimension(undf_as1_f1) :: fld1_v1
    real(kind=r_def), intent(inout), dimension(undf_as1_f1) :: fld1_v2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: fld2
    real(kind=r_def), intent(in), dimension(undf_any_w2)    :: fld3

  end subroutine testkern_anys_vector_write_code

end module testkern_anys_vector_write_mod
