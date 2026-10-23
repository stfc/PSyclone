! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module matrix_vector_kernel_mod


use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,      &
                                    GH_FIELD, GH_OPERATOR,    &
                                    GH_REAL, GH_READ, GH_INC, &
                                    ANY_SPACE_1, CELL_COLUMN
use constants_mod,           only : r_def, i_def

implicit none

private

type, public, extends(kernel_type) :: matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                    &
       arg_type(GH_FIELD,    GH_REAL, GH_INC,  ANY_SPACE_1),             &
       arg_type(GH_FIELD,    GH_REAL, GH_READ, ANY_SPACE_1),             &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, ANY_SPACE_1) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: matrix_vector_code
end type

public matrix_vector_code

contains

  subroutine matrix_vector_code(cell, nlayers,  &
                                field1, field2, &
                                ncell_3d, op_3, &
                                ndf1, undf1, map1)

    implicit none

    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf1
    integer(kind=i_def), intent(in) :: undf1
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in), dimension(ndf1) :: map1
    real(kind=r_def), intent(inout), dimension(undf1) :: field1
    real(kind=r_def), intent(in), dimension(undf1)    :: field2
    real(kind=r_def), intent(in), dimension(ncell_3d,ndf1,ndf1) :: op_3

  end subroutine matrix_vector_code

end module matrix_vector_kernel_mod
