! -----------------------------------------------------------------------------
! Original under:
! Copyright (c) 2017-2026,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module scaled_matrix_vector_kernel_mod

use argument_mod,            only : arg_type,                 &
                                    GH_FIELD, GH_OPERATOR,    &
                                    GH_REAL, GH_READ, GH_INC, &
                                    ANY_SPACE_1, ANY_SPACE_2, &
                                    CELL_COLUMN
use fs_continuity_mod,       only : W3
use constants_mod,           only : r_def, i_def
use kernel_mod,              only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: scaled_matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                                      &
       arg_type(GH_FIELD,    GH_REAL, GH_INC,  ANY_SPACE_1),               &
       arg_type(GH_FIELD,    GH_REAL, GH_READ, ANY_SPACE_2),               &
       ! Modified so that the redundant computation example will run
       !arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, ANY_SPACE_2), &
       arg_type(GH_FIELD,    GH_REAL, GH_READ, W3),                        &
       arg_type(GH_FIELD,    GH_REAL, GH_READ, ANY_SPACE_1)                &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: scaled_matrix_vector_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public scaled_matrix_vector_code

contains

!> @brief Computes lhs = y*matrix*x where matrix maps from x space to lhs space
!>        and y is a field in the same space as lhs
!> @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!! @param[in,out] lhs Output lhs (A*x)
!! @param[in] x Input data
!! @param[in] ncell_3d Total number of cells
!! @param[in] matrix Local matrix assembly form of the operator A
!! @param[in] y Field to scale output by
!! @param[in] ndf1 Number of degrees of freedom per cell for the output field
!! @param[in] undf1 Unique number of degrees of freedom  for the output field
!! @param[in] map1 Dofmap for the cell at the base of the column for the output field
!! @param[in] ndf2 Number of degrees of freedom per cell for the input field
!! @param[in] undf2 Unique number of degrees of freedom for the input field
!! @param[in] map2 Dofmap for the cell at the base of the column for the input field
subroutine scaled_matrix_vector_code(cell,              &
                                     nlayers,           &
                                     lhs, x,            &
                                     ncell_3d,          &
                                     matrix,            &
                                     y,                 &
                                     ndf1, undf1, map1, &
                                     ndf2, undf2, map2)

  implicit none

  ! Arguments
  integer(kind=i_def),                   intent(in)    :: cell, nlayers, ncell_3d
  integer(kind=i_def),                   intent(in)    :: undf1, ndf1
  integer(kind=i_def),                   intent(in)    :: undf2, ndf2
  integer(kind=i_def), dimension(ndf1),  intent(in)    :: map1
  integer(kind=i_def), dimension(ndf2),  intent(in)    :: map2
  real(kind=r_def), dimension(undf2),              intent(in)    :: x
  real(kind=r_def), dimension(undf1),              intent(inout) :: lhs
  real(kind=r_def), dimension(ncell_3d,ndf1,ndf2), intent(in)    :: matrix
  real(kind=r_def), dimension(undf1),              intent(in)    :: y

  ! Internal variables
  integer(kind=i_def) :: df, df2, k, ik

  do df = 1, ndf1
    do df2 = 1, ndf2
      do k = 0, nlayers-1
        ik = (cell-1)*nlayers + k + 1
        lhs(map1(df)+k) = lhs(map1(df)+k) + matrix(ik,df,df2)*x(map2(df2)+k)*y(map1(df)+k)*z(map1(df)+k)
      end do
    end do
  end do

end subroutine scaled_matrix_vector_code

end module scaled_matrix_vector_kernel_mod
