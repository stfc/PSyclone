!-----------------------------------------------------------------------------
! Original under:
! Copyright (c) 2017-2026, Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> @brief Stores the diagonal elements of a mass matrix

!> @details Stores the diagonal elements of a mass matrix M into a field D
!>          i.e D(df) = M(df,df)
module mm_diagonal_kernel_mod

use argument_mod,            only : arg_type,                 &
                                    GH_FIELD, GH_OPERATOR,    &
                                    GH_REAL, GH_READ, GH_INC, &
                                    ANY_SPACE_1,              &
                                    CELL_COLUMN
use constants_mod,           only : r_def, i_def
use kernel_mod,              only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: mm_diagonal_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                    &
       arg_type(GH_FIELD,    GH_REAL, GH_INC,  ANY_SPACE_1),             &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_SPACE_1, ANY_SPACE_1) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: mm_diagonal_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public mm_diagonal_code

contains

!> @brief Stores the diagonal of a mass_matrix
!> @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!! @param[in,out] mm_diag Diagonal entries of the mass matrix
!! @param[in] ncell_3d Total number of cells
!! @param[in] mass_matrix Mass matrix values
!! @param[in] ndf Number of degrees of freedom per cell
!! @param[in] undf Unique number of degrees of freedom
!! @param[in] map Dofmap for the cell at the base of the column
subroutine mm_diagonal_code(cell,        &
                            nlayers,     &
                            mm_diag,     &
                            ncell_3d,    &
                            mass_matrix, &
                            ndf, undf, map)

  implicit none

  ! Arguments
  integer(kind=i_def),                   intent(in) :: cell, nlayers, ndf
  integer(kind=i_def),                   intent(in) :: undf, ncell_3d
  integer(kind=i_def), dimension(ndf),   intent(in) :: map
  real(kind=r_def), dimension(undf), intent(inout) :: mm_diag
  real(kind=r_def), dimension(ncell_3d,ndf,ndf), intent(in) :: mass_matrix

  ! Internal variables
  integer(kind=i_def) :: df, k, ik

  do k = 0, nlayers-1
    ik = (cell-1)*nlayers + k + 1
    do df = 1,ndf
       mm_diag(map(df)+k) = mm_diag(map(df)+k) + mass_matrix(ik,df,df)
    end do
  end do

end subroutine mm_diagonal_code

end module mm_diagonal_kernel_mod
