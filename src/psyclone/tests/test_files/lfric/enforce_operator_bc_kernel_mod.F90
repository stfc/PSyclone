! -----------------------------------------------------------------------------
! Original under:
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! Modifications under:
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> @brief Applies boundary conditions to a lma operator
!> @details Wrapper code for applying boundary conditions to a operator
module enforce_operator_bc_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,               &
                                    GH_OPERATOR, GH_READWRITE,         &
                                    GH_REAL, ANY_SPACE_1, ANY_SPACE_2, &
                                    CELL_COLUMN
use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: enforce_operator_bc_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                         &
       arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, ANY_SPACE_1, ANY_SPACE_2) &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: enforce_operator_bc_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public enforce_operator_bc_code

contains

!> @brief Applies boundary conditions to an operator
!> @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!! @param[in] ncell_3d Total number of cells
!! @param[in,out] op Operator data array to map from space 1 to space 2
!! @param[in] ndf1 Number of degrees of freedom per cell for to space
!! @param[in] ndf2 Number of degrees of freedom per cell for from space
!! @param[in] boundary_value Flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
subroutine enforce_operator_bc_code(cell, nlayers,                   &
                                    ncell_3d, op,                    &
                                    ndf1, ndf2, boundary_value       &
                                   )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, cell, ncell_3d
  integer(kind=i_def), intent(in) :: ndf1, ndf2
  integer(kind=i_def), dimension(ndf1,2), intent(in) :: boundary_value

  real(kind=r_def), dimension(ncell_3d,ndf1,ndf2), intent(inout) :: op

  ! Local variables
  integer(kind=i_def) :: df, k, ik

  k = 1
  ik = (cell-1)*nlayers + k
  do df = 1,ndf1
    op(ik,df,:) = op(ik,df,:)*real(boundary_value(df,1), r_def)
  end do
  k = nlayers  
  ik = (cell-1)*nlayers + k
  do df = 1,ndf1
    op(ik,df,:) = op(ik,df,:)*real(boundary_value(df,2), r_def)
  end do

end subroutine enforce_operator_bc_code

end module enforce_operator_bc_kernel_mod
