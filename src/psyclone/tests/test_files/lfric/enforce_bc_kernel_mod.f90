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

!> @brief Applies boundary conditions to a field
!> @details Wrapper code for applying boundary conditions to a field
!>          When the PSyclone api is updated to correctly deal with
!>          boundary dofs this can be removed
module enforce_bc_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,               &
                                    GH_FIELD, GH_REAL, GH_INC,         &
                                    ANY_SPACE_1,                       &
                                    CELL_COLUMN
use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: enforce_bc_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_FIELD,  GH_REAL,  GH_INC,  ANY_SPACE_1)             &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: enforce_bc_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public enforce_bc_code

contains

!> @brief Applies boundary conditions to a field
!! @param[in] nlayers Number of layers
!! @param[in,out] field Input/Output data
!! @param[in] ndf Number of degrees of freedom per cell
!! @param[in] undf Number unique of degrees of freedom
!! @param[in] map Dofmap for the cell at the base of the column
!! @param[in] boundary_value Flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
subroutine enforce_bc_code(nlayers,                        &
                           field,                          &
                           ndf, undf, map, boundary_value  &
                          )

  implicit none
 
  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: ndf
  integer(kind=i_def), intent(in) :: undf
  integer(kind=i_def), dimension(ndf),   intent(in) :: map
  integer(kind=i_def), dimension(ndf,2), intent(in) :: boundary_value

  real(kind=r_def), dimension(undf), intent(inout) :: field

  ! Local variables
  integer(kind=i_def) :: df, k

  k = 0
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_value(df,1), r_def)
  end do
  k = nlayers - 1 
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_value(df,2), r_def)
  end do

end subroutine enforce_bc_code

end module enforce_bc_kernel_mod
