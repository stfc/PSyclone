!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2021, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!------------------------------------------------------------------------------
! Modified by A. Porter, STFC.
! Modified by I. Kavcic, Met Office.

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

  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d), intent(inout) :: op

  ! Local variables
  integer(kind=i_def) :: df, k, ik

  k = 1
  ik = (cell-1)*nlayers + k
  do df = 1,ndf1
    op(df,:,ik) = op(df,:,ik)*real(boundary_value(df,1), r_def)
  end do
  k = nlayers  
  ik = (cell-1)*nlayers + k
  do df = 1,ndf1
    op(df,:,ik) = op(df,:,ik)*real(boundary_value(df,2), r_def)
  end do

end subroutine enforce_operator_bc_code

end module enforce_operator_bc_kernel_mod
