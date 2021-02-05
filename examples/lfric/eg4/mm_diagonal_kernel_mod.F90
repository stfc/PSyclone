!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
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
! Modified by I. Kavcic, Met Office

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
  procedure, nopass :: mm_diagonal_kernel_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public mm_diagonal_kernel_code

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
subroutine mm_diagonal_kernel_code(cell,        &
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
  real(kind=r_def), dimension(ndf,ndf,ncell_3d), intent(in) :: mass_matrix

  ! Internal variables
  integer(kind=i_def) :: df, k, ik

  do k = 0, nlayers-1
    ik = (cell-1)*nlayers + k + 1
    do df = 1,ndf
       mm_diag(map(df)+k) = mm_diag(map(df)+k) + mass_matrix(df,df,ik)
    end do
  end do

end subroutine mm_diagonal_kernel_code

end module mm_diagonal_kernel_mod
