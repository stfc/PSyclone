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
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! Modified by A. Porter, STFC
! Modified by I. Kavcic, Met Office

!> @brief Applies boundary conditions to a field
!> @details Wrapper code for applying boundary conditions to a field
!>         When the Psyclone api is updated to correctly deal with
!>         boundary dofs this can be removed
module enforce_bc_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_INC,                        &
                                    ANY_SPACE_1,                             &
                                    CELLS
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
       arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1)                      &
       /)
  integer :: iterates_over = CELLS
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
    field(map(df) + k) = field(map(df) + k)*real(boundary_value(df,1))
  end do
  k = nlayers - 1
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_value(df,2))
  end do

end subroutine enforce_bc_code

end module enforce_bc_kernel_mod
