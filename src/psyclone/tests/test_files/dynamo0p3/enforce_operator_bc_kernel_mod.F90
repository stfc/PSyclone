!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

! Modified by A. Porter, STFC.

! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017, Science and Technology Facilities Council
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
!-------------------------------------------------------------------------------

!> @brief Applies boundary conditions to a lma operator
!> @details Wrapper code for applying boundary conditions to a operator
module enforce_operator_bc_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_OPERATOR, GH_INC,                     &
                                    ANY_SPACE_1, ANY_SPACE_2,                &
                                    CELLS
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: enforce_operator_bc_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                               &
       arg_type(GH_OPERATOR, GH_INC, ANY_SPACE_1, ANY_SPACE_2)      &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: enforce_operator_bc_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface enforce_operator_bc_kernel_type
  module procedure enforce_operator_bc_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public enforce_operator_bc_code
contains

type(enforce_operator_bc_kernel_type) function enforce_operator_bc_kernel_constructor() result(self)
  return
end function enforce_operator_bc_kernel_constructor

!> @brief Applies boundary conditions to an operator
!> @param[in] cell Horizontal cell index
!! @param[in] nlayers Number of layers
!! @param[in] ncell_3d Total number of cells
!! @param[inout] op Operator data array to map from space 1 to space 2
!! @param[in] ndf1 Number of degrees of freedom per cell for to space
!! @param[in] ndf2 Number of degrees of freedom per cell for from space
!! @param[in] boundary_value Flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
subroutine enforce_operator_bc_code(cell, nlayers,                   &
                                    op, ncell_3d,                    &
                                    ndf1, ndf2, boundary_value       &
                                   )
  
  !Arguments
  integer, intent(in) :: nlayers, cell, ncell_3d
  integer, intent(in) :: ndf1, ndf2
  integer, dimension(ndf1,2), intent(in) :: boundary_value

  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d), intent(inout) :: op

  ! Local variables
  integer :: df, k, ik

  k = 1
  ik = (cell-1)*nlayers + k
  do df = 1,ndf1
    op(df,:,ik) = op(df,:,ik)*real(boundary_value(df,1))
  end do
  k = nlayers  
  ik = (cell-1)*nlayers + k
  do df = 1,ndf1
    op(df,:,ik) = op(df,:,ik)*real(boundary_value(df,2))
  end do

end subroutine enforce_operator_bc_code

end module enforce_operator_bc_kernel_mod
