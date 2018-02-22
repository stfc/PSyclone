!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2018, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by I Kavcic, Met Office
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the W2_solver_kernel  

!> @details Accessor functions for the W2_solver_kernel class are defined in this module.

module matrix_vector_mm_mod
use argument_mod,            only : arg_type,                               &
                                    GH_FIELD, GH_OPERATOR, GH_READ, GH_INC, &
                                    ANY_SPACE_1,                            &
                                    CELLS 
use constants_mod,           only : r_def
use kernel_mod,              only : kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: matrix_vector_kernel_mm_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &  
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_1),                    &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_1)        &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::matrix_vector_mm_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface matrix_vector_kernel_mm_type
   module procedure matrix_vector_kernel_mm_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public matrix_vector_mm_code
contains

  type(matrix_vector_kernel_mm_type) function matrix_vector_kernel_mm_constructor() result(self)
  return
end function matrix_vector_kernel_mm_constructor

!> @brief The subroutine which is called directly by the Psy layer, computes mass_matrix*x
!> @param[in]  cell the horizontal cell index
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] undf The unique number of degrees of freedom 
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!! @param[in] x Real array the data
!> @param[inout] lhs Real array, the output lhs (A*x)
!! @param[in] ncell_3d total number of cells
!! @param[in] mass_matrix Real: Array holding mass matrix values
subroutine matrix_vector_mm_code(cell,        &
                                 nlayers,     &
                                 lhs, x,      & 
                                 ncell_3d,    &
                                 mass_matrix, &
                                 ndf,undf,map)
 
  ! Arguments
  integer,                   intent(in)    :: cell, nlayers, ndf
  integer,                   intent(in)    :: undf, ncell_3d
  integer, dimension(ndf),   intent(in)    :: map
  real(kind=r_def), dimension(undf), intent(in)    :: x
  real(kind=r_def), dimension(undf), intent(inout) :: lhs
  real(kind=r_def), dimension(ndf,ndf,ncell_3d), intent(in) :: mass_matrix

  ! Internal variables
  integer                                  :: df, k, ik 
  real(kind=r_def), dimension(ndf)         :: x_e, lhs_e
 
  do k = 0, nlayers-1
    do df = 1, ndf  
      x_e(df) = x(map(df)+k)
    end do
    ik = (cell-1)*nlayers + k + 1
    lhs_e = matmul(mass_matrix(:,:,ik),x_e)
    do df = 1,ndf
       lhs(map(df)+k) = lhs(map(df)+k) + lhs_e(df) 
    end do
  end do
 
end subroutine matrix_vector_mm_code

end module matrix_vector_mm_mod
