!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
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
! Modified by I. Kavcic, Met Office

!> @brief Kernel which operates on v3 field. Determines the RHS of Galerkin projection

!> @detail The kernel computes the integral of rho_df * P
!! with P_analytic over a single column


module v3_kernel_mod
use lfric
use argument_mod,            only: arg_type, &          ! the type
                                   gh_rw, v3, fe, cells ! the enums


implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: v3_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/ &
       arg_type(gh_rw,v3,fe,.true.,.false.,.true.) &
       /)
  integer :: iterates_over = cells

contains
  procedure, nopass :: rhs_v3_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rhs_v3_code

contains

!> @brief The subroutine which is called directly by the psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[in] ndf The number of degrees of freedom per cell
!! @param[in] map Integer array holding the dofmap for the cell at the base of the column
!! @param[in] v3_basis Real 5-dim array holding basis functions evaluated at gaussian quadrature points
!! @param[in,out] X Real array, the actual data
!! @param[in,out] gq Type, gaussian quadrature rule
subroutine rhs_v3_code(nlayers,ndf,map,v3_basis,x,gq)
  ! needs to compute the integral of rho_df * P
  ! P_analytic over a single column
  implicit none

  ! Arguments
  integer, intent(in) :: nlayers, ndf
  integer, intent(in) :: map(ndf)
  real(kind=dp), intent(in), dimension(ndf,ngp,ngp,ngp,1) :: v3_basis
  real(kind=dp), intent(inout) :: x(*)
  type(gaussian_quadrature_type), intent(inout) :: gq

  ! Internal variables
  integer               :: df, k
  integer               :: qp1, qp2, qp3
  real(kind=dp), dimension(ngp,ngp,ngp) :: f

  ! compute the analytic R integrated over one cell
  do k = 0, nlayers-1
    do df = 1, ndf
       do qp1 = 1, ngp
          do qp2 = 1, ngp
             do qp3 = 1, ngp
                f(qp1,qp2,qp3) = v3_basis(df,qp1,qp2,qp3,1) * real(k+1)
             end do
          end do
       end do
       x(map(df) + k) = gq%integrate(f)
    end do
  end do

end subroutine rhs_v3_code

end module v3_kernel_mod
