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

!> @brief Provides access to the members of the v2_kernel class.

!> @details Accessor functions for the v2_kernel class are defined in this module.

!> @param RHS_v2_code              Code to implement the RHS for a v2 field
!> @param gaussian_quadrature      Contains result of gaussian quadrature

module v2_kernel_mod
use constants_mod,           only: dp
use kernel_mod,              only: kernel_type
use gaussian_quadrature_mod, only: gaussian_quadrature_type, &
                                   ngp_h,ngp_v
use argument_mod,            only: arg_type, &          ! the type
                                   gh_inc, v2, fe, cells ! the enums


implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: v2_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/ &
       arg_type(gh_inc,v2,fe,.true.,.false.,.false.,.true.) &
       /)
  integer :: iterates_over = cells

contains
  procedure, nopass :: rhs_v2_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rhs_v2_code

contains

!> @brief This subroutine calculates the RHS of Galerkin projection on W2 space.
!! @param[in] nlayers Integer: The number of layers.
!! @param[in] ndf Integer: The number of degrees of freedom per cell.
!! @param[in] map Integer: Array holding the dofmap for the cell at the base of the column.
!! @param[in] v3_basis Real: 4-dim array holding VECTOR basis functions evaluated at quadrature points.
!! @param[in,out] X Real: The array of actual data.
!! @param[in,out] gq Type: Quadrature rule (here Gaussian).
subroutine rhs_v2_code(nlayers,ndf,map,basis,x,gq)
  ! Needs to compute the integral of v_df * P
  !  P_analytic over a single column
  implicit none

  ! Arguments
  integer,                                     intent(in)    :: nlayers, ndf
  integer,                                     intent(in)    :: map(ndf)
  real(kind=dp), dimension(3,ndf,ngp_h,ngp_v), intent(in)    :: basis
  real(kind=dp),                               intent(inout) :: x(*)
  type(gaussian_quadrature_type),              intent(inout) :: gq

  ! Internal variables
  integer               :: df, k
  integer               :: qp1, qp2
  real(kind=dp), dimension(ngp_h,ngp_v) :: f
  real(kind=dp), dimension(1,3) :: basisfun_i
  real(kind=dp), dimension(3,1) :: constantvec
  real(kind=dp), dimension(1,1) :: T_1

  constantvec(1,1) =  4.0_dp;
  constantvec(2,1) =  2.0_dp;
  constantvec(3,1) =  1.0_dp;
  ! Compute the analytic R integrated over one cell
  do k = 0, nlayers-1
    do df = 1, ndf
       do qp1 = 1, ngp_h
          do qp2 = 1, ngp_v
             basisfun_i(1,1:3) = basis(1:3,df,qp1,qp2)
             T_1 = matmul(basisfun_i,constantvec)
             f(qp1,qp2) = T_1(1,1)
          end do
       end do
       ! Push data to global array
       x(map(df) + k) = x(map(df) + k) + gq%integrate(f)
    end do
 end do

end subroutine rhs_v2_code

end module v2_kernel_mod
