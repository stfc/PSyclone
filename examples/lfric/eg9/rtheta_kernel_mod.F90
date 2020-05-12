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
! Modifications copyright (c) 2018-2020, Science and Technology Facilities Council
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
!
!> @brief The kernel computes the rhs of the thermodynamic equation for the nonlinear
!>        equations, this constists entirely of the advection term u.grad(theta)
!> @details Kernel to  compute the rhs of thermodynamic equation for the nonlinear
!>          equations, in the absense of source terms this is purely an advection
!>          term: rtheta = u.grad(theta)
module rtheta_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,               &
                                    GH_FIELD, GH_READ, GH_INC,         &
                                    GH_BASIS, GH_DIFF_BASIS,           &
                                    CELLS, GH_QUADRATURE_XYoZ
use fs_continuity_mod,       only : W0, W2
use constants_mod,           only : r_def, i_def

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: rtheta_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  W0),                              &
       arg_type(GH_FIELD,   GH_READ, W0),                              &
       arg_type(GH_FIELD,   GH_READ, W2)                               &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(W0, GH_BASIS, GH_DIFF_BASIS),                         &
       func_type(W2, GH_BASIS)                                         &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ
contains
  procedure, nopass :: rtheta_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rtheta_code

contains

!> @brief Compute the right hand side of the thermodynamic equation
!! @param[in] nlayers Number of layers
!! @param[in,out] r_theta Right hand side of the thermodynamic equation
!! @param[in,out] theta Potential temperature
!! @param[in] u wind field
!! @param[in] ndf_w0 Number of degrees of freedom per cell for w0
!! @param[in] undf_w0  Number of unique degrees of freedom  for w0
!! @param[in] map_w0 Dofmap for the cell at the base of the column for w0
!! @param[in] w0_basis Basis functions evaluated at gaussian quadrature points
!! @param[in] w0_diff_basis Differential basis functions evaluated at gaussian quadrature points
!! @param[in] ndf_w2 Number of degrees of freedom per cell for w2
!! @param[in] undf_w2  Number of unique degrees of freedom  for w2
!! @param[in] map_w2 Dofmap for the cell at the base of the column for w2
!! @param[in] w2_basis Basis functions evaluated at gaussian quadrature points
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Weights of the horizontal quadrature points
!! @param[in] wqp_v Weights of the vertical quadrature points
subroutine rtheta_code(nlayers,                                                &
                       r_theta, theta, u,                                      &
                       ndf_w0, undf_w0, map_w0, w0_basis, w0_diff_basis,       &
                       ndf_w2, undf_w2, map_w2, w2_basis,                      &
                       nqp_h, nqp_v, wqp_h, wqp_v )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_w0, ndf_w2, undf_w0, undf_w2

  integer(kind=i_def), dimension(ndf_w0), intent(in) :: map_w0
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2

  real(kind=r_def), dimension(1,ndf_w0,nqp_h,nqp_v), intent(in) :: w0_basis
  real(kind=r_def), dimension(3,ndf_w0,nqp_h,nqp_v), intent(in) :: w0_diff_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis

  real(kind=r_def), dimension(undf_w0), intent(inout) :: r_theta
  real(kind=r_def), dimension(undf_w0), intent(in)    :: theta
  real(kind=r_def), dimension(undf_w2), intent(in)    :: u

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  ! Internal variables
  integer(kind=i_def)               :: df, k
  integer(kind=i_def)               :: qp1, qp2

  real(kind=r_def), dimension(ndf_w0)          :: rtheta_e, theta_e
  real(kind=r_def), dimension(ndf_w2)          :: u_e
  real(kind=r_def) :: u_at_quad(3), grad_theta_at_quad(3)
  real(kind=r_def) :: advective_term

  do k = 0, nlayers-1
    ! Extract element arrays of chi
    do df = 1, ndf_w0
      rtheta_e(df) = 0.0_r_def
      theta_e(df)  = theta(  map_w0(df) + k )
    end do
    do df = 1, ndf_w2
      u_e(df) = u( map_w2(df) + k )
    end do
    ! Compute the RHS integrated over one cell
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        u_at_quad(:) = 0.0_r_def
        do df = 1, ndf_w2
          u_at_quad(:)  = u_at_quad(:)  + u_e(df)*w2_basis(:,df,qp1,qp2)
        end do
        grad_theta_at_quad(:) = 0.0_r_def
        do df = 1, ndf_w0
          grad_theta_at_quad(:) = grad_theta_at_quad(:) &
                                + theta_e(df)*w0_diff_basis(:,df,qp1,qp2)
        end do

        advective_term = wqp_h(qp1)*wqp_v(qp2) &
                       * dot_product(u_at_quad,grad_theta_at_quad)

        do df = 1, ndf_w0
          rtheta_e(df) = rtheta_e(df) + w0_basis(1,df,qp1,qp2)*advective_term
        end do
      end do
    end do
    do df = 1, ndf_w0
      r_theta( map_w0(df) + k ) =  r_theta( map_w0(df) + k ) + rtheta_e(df)
    end do
  end do

end subroutine rtheta_code

end module rtheta_kernel_mod
