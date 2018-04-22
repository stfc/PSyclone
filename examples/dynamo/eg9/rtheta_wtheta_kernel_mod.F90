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
! Modifications copyright (c) 2018, Science and Technology Facilities Council
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
!> @brief The kernel computes the rhs of the thermodynamic equation for the nonlinear
!>        equations for horizontally discontinuous temperature basis functions,
!>        this consists of the term theta*gamma*div(u) + theta u*grad(gamma)
!> @details Kernel to  compute the rhs of thermodynamic equation for the nonlinear
!>          equations, in the absense of source terms this is
!>          rtheta = -(theta*gamma*div(u) + theta u*grad(gamma))
module rtheta_wtheta_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_READ, GH_INC,               &
                                    W2, Wtheta,                              &
                                    GH_BASIS, GH_DIFF_BASIS, GH_ORIENTATION, &
                                    CELLS, GH_QUADRATURE_XYoZ
use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: rtheta_wtheta_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,   GH_READWRITE, Wtheta),                     &
       arg_type(GH_FIELD,   GH_READ,      Wtheta),                     &
       arg_type(GH_FIELD,   GH_READ,      W2)                          &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(Wtheta, GH_BASIS, GH_DIFF_BASIS),                     &
       func_type(W2, GH_BASIS, GH_DIFF_BASIS)                          &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ
contains
  procedure, nopass ::rtheta_wtheta_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface rtheta_wtheta_kernel_type
   module procedure rtheta_wtheta_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rtheta_wtheta_code
contains

type(rtheta_wtheta_kernel_type) function rtheta_wtheta_kernel_constructor() result(self)
  return
end function rtheta_wtheta_kernel_constructor

!> @brief Compute right hand side of the thermodynamic equation
!! @param[in] nlayers Number of layers
!! @param[inout] r_theta Right hand side of the thermodynamic equation
!! @param[inout] theta Potential temperature
!! @param[inout] u Velocity
!! @param[in] ndf_w2 Number of degrees of freedom per cell for w2
!! @param[in] undf_w2  Number of unique degrees of freedom  for w2
!! @param[in] map_w2 Dofmap for the cell at the base of the column for w2
!! @param[in] w2_basis Basis functions evaluated at gaussian quadrature points
!! @param[in] w2_diff_basis Differential basis functions evaluated at gaussian quadrature points
!! @param[in] ndf_wtheta Number of degrees of freedom per cell for wtheta
!! @param[in] undf_wtheta  Number of unique degrees of freedom  for wtheta
!! @param[in] map_wtheta Dofmap for the cell at the base of the column for wtheta
!! @param[in] wtheta_basis Basis functions evaluated at gaussian quadrature points
!! @param[in] wtheta_diff_basis Differential basis functions evaluated at gaussian quadrature points
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Weights of the horizontal quadrature points
!! @param[in] wqp_v Weights of the vertical quadrature points

subroutine rtheta_wtheta_code(nlayers,                                                        &
                       r_theta, theta, u,                                                     &
                       ndf_wtheta, undf_wtheta, map_wtheta, wtheta_basis, wtheta_diff_basis,  &
                       ndf_w2, undf_w2, map_w2, w2_basis, w2_diff_basis,                      &
                       nqp_h, nqp_v, wqp_h, wqp_v )


  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_w2, ndf_wtheta, undf_w2, undf_wtheta

  integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2

  real(kind=r_def), dimension(1,ndf_wtheta,nqp_h,nqp_v), intent(in) :: wtheta_basis
  real(kind=r_def), dimension(3,ndf_wtheta,nqp_h,nqp_v), intent(in) :: wtheta_diff_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_diff_basis

  real(kind=r_def), dimension(undf_wtheta), intent(inout) :: r_theta
  real(kind=r_def), dimension(undf_wtheta), intent(in)    :: theta
  real(kind=r_def), dimension(undf_w2), intent(in)    :: u

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  ! Internal variables
  integer(kind=i_def)               :: df, k
  integer(kind=i_def)               :: qp1, qp2

  real(kind=r_def), dimension(ndf_wtheta)      :: rtheta_e, theta_e
  real(kind=r_def), dimension(ndf_w2)          :: u_e
  real(kind=r_def) :: u_at_quad(3)
  real(kind=r_def) :: theta_at_quad, grad_theta_at_quad(3), div_u_at_quad
  real(kind=r_def) :: gamma_wtheta, grad_gamma_wtheta(3)

  do k = 0, nlayers-1
    ! Extract element arrays of chi
    do df = 1, ndf_wtheta
      rtheta_e(df) = 0.0_r_def
      theta_e(df)  = theta(  map_wtheta(df) + k )
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

        div_u_at_quad = 0.0_r_def
        do df = 1, ndf_w2
          div_u_at_quad    = div_u_at_quad + u_e(df)*w2_diff_basis(1,df,qp1,qp2)
        end do

        theta_at_quad = 0.0_r_def
        do df = 1, ndf_wtheta
          theta_at_quad   = theta_at_quad + theta_e(df)*wtheta_basis(1,df,qp1,qp2)
        end do

        grad_theta_at_quad(:) = 0.0_r_def
        do df = 1, ndf_wtheta
          grad_theta_at_quad(:) = grad_theta_at_quad(:) &
                                + theta_e(df)*wtheta_diff_basis(:,df,qp1,qp2)
        end do

        do df = 1, ndf_wtheta
          gamma_wtheta         = wtheta_basis(1, df, qp1, qp2)
          grad_gamma_wtheta(:) = wtheta_diff_basis(:, df, qp1, qp2)
          rtheta_e(df)         = rtheta_e(df) + wqp_h(qp1)*wqp_v(qp2) * theta_at_quad * &
                                   ( gamma_wtheta * div_u_at_quad + &
                                      dot_product(u_at_quad, grad_gamma_wtheta) )
        end do
      end do
    end do

    do df = 1, ndf_wtheta
      r_theta( map_wtheta(df) + k ) =  r_theta( map_wtheta(df) + k ) - rtheta_e(df)
    end do
  end do

end subroutine rtheta_wtheta_code

end module rtheta_wtheta_kernel_mod
