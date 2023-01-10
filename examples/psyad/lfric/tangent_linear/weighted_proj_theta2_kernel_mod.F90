!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Compute the projection operator from the velocity space to
!!        the potential temperature space weighted by the potential temperature gradient
!> @details Compute the projection operator \f[<\gamma,\nabla(\theta*)v>\f]
!!          where v is in W2 and gamma is in the potential temperature space
!> @todo Create unit test for this kernel, see #2935
module weighted_proj_theta2_kernel_mod

use argument_mod,            only : arg_type, func_type,     &
                                    GH_OPERATOR, GH_FIELD,   &
                                    GH_SCALAR, GH_REAL,      &
                                    GH_READ, GH_WRITE,       &
                                    GH_BASIS, GH_DIFF_BASIS, &
                                    CELL_COLUMN, GH_QUADRATURE_XYoZ
use constants_mod,           only : r_def, i_def
use fs_continuity_mod,       only : W2, Wtheta
use kernel_mod,              only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: weighted_proj_theta2_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                        &
       arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, Wtheta, W2), &
       arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),     &
       arg_type(GH_SCALAR,   GH_REAL, GH_READ)               &
       /)
  type(func_type) :: meta_funcs(2) = (/                      &
       func_type(Wtheta, GH_BASIS, GH_DIFF_BASIS),           &
       func_type(W2,     GH_BASIS, GH_DIFF_BASIS)            &
       /)
  integer :: operates_on = CELL_COLUMN
  integer :: gh_shape = GH_QUADRATURE_XYoZ
contains
  procedure, nopass :: weighted_proj_theta2_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: weighted_proj_theta2_code

contains

!> @brief Compute the weighted projection operator that maps from W2 to Wtheta
!! @param[in] cell Current cell index
!! @param[in] nlayers Number of layers
!! @param[in] ncell_3d Total number of cells in the 3d mesh
!! @param[in,out] projection Locally assembled projection operator
!! @param[in] theta Potential temperature array
!! @param[in] scalar Real to scale matrix by
!! @param[in] ndf_wt Number of degrees of freedom per cell for Wtheta
!! @param[in] undf_wt Number of unique degrees of freedom for Wtheta
!! @param[in] map_wt Dofmap for the cell at the base of the column for Wtheta
!! @param[in] wt_basis Basis functions evaluated at Gaussian quadrature points
!! @param[in] wt_diff_basis Differential basis functions evaluated at
!!                          Gaussian quadrature points
!! @param[in] ndf_w2 Number of degrees of freedom per cell for W2
!! @param[in] w2_basis Basis functions evaluated at Gaussian quadrature points
!! @param[in] w2_diff_basis Differential basis functions evaluated at
!!                          Gaussian quadrature points
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Weights of the horizontal quadrature points
!! @param[in] wqp_v Weights of the vertical quadrature points
subroutine weighted_proj_theta2_code(cell, nlayers, ncell_3d,         &
                                     projection,                      &
                                     theta,                           &
                                     scalar,                          &
                                     ndf_wt, undf_wt, map_wt,         &
                                     wt_basis, wt_diff_basis,         &
                                     ndf_w2, w2_basis, w2_diff_basis, &
                                     nqp_h, nqp_v, wqp_h, wqp_v)

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: cell, nlayers, ncell_3d, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_wt, ndf_w2, undf_wt

  integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt

  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_basis
  real(kind=r_def), dimension(3,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_diff_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_diff_basis

  real(kind=r_def), dimension(ndf_wt,ndf_w2,ncell_3d), intent(inout) :: projection
  real(kind=r_def), dimension(undf_wt),                intent(in)    :: theta
  real(kind=r_def),                                    intent(in)    :: scalar

  real(kind=r_def), dimension(nqp_h), intent(in) ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in) ::  wqp_v

  ! Internal variables
  integer(kind=i_def) :: df, k, ik, dft, df2
  integer(kind=i_def) :: qp1, qp2

  real(kind=r_def), dimension(ndf_wt) :: theta_e
  real(kind=r_def) :: theta_at_quad
  real(kind=r_def) :: div_gamma_v, i1

  do k = 0, nlayers-1
    do df = 1, ndf_wt
      theta_e(df)  = theta( map_wt(df) + k )
    end do
    ik = k + 1 + (cell-1)*nlayers
    projection(:,:,ik) = 0.0_r_def
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        theta_at_quad = 0.0_r_def
        do df = 1, ndf_wt
          theta_at_quad = theta_at_quad + theta_e(df)*wt_basis(1,df,qp1,qp2)
        end do
        i1 = scalar*theta_at_quad*wqp_h(qp1)*wqp_v(qp2)
        do df2 = 1,ndf_w2
          do dft = 1,ndf_wt
            div_gamma_v = wt_basis(1,dft,qp1,qp2)*w2_diff_basis(1,df2,qp1,qp2) &
                        + dot_product(wt_diff_basis(:,dft,qp1,qp2),w2_basis(:,df2,qp1,qp2))
            projection(dft,df2,ik) = projection(dft,df2,ik) - div_gamma_v*i1
          end do
        end do
      end do
    end do
  end do

end subroutine weighted_proj_theta2_code

end module weighted_proj_theta2_kernel_mod
