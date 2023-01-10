!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Compute the divergence operatore weigthed by the potential
!>        temperature.
!>
!> @details Compute the locally assembled operator
!>          \f[<\sigma,\nabla.(\theta*\mathbf{v})> \f]
!>          where sigma is the W3 test function, v is the W2 trial function
!>          and theta is the potential temperature.
!>
module weighted_div_kernel_mod

  use argument_mod,      only: arg_type, func_type,     &
                               GH_OPERATOR, GH_FIELD,   &
                               GH_SCALAR, GH_REAL,      &
                               GH_READ, GH_WRITE,       &
                               GH_BASIS, GH_DIFF_BASIS, &
                               CELL_COLUMN, GH_QUADRATURE_XYoZ
  use constants_mod,     only: r_def, i_def
  use fs_continuity_mod, only: W2, W3, Wtheta
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  type, public, extends(kernel_type) :: weighted_div_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                    &
         arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W2, W3), &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta), &
         arg_type(GH_SCALAR,   GH_REAL, GH_READ)           &
         /)
    type(func_type) :: meta_funcs(3) = (/                  &
         func_type(W2,     GH_BASIS, GH_DIFF_BASIS),       &
         func_type(W3,     GH_BASIS),                      &
         func_type(Wtheta, GH_BASIS, GH_DIFF_BASIS)        &
        /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass :: weighted_div_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: weighted_div_code

contains

!> @brief Computes the LMA form of the divegence operator
!! @param[in] cell Cell number
!! @param[in] nlayers Number of layers
!! @param[in] ncell_3d ncell*ndf
!! @param[in,out] div Local stencil of the div operator
!! @param[in] theta Potential temperature
!! @param[in] scalar Real to scale operator by
!! @param[in] ndf_w2 Number of degrees of freedom per cell
!! @param[in] basis_w2 Scalar basis functions evaluated at quadrature points
!! @param[in] diff_basis_w2 Differential vector basis functions evaluated
!!                          at quadrature points
!! @param[in] ndf_w3 Number of degrees of freedom per cell
!! @param[in] basis_w3 Basis functions evaluated at quadrature points
!! @param[in] ndf_wtheta Number of degrees of freedom per cell
!! @param[in] undf_wtheta Total number of degrees of freedom
!! @param[in] map_wtheta Dofmap for Wtheta
!! @param[in] basis_wtheta Basis functions evaluated at quadrature points
!! @param[in] diff_basis_wtheta Differential vector basis functions evaluated
!!            at quadrature points
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Horizontal quadrature weights
!! @param[in] wqp_v Vertical quadrature weights
subroutine weighted_div_code(cell, nlayers, ncell_3d,             &
                             div,                                 &
                             theta,                               &
                             scalar,                              &
                             ndf_w2, basis_w2, diff_basis_w2,     &
                             ndf_w3, basis_w3,                    &
                             ndf_wtheta, undf_wtheta, map_wtheta, &
                             basis_wtheta, diff_basis_wtheta,     &
                             nqp_h, nqp_v, wqp_h, wqp_v)

  implicit none

  ! Arguments

  integer(kind=i_def),                         intent(in) :: cell, nqp_h, nqp_v
  integer(kind=i_def),                         intent(in) :: nlayers
  integer(kind=i_def),                         intent(in) :: ncell_3d
  integer(kind=i_def),                         intent(in) :: ndf_w3, ndf_w2, ndf_wtheta, undf_wtheta
  integer(kind=i_def), dimension(ndf_wtheta),  intent(in) :: map_wtheta

  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v),       intent(in) :: basis_w3
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v),       intent(in) :: diff_basis_w2
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v),       intent(in) :: basis_w2
  real(kind=r_def), dimension(1,ndf_wtheta,nqp_h,nqp_v),   intent(in) :: basis_wtheta
  real(kind=r_def), dimension(3,ndf_wtheta,nqp_h,nqp_v),   intent(in) :: diff_basis_wtheta

  real(kind=r_def), dimension(ndf_w2,ndf_w3,ncell_3d), intent(inout) :: div
  real(kind=r_def), dimension(undf_wtheta),            intent(in)    :: theta
  real(kind=r_def),                                    intent(in)    :: scalar
  real(kind=r_def), dimension(nqp_h),                  intent(in)    :: wqp_h
  real(kind=r_def), dimension(nqp_v),                  intent(in)    :: wqp_v

  ! Internal variables
  integer(kind=i_def)                          :: df, df2, df3, k, ik
  integer(kind=i_def)                          :: qp1, qp2
  real(kind=r_def), dimension(ndf_wtheta)      :: theta_e
  real(kind=r_def)                             :: integrand
  real(kind=r_def)                             :: div_theta_v
  real(kind=r_def)                             :: theta_quad, grad_theta_quad(3)

  do k = 0, nlayers - 1
    ik = k + 1 + (cell-1)*nlayers
    do df = 1,ndf_wtheta
      theta_e(df) = theta(map_wtheta(df) + k)
    end do
    div(:,:,ik) = 0.0_r_def
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        theta_quad      = 0.0_r_def
        grad_theta_quad = 0.0_r_def
        do df = 1, ndf_wtheta
          theta_quad = theta_quad                                      &
                     + theta_e(df)*basis_wtheta(1,df,qp1,qp2)
          grad_theta_quad(:) = grad_theta_quad(:) &
                             + theta_e(df)*diff_basis_wtheta(:,df,qp1,qp2)
        end do
        do df3 = 1, ndf_w3
          integrand = scalar*wqp_h(qp1)*wqp_v(qp2)*basis_w3(1,df3,qp1,qp2)
          do df2 = 1, ndf_w2
            div_theta_v = diff_basis_w2(1,df2,qp1,qp2)*theta_quad &
                        + dot_product(basis_w2(:,df2,qp1,qp2),grad_theta_quad)
            div(df2,df3,ik) = div(df2,df3,ik) + integrand*div_theta_v
          end do
        end do
      end do
    end do
  end do

end subroutine weighted_div_code

end module weighted_div_kernel_mod
