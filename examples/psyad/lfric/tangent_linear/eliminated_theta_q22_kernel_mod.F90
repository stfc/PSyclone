!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Compute the q22 matrix for analytic elimination of theta. The family
!!        of qXY matrices come from elimination of theta from the mixed solver
!> @details Mapping from the velocity space to itself that arises due to the
!!          analytic elimination of theta
!!          q22 = const * norm_u * <v, k dtheta/dchi3 * dexner/dchi3 * w / detj>
!!          where v is a test function in the W2 space,
!!          w is a basis function in the Wtheta space and k is unit vector
!!          in the vertical direction of the reference cell.
!!          For more details, see the solver section of
!!          https://code.metoffice.gov.uk/trac/lfric/wiki/GhaspSupport/Documentation
module eliminated_theta_q22_kernel_mod

  use argument_mod,            only: arg_type, func_type,     &
                                     GH_OPERATOR, GH_FIELD,   &
                                     GH_REAL, GH_SCALAR,      &
                                     GH_READ, GH_WRITE,       &
                                     GH_BASIS, GH_DIFF_BASIS, &
                                     CELL_COLUMN,             &
                                     GH_QUADRATURE_XYoZ,      &
                                     ANY_DISCONTINUOUS_SPACE_3

  use constants_mod,           only: i_def, r_def
  use coordinate_jacobian_mod, only: coordinate_jacobian
  use fs_continuity_mod,       only: W2, Wtheta, Wchi
  use kernel_mod,              only: kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  type, public, extends(kernel_type) :: eliminated_theta_q22_kernel_type
    private
    type(arg_type) :: meta_args(7) = (/                                      &
        arg_type(GH_OPERATOR, GH_REAL, GH_WRITE, W2, W2),                    &
        arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
        arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
        arg_type(GH_FIELD,    GH_REAL, GH_READ,  W2),                        &
        arg_type(GH_FIELD*3,  GH_REAL, GH_READ,  Wchi),                      &
        arg_type(GH_FIELD,    GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_3), &
        arg_type(GH_SCALAR,   GH_REAL, GH_READ)                              &
        /)
    type(func_type) :: meta_funcs(3) = (/                                    &
        func_type(W2,     GH_BASIS),                                         &
        func_type(Wtheta,           GH_DIFF_BASIS),                          &
        func_type(Wchi,   GH_BASIS, GH_DIFF_BASIS)                           &
        /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass :: eliminated_theta_q22_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public eliminated_theta_q22_code

contains

!> @brief Mapping from the velocity space to itself that arises due to the
!!        analytic elimination of theta, defined as:
!!        q22 = const * norm_u * <v, k dtheta/dchi3 * dexner/dchi3 * w / detj>.
!> @param[in]     cell           Horizontal cell index
!> @param[in]     nlayers        Number of layers
!> @param[in]     ncell_3d1      Number of cells in the 3D mesh
!> @param[in,out] q22_op         Projection matrix
!> @param[in]     theta          Potential temperature
!> @param[in]     exner          Exner pressure in the Wtheta space
!> @param[in]     norm_u         Normalisation for the momentum equation
!> @param[in]     chi1           First component of the coordinate array
!> @param[in]     chi2           Second component of the coordinate array
!> @param[in]     chi3           Third component of the coordinate array
!> @param[in]     panel_id       Field containing the panel ID indicator
!> @param[in]     const          Scalar constant to multiply operator by
!> @param[in]     ndf_w2         Number of degrees of freedom per cell for the velocity space
!> @param[in]     undf_w2        Total number of degrees of freedom for the velocity space
!> @param[in]     map_w2         Dofmap for the bottom layer in the velocity space
!> @param[in]     basis_w2       Basis functions for the W2 space evaluated at quadrature points
!> @param[in]     ndf_wt         Number of degrees of freedom per cell for the theta space
!> @param[in]     undf_wt        Total number of degrees of freedom for the theta space
!> @param[in]     map_wt         Dofmap for the bottom layer in the theta space
!> @param[in]     diff_basis_wt  Differential basis functions for the Wtheta space evaluated at quadrature points
!> @param[in]     ndf_chi        Number of degrees of freedom per cell for the coordinate space
!> @param[in]     undf_chi       Number of unique degrees of freedom for coordinate space
!> @param[in]     map_chi        Dofmap for the cell at the base of the column
!> @param[in]     basis_chi      Wchi basis functions evaluated at quadrature points
!> @param[in]     diff_basis_chi Wchi differential basis functions evaluated at quadrature points
!> @param[in]     ndf_pid        Number of degrees of freedom per cell for panel_id
!> @param[in]     undf_pid       Number of unique degrees of freedom for panel_id
!> @param[in]     map_pid        Dofmap for the cell at the base of the column for panel_id
!> @param[in]     nqp_h          Number of horizontal quadrature points
!> @param[in]     nqp_v          Number of vertical quadrature points
!> @param[in]     wqp_h          Horizontal quadrature weights
!> @param[in]     wqp_v          Vertical quadrature weights
subroutine eliminated_theta_q22_code(cell, nlayers, ncell_3d,    &
                                     q22_op,                     &
                                     theta, exner, norm_u,       &
                                     chi1, chi2, chi3,           &
                                     panel_id,                   &
                                     const,                      &
                                     ndf_w2, undf_w2, map_w2,    &
                                     basis_w2,                   &
                                     ndf_wt, undf_wt, map_wt,    &
                                     diff_basis_wt,              &
                                     ndf_chi, undf_chi, map_chi, &
                                     basis_chi, diff_basis_chi,  &
                                     ndf_pid, undf_pid, map_pid, &
                                     nqp_h, nqp_v, wqp_h, wqp_v)

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, ncell_3d, cell
  integer(kind=i_def), intent(in) :: nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_w2, undf_w2
  integer(kind=i_def), intent(in) :: ndf_wt, undf_wt
  integer(kind=i_def), intent(in) :: ndf_chi, undf_chi
  integer(kind=i_def), intent(in) :: ndf_pid, undf_pid

  integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
  integer(kind=i_def), dimension(ndf_wt),  intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_w2),  intent(in) :: map_w2
  integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid

  real(kind=r_def), dimension(3, ndf_w2,  nqp_h, nqp_v), intent(in) :: basis_w2
  real(kind=r_def), dimension(1, ndf_chi, nqp_h, nqp_v), intent(in) :: basis_chi
  real(kind=r_def), dimension(3, ndf_chi, nqp_h, nqp_v), intent(in) :: diff_basis_chi
  real(kind=r_def), dimension(3, ndf_wt,  nqp_h, nqp_v), intent(in) :: diff_basis_wt

  real(kind=r_def), dimension(ndf_w2, ndf_w2, ncell_3d), intent(inout) :: q22_op

  real(kind=r_def), dimension(undf_chi), intent(in) :: chi1, chi2, chi3
  real(kind=r_def), dimension(undf_wt),  intent(in) :: theta, exner
  real(kind=r_def), dimension(undf_w2),  intent(in) :: norm_u
  real(kind=r_def), dimension(undf_pid), intent(in) :: panel_id
  real(kind=r_def),                      intent(in) :: const
  real(kind=r_def), dimension(nqp_h),    intent(in) :: wqp_h
  real(kind=r_def), dimension(nqp_v),    intent(in) :: wqp_v

  ! Internal variables
  integer(kind=i_def) :: df, df2, k, ik
  integer(kind=i_def) :: qp1, qp2
  integer(kind=i_def) :: ipanel

  real(kind=r_def), dimension(ndf_chi)         :: chi1_e, chi2_e, chi3_e
  real(kind=r_def), dimension(3)               :: grad_theta, grad_exner, grad_term
  real(kind=r_def), dimension(nqp_h,nqp_v)     :: dj
  real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac

  ipanel = int(panel_id(map_pid(1)), i_def)

  do k = 0, nlayers-1
     ik = 1 + k + (cell-1)*nlayers

     do df = 1, ndf_chi
        chi1_e(df) = chi1(map_chi(df) + k)
        chi2_e(df) = chi2(map_chi(df) + k)
        chi3_e(df) = chi3(map_chi(df) + k)
     end do

    call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi1_e, chi2_e, chi3_e, &
                             ipanel, basis_chi, diff_basis_chi, jac, dj)
    q22_op(:, :, ik) = 0.0_r_def
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        grad_theta(:) = 0.0_r_def
        grad_exner(:) = 0.0_r_def
        ! Only take the vertical component of gradient
        do df = 1, ndf_wt
          grad_theta(3) = grad_theta(3) + theta(map_wt(df)+k)*diff_basis_wt(3, df, qp1, qp2)
          grad_exner(3) = grad_exner(3) + exner(map_wt(df)+k)*diff_basis_wt(3, df, qp1, qp2)
        end do
        ! Ensure dtheta/dz (and hence static stability: N^2 = g/theta *
        ! dtheta/dz) is positive
        grad_theta(3) = max(1.0_r_def, grad_theta(3))
        do df2 = 1, ndf_w2
          grad_term = wqp_h(qp1) * wqp_v(qp2) * const                     &
                     *dot_product(grad_theta(:), basis_w2(:,df2,qp1,qp2)) &
                     *grad_exner/dj(qp1,qp2)
          do df = 1, ndf_w2
            q22_op(df,df2,ik) = q22_op(df,df2,ik) + norm_u(map_w2(df)+k) &
                               *dot_product(basis_w2(:,df,qp1,qp2), grad_term)
          end do
        end do
       end do
    end do
  end do

end subroutine eliminated_theta_q22_code

end module eliminated_theta_q22_kernel_mod
