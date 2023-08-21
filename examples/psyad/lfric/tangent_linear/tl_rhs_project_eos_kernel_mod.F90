!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Computes the rhs for the tangent linear of the equation of state
!>        by Galerkin projection.
!>
!> @details The kernel computes the rhs of the linearized equation of state
!>          by Galerkin projection.
!> The nonlinear is:
!> \f[ rhs_{\Pi} = 1 - p0/Rd * exner ^ (1-kappa)/kappa /(rho*theta_vd) \f]
!> Following \f[delta r = dr/dp delta p + dr/drho delta rho + dr/dt delta t \f]
!> The linear is:
!> \f[ rhs = -L ( gamma*exner/ls_exner - rho/ls_rho - theta/ls_theta ) \f]
!> where
!> \f[ L = p0/Rd * ls_exner ^ gamma /(ls_rho*ls_theta_vd) \f]
!> and \f[ gamma = (1-kappa)/kappa \f]

module tl_rhs_project_eos_kernel_mod

  use argument_mod,      only : arg_type, func_type,         &
                                GH_FIELD, GH_READ, GH_WRITE, &
                                GH_SCALAR,                   &
                                GH_REAL, ANY_SPACE_9,        &
                                ANY_DISCONTINUOUS_SPACE_3,   &
                                GH_BASIS, GH_DIFF_BASIS,     &
                                CELL_COLUMN, GH_QUADRATURE_XYoZ
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W3, Wtheta
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !-------------------------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: tl_rhs_project_eos_kernel_type
    private
    type(arg_type) :: meta_args(14) = (/                                     &
         arg_type(GH_FIELD,   GH_REAL, GH_WRITE, W3),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD*3, GH_REAL, GH_READ,  ANY_SPACE_9),               &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_3), &
         arg_type(GH_SCALAR, GH_REAL, GH_READ),                              &
         arg_type(GH_SCALAR, GH_REAL, GH_READ),                              &
         arg_type(GH_SCALAR, GH_REAL, GH_READ)                               &
         /)
    type(func_type) :: meta_funcs(3) = (/                                   &
         func_type(W3,          GH_BASIS),                                  &
         func_type(Wtheta,      GH_BASIS),                                  &
         func_type(ANY_SPACE_9, GH_BASIS, GH_DIFF_BASIS)                    &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass :: tl_rhs_project_eos_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: tl_rhs_project_eos_code

contains

!> @brief Computes rhs of the equation of state for the nonlinear equations
!! @param[in] nlayers       Number of layers
!! @param[in,out] rhs_eos   RHS array for the tangent linear equation of state
!! @param[in] exner         Change in Pressure
!! @param[in] rho           Change in Density
!! @param[in] theta         Change in Potential temperature
!! @param[in] moist_dyn_gas Change in Moist dynamics factor in gas law
!! @param[in] ls_exner      Lin state for Pressure
!! @param[in] ls_rho        Lin state for Density
!! @param[in] ls_theta      Lin state for Potential temperature
!! @param[in] ls_moist_dyn_gas Lin state for Moist dynamics factor in gas law
!! @param[in] chi_1         1st coordinate field in Wchi
!! @param[in] chi_2         2nd coordinate field in Wchi
!! @param[in] chi_3         3rd coordinate field in Wchi
!! @param[in] panel_id      Field giving the ID for mesh panels
!! @param[in] kappa         Ratio of rd and cp
!! @param[in] rd            Specific heat of dry air at constant density
!! @param[in] p_zero        Reference surface pressure
!! @param[in] ndf_w3        Number of degrees of freedom per cell for W3
!! @param[in] undf_w3       Number of (local) unique degrees of freedom
!! @param[in] map_w3        Dofmap for the cell at the base of the column for W3
!! @param[in] w3_basis      Basis functions evaluated at quadrature points
!! @param[in] ndf_wt        Number of degrees of freedom per cell for wt
!! @param[in] undf_wt       Number of (local) unique degrees of freedom
!! @param[in] map_wt        Dofmap for the cell at the base of the column for wt
!! @param[in] wt_basis      Basis functions evaluated at quadrature points
!! @param[in] ndf_chi       Number of degrees of freedom per cell for chi
!! @param[in] undf_chi      Number of (local) unique degrees of freedom for chi
!! @param[in] map_chi       Dofmap for the cell at the base of the column
!!                             for chi
!! @param[in] chi_basis     Wchi basis functions evaluated at quadrature points
!! @param[in] chi_diff_basis Wchi derivatives of basis functions
!!                             evaluated at quadrature points
!! @param[in] ndf_pid       Number of degrees of freedom per cell for panel_id
!! @param[in] undf_pid      Number of unique degrees of freedom for panel_id
!! @param[in] map_pid       Dofmap for the cell at the base of the column
!!                             for panel_id
!! @param[in] nqp_h         Number of quadrature points in the horizontal
!! @param[in] nqp_v         Number of quadrature points in the vertical
!! @param[in] wqp_h         Horizontal quadrature weights
!! @param[in] wqp_v         Vertical quadrature weights
subroutine tl_rhs_project_eos_code(nlayers,                                         &
                                   rhs_eos, exner, rho, theta, moist_dyn_gas,       &
                                   ls_exner, ls_rho, ls_theta, ls_moist_dyn_gas,    &
                                   chi1, chi2, chi3, panel_id,                      &
                                   kappa, rd, p_zero,                               &
                                   ndf_w3, undf_w3, map_w3, w3_basis,               &
                                   ndf_wt, undf_wt, map_wt, wt_basis,               &
                                   ndf_chi, undf_chi, map_chi,                      &
                                   chi_basis, chi_diff_basis,                       &
                                   ndf_pid, undf_pid, map_pid,                      &
                                   nqp_h, nqp_v, wqp_h, wqp_v)

  use coordinate_jacobian_mod,  only: coordinate_jacobian

  implicit none
  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_wt, ndf_w3, ndf_chi, ndf_pid
  integer(kind=i_def), intent(in) :: undf_wt, undf_w3, undf_chi, undf_pid
  integer(kind=i_def), dimension(ndf_w3),  intent(in) :: map_w3
  integer(kind=i_def), dimension(ndf_wt),  intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
  integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid

  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v),  intent(in) :: w3_basis
  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v),  intent(in) :: wt_basis
  real(kind=r_def), dimension(1,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_basis
  real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_diff_basis

  real(kind=r_def), dimension(undf_w3), intent(inout) :: rhs_eos
  real(kind=r_def), dimension(undf_wt), intent(in)    :: theta
  real(kind=r_def), dimension(undf_wt), intent(in)    :: moist_dyn_gas
  real(kind=r_def), dimension(undf_w3), intent(in)    :: rho, exner
  real(kind=r_def), dimension(undf_wt), intent(in)    :: ls_theta
  real(kind=r_def), dimension(undf_wt), intent(in)    :: ls_moist_dyn_gas
  real(kind=r_def), dimension(undf_w3), intent(in)    :: ls_rho, ls_exner
  real(kind=r_def), dimension(undf_chi), intent(in)   :: chi1, chi2, chi3
  real(kind=r_def), dimension(undf_pid), intent(in)   :: panel_id

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  real(kind=r_def), intent(in) :: kappa
  real(kind=r_def), intent(in) :: rd
  real(kind=r_def), intent(in) :: p_zero

  ! Internal variables
  integer(kind=i_def) :: df, k
  integer(kind=i_def) :: qp1, qp2

  real(kind=r_def), dimension(ndf_wt)  :: theta_vd_e
  real(kind=r_def), dimension(ndf_wt)  :: ls_theta_vd_e
  real(kind=r_def), dimension(ndf_chi) :: chi1_e, chi2_e, chi3_e
  real(kind=r_def), dimension(ndf_w3)  :: rho_e, exner_e
  real(kind=r_def)                     :: rho_quad, theta_vd_quad, exner_quad
  real(kind=r_def), dimension(ndf_w3)  :: ls_rho_e, ls_exner_e
  real(kind=r_def)                     :: ls_rho_quad, ls_theta_vd_quad
  real(kind=r_def)                     :: ls_exner_quad
  real(kind=r_def)                             :: eos, p0_over_rd, onemk_over_k
  real(kind=r_def)                             :: ls_eos
  real(kind=r_def), dimension(nqp_h,nqp_v)     :: dj
  real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac

  integer(kind=i_def) :: ipanel

  ipanel = int(panel_id(map_pid(1)), i_def)

  p0_over_rd = p_zero/Rd
  onemk_over_k = (1.0_r_def - kappa)/kappa

  do k = 0, nlayers-1

    do df = 1, ndf_chi
      chi1_e(df) = chi1(map_chi(df) + k)
      chi2_e(df) = chi2(map_chi(df) + k)
      chi3_e(df) = chi3(map_chi(df) + k)
    end do
    call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi1_e, chi2_e, chi3_e,  &
                             ipanel, chi_basis, chi_diff_basis, jac, dj)

    ! Linearisation state
    do df = 1, ndf_wt
      ls_theta_vd_e(df) = ls_theta(map_wt(df) + k) &
                        * ls_moist_dyn_gas(map_wt(df) + k)
    end do
    do df = 1, ndf_w3
      ls_exner_e(df) = ls_exner(map_w3(df) + k)
      ls_rho_e(df) = ls_rho(map_w3(df) + k)
    end do

    ! Perturbation
    do df = 1, ndf_wt
      theta_vd_e(df) = ( ls_theta(map_wt(df) + k) *         &
                         moist_dyn_gas(map_wt(df) + k) )    &
                     + ( theta(map_wt(df) + k) *            &
                         ls_moist_dyn_gas(map_wt(df) + k) )
    end do
    do df = 1, ndf_w3
      exner_e(df) = exner(map_w3(df) + k)
      rho_e(df) = rho(map_w3(df) + k)
      rhs_eos(map_w3(df)+k) = 0.0_r_def
    end do

    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h

        ! Linearisation state
        ls_theta_vd_quad = 0.0_r_def
        do df = 1, ndf_wt
          ls_theta_vd_quad = ls_theta_vd_quad &
                           + ls_theta_vd_e(df)*wt_basis(1,df,qp1,qp2)
        end do
        ls_exner_quad = 0.0_r_def
        ls_rho_quad = 0.0_r_def
        do df = 1, ndf_w3
          ls_exner_quad = ls_exner_quad + ls_exner_e(df)*w3_basis(1,df,qp1,qp2)
          ls_rho_quad   = ls_rho_quad   + ls_rho_e(df)  *w3_basis(1,df,qp1,qp2)
        end do

        ls_eos = 0.0_r_def - p0_over_rd * ls_exner_quad ** onemk_over_k &
               / ( ls_rho_quad *  ls_theta_vd_quad )

        ! Perturbation
        theta_vd_quad = 0.0_r_def
        do df = 1, ndf_wt
          theta_vd_quad = theta_vd_quad + theta_vd_e(df)*wt_basis(1,df,qp1,qp2)
        end do
        exner_quad = 0.0_r_def
        rho_quad = 0.0_r_def
        do df = 1, ndf_w3
          exner_quad = exner_quad + exner_e(df)*w3_basis(1,df,qp1,qp2)
          rho_quad   = rho_quad   + rho_e(df)  *w3_basis(1,df,qp1,qp2)
        end do

        ! Calculation
        eos = ( onemk_over_k * exner_quad / ls_exner_quad  - &
                rho_quad / ls_rho_quad                     - &
                theta_vd_quad / ls_theta_vd_quad ) * ls_eos

        eos = wqp_h(qp1) * wqp_v(qp2) * dj(qp1,qp2) * eos

        do df = 1, ndf_w3
          rhs_eos(map_w3(df)+k) = rhs_eos(map_w3(df)+k) &
                                - w3_basis(1,df,qp1,qp2) * eos
        end do

      end do
    end do
  end do

end subroutine tl_rhs_project_eos_code

end module tl_rhs_project_eos_kernel_mod
