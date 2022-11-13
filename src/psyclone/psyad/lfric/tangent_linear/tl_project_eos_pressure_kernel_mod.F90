!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Tangent linear for computing the projection of the pressure field
!!        into the same space as density.
module tl_project_eos_pressure_kernel_mod

  use argument_mod,      only : arg_type, func_type,       &
                                GH_FIELD, GH_OPERATOR,     &
                                GH_READ, GH_WRITE,         &
                                GH_REAL, ANY_SPACE_2,      &
                                ANY_DISCONTINUOUS_SPACE_3, &
                                GH_BASIS, GH_DIFF_BASIS,   &
                                CELL_COLUMN, GH_QUADRATURE_XYoZ
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W3, Wtheta
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: tl_project_eos_pressure_kernel_type
    private
    type(arg_type) :: meta_args(10) = (/                                      &
         arg_type(GH_FIELD,    GH_REAL, GH_WRITE, W3),                        &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD*3,  GH_REAL, GH_READ,  ANY_SPACE_2),               &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  ANY_DISCONTINUOUS_SPACE_3), &
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W3, W3)                     &
         /)
    type(func_type) :: meta_funcs(3) = (/                                     &
         func_type(W3,          GH_BASIS),                                    &
         func_type(Wtheta,      GH_BASIS),                                    &
         func_type(ANY_SPACE_2, GH_BASIS, GH_DIFF_BASIS)                      &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass :: tl_project_eos_pressure_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: tl_project_eos_pressure_code

contains

!> @brief Compute the tangent linear for project pressure
!! @param[in] cell             Horizontal cell index
!! @param[in] nlayers          Number of layers
!! @param[in,out] exner        Change in pressure field
!! @param[in] rho              Change in density
!! @param[in] theta            Change in potential temperature
!! @param[in] moist_dyn_gas    Change in moist dynamics factor
!! @param[in] ls_rho           Lin state for density
!! @param[in] ls_theta         Lin state for potential temperature
!! @param[in] ls_moist_dyn_gas Lin state for moist dynamics factor
!! @param[in] chi_1            1st (spherical) coordinate field in Wchi
!! @param[in] chi_2            2nd (spherical) coordinate field in Wchi
!! @param[in] chi_3            3rd (spherical) coordinate field in Wchi
!! @param[in] panel_id         Field giving the ID for mesh panels
!! @param[in] ncell_3d         Number of cells
!! @param[in] m3_inv           Inverse of W3 mass matrix
!! @param[in] ndf_w3           Number of degrees of freedom per cell for w3
!! @param[in] undf_w3          Number of unique degrees of freedom for w3
!! @param[in] map_w3           Dofmap for the cell at the base of the column for w3
!! @param[in] w3_basis         Basis functions evaluated at Gaussian quadrature points
!! @param[in] ndf_wt           Number of degrees of freedom per cell for theta space
!! @param[in] undf_wt          Number of unique degrees of freedom for theta space
!! @param[in] map_wt           Dofmap for the cell at the base of the column for theta space
!! @param[in] wt_basis         Basis functions evaluated at Gaussian quadrature points
!! @param[in] ndf_chi          Number of degrees of freedom per cell for chi space
!! @param[in] undf_chi         Number of unique degrees of freedom for chi space
!! @param[in] map_chi          Dofmap for the cell at the base of the column for chi space
!! @param[in] chi_basis        Wchi basis functions evaluated at Gaussian quadrature points
!! @param[in] chi_diff_basis   Derivatives of Wchi basis functions
!!                             evaluated at Gaussian quadrature points
!! @param[in] ndf_pid          Number of degrees of freedom per cell for panel_id
!! @param[in] undf_pid         Number of unique degrees of freedom for panel_id
!! @param[in] map_pid          Dofmap for the cell at the base of the column for panel_id
!! @param[in] nqp_h            Number of quadrature points in the horizontal
!! @param[in] nqp_v            Number of quadrature points in the vertical
!! @param[in] wqp_h            horizontal quadrature weights
!! @param[in] wqp_v            vertical quadrature weights
subroutine tl_project_eos_pressure_code(cell, nlayers,                             &
                                 exner, rho, theta, moist_dyn_gas,             &
                                 ls_rho, ls_theta, ls_moist_dyn_gas,           &
                                 chi1, chi2, chi3,                             &
                                 panel_id,                                     &
                                 ncell_3d, m3_inv,                             &
                                 ndf_w3, undf_w3, map_w3, w3_basis,            &
                                 ndf_wt, undf_wt, map_wt, wt_basis,            &
                                 ndf_chi, undf_chi, map_chi,                   &
                                 chi_basis, chi_diff_basis,                    &
                                 ndf_pid, undf_pid, map_pid,                   &
                                 nqp_h, nqp_v, wqp_h, wqp_v                    &
                                 )

  use tl_calc_exner_pointwise_mod,only: tl_calc_exner_pointwise
  use coordinate_jacobian_mod, only: coordinate_jacobian

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, nqp_h, nqp_v, ncell_3d, cell
  integer(kind=i_def), intent(in) :: ndf_wt, ndf_w3, ndf_chi, ndf_pid
  integer(kind=i_def), intent(in) :: undf_wt, undf_w3, undf_chi, undf_pid
  integer(kind=i_def), dimension(ndf_wt),  intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
  integer(kind=i_def), dimension(ndf_w3),  intent(in) :: map_w3
  integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid

  real(kind=r_def), dimension(1,ndf_w3, nqp_h,nqp_v), intent(in) :: w3_basis
  real(kind=r_def), dimension(1,ndf_wt, nqp_h,nqp_v), intent(in) :: wt_basis
  real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_diff_basis
  real(kind=r_def), dimension(1,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_basis

  real(kind=r_def), dimension(undf_w3),  intent(inout) :: exner
  real(kind=r_def), dimension(undf_w3),  intent(in)    :: rho
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: theta
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: moist_dyn_gas
  real(kind=r_def), dimension(undf_w3),  intent(in)    :: ls_rho
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: ls_theta
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: ls_moist_dyn_gas
  real(kind=r_def), dimension(undf_chi), intent(in)    :: chi1, chi2, chi3
  real(kind=r_def), dimension(undf_pid), intent(in)    :: panel_id

  real(kind=r_def), dimension(ndf_w3,ndf_w3,ncell_3d), intent(in) :: m3_inv

  real(kind=r_def), dimension(nqp_h), intent(in) ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in) ::  wqp_v

  ! Internal variables
  integer(kind=i_def) :: df, k, ik, ipanel
  integer(kind=i_def) :: qp1, qp2

  real(kind=r_def), dimension(ndf_w3)          :: rho_e
  real(kind=r_def), dimension(ndf_w3)          :: r_exner, exner_e
  real(kind=r_def), dimension(ndf_wt)          :: theta_vd_e
  real(kind=r_def), dimension(ndf_w3)          :: ls_rho_e
  real(kind=r_def), dimension(ndf_wt)          :: ls_theta_vd_e
  real(kind=r_def), dimension(ndf_chi)         :: chi1_e, chi2_e, chi3_e
  real(kind=r_def), dimension(nqp_h,nqp_v)     :: dj
  real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac

  real(kind=r_def) :: exner_at_quad, rho_at_quad, theta_vd_at_quad
  real(kind=r_def) :: ls_rho_at_quad, ls_theta_vd_at_quad

  ipanel = int(panel_id(map_pid(1)), i_def)

  do k = 0, nlayers-1
    do df = 1, ndf_chi
      chi1_e(df) = chi1(map_chi(df) + k)
      chi2_e(df) = chi2(map_chi(df) + k)
      chi3_e(df) = chi3(map_chi(df) + k)
    end do
    call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi1_e, chi2_e, chi3_e,  &
                             ipanel, chi_basis, chi_diff_basis, jac, dj)

    ! Linearisation state
    do df = 1, ndf_w3
      ls_rho_e(df) = ls_rho( map_w3(df) + k )
    end do
    do df = 1, ndf_wt
      ls_theta_vd_e(df) = ls_theta( map_wt(df) + k )           &
                        * ls_moist_dyn_gas( map_wt(df) + k )
    end do

    ! Perturbation
    do df = 1, ndf_w3
      rho_e(df) = rho( map_w3(df) + k )
      r_exner(df) = 0.0_r_def
    end do
    do df = 1, ndf_wt
      theta_vd_e(df) = ( ls_theta( map_wt(df) + k )        &
                     * moist_dyn_gas( map_wt(df) + k ) )   &
                     + ( theta( map_wt(df) + k )           &
                     * ls_moist_dyn_gas( map_wt(df) + k ) )
    end do

    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h

        ! Linearisation state
        ls_rho_at_quad = 0.0_r_def
        do df = 1, ndf_w3
          ls_rho_at_quad  = ls_rho_at_quad + ls_rho_e(df)*w3_basis(1,df,qp1,qp2)
        end do
        ls_theta_vd_at_quad = 0.0_r_def
        do df = 1, ndf_wt
          ls_theta_vd_at_quad = ls_theta_vd_at_quad + ls_theta_vd_e(df)*wt_basis(1,df,qp1,qp2)
        end do

        ! Perturbation
        rho_at_quad = 0.0_r_def
        do df = 1, ndf_w3
          rho_at_quad  = rho_at_quad + rho_e(df)*w3_basis(1,df,qp1,qp2)
        end do
        theta_vd_at_quad = 0.0_r_def
        do df = 1, ndf_wt
          theta_vd_at_quad = theta_vd_at_quad + theta_vd_e(df)*wt_basis(1,df,qp1,qp2)
        end do

        ! Calculation
        exner_at_quad = wqp_h(qp1)*wqp_v(qp2)*dj(qp1,qp2)                          &
                      *tl_calc_exner_pointwise(rho_at_quad, theta_vd_at_quad,      &
                                               ls_rho_at_quad, ls_theta_vd_at_quad)

        do df = 1, ndf_w3
          r_exner(df) = r_exner(df) + w3_basis(1,df,qp1,qp2)*exner_at_quad
        end do
      end do
    end do
    ik = 1 + k + (cell-1)*nlayers
    exner_e = matmul(m3_inv(:,:,ik),r_exner)
    do df = 1, ndf_w3
      exner( map_w3(df) + k ) =  exner_e(df)
    end do
  end do

end subroutine tl_project_eos_pressure_code

end module tl_project_eos_pressure_kernel_mod
