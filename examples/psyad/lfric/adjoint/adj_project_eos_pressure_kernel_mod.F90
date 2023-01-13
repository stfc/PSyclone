module adj_project_eos_pressure_kernel_mod
  use argument_mod, only : any_discontinuous_space_3, any_space_2, arg_type, cell_column, func_type, gh_basis, gh_diff_basis, &
&gh_field, gh_operator, gh_quadrature_xyoz, gh_read, gh_real, gh_readwrite
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w3, wtheta
  use kernel_mod, only : kernel_type
  use planet_config_mod, only : kappa, Rd, p_zero
  implicit none
  type, public, extends(kernel_type) :: adj_project_eos_pressure_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(10) = (/&
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_FIELD * 3, GH_REAL, GH_READ, ANY_SPACE_2), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_3), &
       arg_type(GH_OPERATOR, GH_REAL, GH_READ, W3, W3)/)
  TYPE(func_type) :: meta_funcs(3) = (/func_type(W3, GH_BASIS), &
       func_type(Wtheta, GH_BASIS), func_type(ANY_SPACE_2, GH_BASIS, &
&GH_DIFF_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_QUADRATURE_XYoZ
  CONTAINS
  PROCEDURE, NOPASS :: adj_project_eos_pressure_code
END TYPE
  private

  public :: adj_project_eos_pressure_code

  contains
  subroutine adj_project_eos_pressure_code(cell, nlayers, exner, rho, theta, moist_dyn_gas, ls_rho, ls_theta, ls_moist_dyn_gas, &
&chi1, chi2, chi3, panel_id, ncell_3d, m3_inv, ndf_w3, undf_w3, map_w3, w3_basis, ndf_wt, undf_wt, map_wt, wt_basis, ndf_chi, &
&undf_chi, map_chi, chi_basis, chi_diff_basis, ndf_pid, undf_pid, map_pid, nqp_h, nqp_v, wqp_h, wqp_v)
    use coordinate_jacobian_mod, only : coordinate_jacobian
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nqp_h
    integer(kind=i_def), intent(in) :: nqp_v
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: ndf_chi
    integer(kind=i_def), intent(in) :: ndf_pid
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: undf_chi
    integer(kind=i_def), intent(in) :: undf_pid
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_pid), intent(in) :: map_pid
    real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v), intent(in) :: w3_basis
    real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_basis
    real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_diff_basis
    real(kind=r_def), dimension(1,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_basis
    real(kind=r_def), dimension(undf_w3), intent(inout) :: exner
    real(kind=r_def), dimension(undf_w3), intent(inout) :: rho
    real(kind=r_def), dimension(undf_wt), intent(inout) :: theta
    real(kind=r_def), dimension(undf_wt), intent(inout) :: moist_dyn_gas
    real(kind=r_def), dimension(undf_w3), intent(in) :: ls_rho
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_theta
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_moist_dyn_gas
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi1
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi2
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi3
    real(kind=r_def), dimension(undf_pid), intent(in) :: panel_id
    real(kind=r_def), dimension(ndf_w3,ndf_w3,ncell_3d), intent(in) :: m3_inv
    real(kind=r_def), dimension(nqp_h), intent(in) :: wqp_h
    real(kind=r_def), dimension(nqp_v), intent(in) :: wqp_v
    integer(kind=i_def) :: df
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    integer(kind=i_def) :: ipanel
    integer(kind=i_def) :: qp1
    integer(kind=i_def) :: qp2
    real(kind=r_def), dimension(ndf_w3) :: rho_e
    real(kind=r_def), dimension(ndf_w3) :: r_exner
    real(kind=r_def), dimension(ndf_w3) :: exner_e
    real(kind=r_def), dimension(ndf_wt) :: theta_vd_e
    real(kind=r_def), dimension(ndf_w3) :: ls_rho_e
    real(kind=r_def), dimension(ndf_wt) :: ls_theta_vd_e
    real(kind=r_def), dimension(ndf_chi) :: chi1_e
    real(kind=r_def), dimension(ndf_chi) :: chi2_e
    real(kind=r_def), dimension(ndf_chi) :: chi3_e
    real(kind=r_def), dimension(nqp_h,nqp_v) :: dj
    real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac
    real(kind=r_def) :: exner_at_quad
    real(kind=r_def) :: rho_at_quad
    real(kind=r_def) :: theta_vd_at_quad
    real(kind=r_def) :: ls_rho_at_quad
    real(kind=r_def) :: ls_theta_vd_at_quad
    real(kind=r_def) :: tmp_ls_exner
    real(kind=r_def) :: tmp_exner
    integer :: i
    integer :: j

    exner_at_quad = 0.0_r_def
    r_exner = 0.0_r_def
    exner_e = 0.0_r_def
    rho_at_quad = 0.0_r_def
    rho_e = 0.0_r_def
    tmp_exner = 0.0_r_def
    theta_vd_at_quad = 0.0_r_def
    theta_vd_e = 0.0_r_def
    ipanel = INT(panel_id(map_pid(1)), i_def)
    do k = nlayers - 1, 0, -1
      do df = 1, ndf_chi, 1
        chi1_e(df) = chi1(map_chi(df) + k)
        chi2_e(df) = chi2(map_chi(df) + k)
        chi3_e(df) = chi3(map_chi(df) + k)
      enddo
      call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi1_e(:), chi2_e(:), chi3_e(:), ipanel, chi_basis(:,:,:,:), &
&chi_diff_basis(:,:,:,:), jac(:,:,:,:), dj(:,:))
      do df = 1, ndf_w3, 1
        ls_rho_e(df) = ls_rho(map_w3(df) + k)
      enddo
      do df = 1, ndf_wt, 1
        ls_theta_vd_e(df) = ls_moist_dyn_gas(k + map_wt(df)) * ls_theta(k + map_wt(df))
      enddo
      ik = cell * nlayers + k - nlayers + 1
      do df = ndf_w3, 1, -1
        exner_e(df) = exner_e(df) + exner(map_w3(df) + k)
        exner(map_w3(df) + k) = 0.0
      enddo
      do i = ndf_w3, 1, -1
        do j = ndf_w3, 1, -1
          r_exner(j) = r_exner(j) + m3_inv(i,j,ik) * exner_e(i)
        enddo
        exner_e(i) = 0.0
      enddo
      do qp2 = nqp_v, 1, -1
        do qp1 = nqp_h, 1, -1
          ls_rho_at_quad = 0.0_r_def
          do df = 1, ndf_w3, 1
            ls_rho_at_quad = ls_rho_at_quad + ls_rho_e(df) * w3_basis(1,df,qp1,qp2)
          enddo
          ls_theta_vd_at_quad = 0.0_r_def
          do df = 1, ndf_wt, 1
            ls_theta_vd_at_quad = ls_theta_vd_at_quad + ls_theta_vd_e(df) * wt_basis(1,df,qp1,qp2)
          enddo
          tmp_ls_exner = (ls_rho_at_quad * ls_theta_vd_at_quad * rd / p_zero) ** (kappa / (1.0 - kappa))
          do df = ndf_w3, 1, -1
            exner_at_quad = exner_at_quad + r_exner(df) * w3_basis(1,df,qp1,qp2)
          enddo
          tmp_exner = tmp_exner + exner_at_quad * dj(qp1,qp2) * wqp_h(qp1) * wqp_v(qp2)
          exner_at_quad = 0.0
          rho_at_quad = rho_at_quad + kappa * tmp_exner * tmp_ls_exner / (-kappa * ls_rho_at_quad + 1.0 * ls_rho_at_quad)
          theta_vd_at_quad = theta_vd_at_quad + kappa * tmp_exner * tmp_ls_exner / (-kappa * ls_theta_vd_at_quad + 1.0 * &
&ls_theta_vd_at_quad)
          tmp_exner = 0.0
          do df = ndf_wt, 1, -1
            theta_vd_e(df) = theta_vd_e(df) + theta_vd_at_quad * wt_basis(1,df,qp1,qp2)
          enddo
          theta_vd_at_quad = 0.0
          do df = ndf_w3, 1, -1
            rho_e(df) = rho_e(df) + rho_at_quad * w3_basis(1,df,qp1,qp2)
          enddo
          rho_at_quad = 0.0
        enddo
      enddo
      do df = ndf_wt, 1, -1
        theta(k + map_wt(df)) = theta(k + map_wt(df)) + ls_moist_dyn_gas(k + map_wt(df)) * theta_vd_e(df)
        moist_dyn_gas(k + map_wt(df)) = moist_dyn_gas(k + map_wt(df)) + ls_theta(k + map_wt(df)) * theta_vd_e(df)
        theta_vd_e(df) = 0.0
      enddo
      do df = ndf_w3, 1, -1
        r_exner(df) = 0.0
        rho(map_w3(df) + k) = rho(map_w3(df) + k) + rho_e(df)
        rho_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_project_eos_pressure_code

end module adj_project_eos_pressure_kernel_mod
