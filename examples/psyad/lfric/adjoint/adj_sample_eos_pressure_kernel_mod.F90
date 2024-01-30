module adj_sample_eos_pressure_kernel_mod
  use argument_mod, only : arg_type, cell_column, func_type, gh_basis, gh_evaluator, gh_field, gh_read, gh_real, gh_readwrite
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w3, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_sample_eos_pressure_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(7) = (/ &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
       &arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta)/)
  TYPE(func_type) :: meta_funcs(2) = (/ &
       func_type(W3, GH_BASIS), func_type(Wtheta, GH_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_EVALUATOR
  CONTAINS
  PROCEDURE, NOPASS :: adj_sample_eos_pressure_code
END TYPE
  private

  public :: adj_sample_eos_pressure_code

  contains
    subroutine adj_sample_eos_pressure_code(&
         nlayers, exner, rho, theta, moist_dyn_gas, &
         ls_rho, ls_theta, ls_moist_dyn_gas, &
         ndf_w3, undf_w3, map_w3, w3_basis, basis_w3_on_wtheta, &
         ndf_wt, undf_wt, map_wt, wt_basis, basis_wtheta_on_wtheta)
    use planet_config_mod, only : kappa, Rd, p_zero
    use coordinate_jacobian_mod, only : coordinate_jacobian
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    real(kind=r_def), dimension(1,ndf_w3,ndf_w3), intent(in) :: w3_basis
    real(kind=r_def), dimension(1,ndf_wt,ndf_w3), intent(in) :: wt_basis
    REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,ndf_wt) :: basis_w3_on_wtheta
    REAL(KIND=r_def), intent(in), dimension(1,ndf_wt,ndf_wt) :: basis_wtheta_on_wtheta
    real(kind=r_def), dimension(undf_w3), intent(inout) :: exner
    real(kind=r_def), dimension(undf_w3), intent(inout) :: rho
    real(kind=r_def), dimension(undf_wt), intent(inout) :: theta
    real(kind=r_def), dimension(undf_wt), intent(inout) :: moist_dyn_gas
    real(kind=r_def), dimension(undf_w3), intent(in) :: ls_rho
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_theta
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_moist_dyn_gas
    integer(kind=i_def) :: df
    integer(kind=i_def) :: df3
    integer(kind=i_def) :: dft
    integer(kind=i_def) :: k
    real(kind=r_def), dimension(ndf_w3) :: rho_e
    real(kind=r_def), dimension(ndf_wt) :: theta_vd_e
    real(kind=r_def), dimension(ndf_w3) :: ls_rho_e
    real(kind=r_def), dimension(ndf_wt) :: ls_theta_vd_e
    real(kind=r_def) :: rho_cell
    real(kind=r_def) :: theta_vd_cell
    real(kind=r_def) :: ls_rho_cell
    real(kind=r_def) :: ls_theta_vd_cell
    real(kind=r_def) :: tmp_ls_exner
    real(kind=r_def) :: tmp_exner

    rho_cell = 0.0_r_def
    rho_e = 0.0_r_def
    tmp_exner = 0.0_r_def
    theta_vd_cell = 0.0_r_def
    theta_vd_e = 0.0_r_def
    do k = nlayers - 1, 0, -1
      do df = 1, ndf_w3, 1
        ls_rho_e(df) = ls_rho(map_w3(df) + k)
      enddo
      do df = 1, ndf_wt, 1
        ls_theta_vd_e(df) = ls_moist_dyn_gas(k + map_wt(df)) * ls_theta(k + map_wt(df))
      enddo
      do df = ndf_w3, 1, -1
        ls_rho_cell = 0.0_r_def
        do df3 = 1, ndf_w3, 1
          ls_rho_cell = ls_rho_cell + ls_rho_e(df3) * w3_basis(1,df3,df)
        enddo
        ls_theta_vd_cell = 0.0_r_def
        do dft = 1, ndf_wt, 1
          ls_theta_vd_cell = ls_theta_vd_cell + ls_theta_vd_e(dft) * wt_basis(1,dft,df)
        enddo
        tmp_ls_exner = (ls_rho_cell * ls_theta_vd_cell * rd / p_zero) ** (kappa / (1.0 - kappa))
        tmp_exner = tmp_exner + exner(map_w3(df) + k)
        exner(map_w3(df) + k) = 0.0
        rho_cell = rho_cell + kappa * tmp_exner * tmp_ls_exner / (-kappa * ls_rho_cell + 1.0 * ls_rho_cell)
        theta_vd_cell = theta_vd_cell + kappa * tmp_exner * tmp_ls_exner / (-kappa * ls_theta_vd_cell + 1.0 * ls_theta_vd_cell)
        tmp_exner = 0.0
        do dft = ndf_wt, 1, -1
          theta_vd_e(dft) = theta_vd_e(dft) + theta_vd_cell * wt_basis(1,dft,df)
        enddo
        theta_vd_cell = 0.0
        do df3 = ndf_w3, 1, -1
          rho_e(df3) = rho_e(df3) + rho_cell * w3_basis(1,df3,df)
        enddo
        rho_cell = 0.0
      enddo
      do df = ndf_wt, 1, -1
        theta(k + map_wt(df)) = theta(k + map_wt(df)) + ls_moist_dyn_gas(k + map_wt(df)) * theta_vd_e(df)
        moist_dyn_gas(k + map_wt(df)) = moist_dyn_gas(k + map_wt(df)) + ls_theta(k + map_wt(df)) * theta_vd_e(df)
        theta_vd_e(df) = 0.0
      enddo
      do df = ndf_w3, 1, -1
        rho(map_w3(df) + k) = rho(map_w3(df) + k) + rho_e(df)
        rho_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_sample_eos_pressure_code

end module adj_sample_eos_pressure_kernel_mod
