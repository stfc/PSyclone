module adj_rhs_sample_eos_kernel_mod
  use argument_mod, only : arg_type, cell_column, func_type, gh_basis, &
       gh_evaluator, gh_field, gh_read, gh_real, gh_scalar, gh_write, gh_readwrite
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w3, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_rhs_sample_eos_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(12) = (/ &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, W3), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, Wtheta), &
       arg_type(GH_SCALAR, GH_REAL, GH_READ), &
       arg_type(GH_SCALAR, GH_REAL, GH_READ), &
       arg_type(GH_SCALAR, GH_REAL, GH_READ)/)
  TYPE(func_type) :: meta_funcs(2) = (/func_type(W3, GH_BASIS), &
                                       func_type(Wtheta, GH_BASIS)/)
  INTEGER :: operates_on = CELL_COLUMN
  INTEGER :: gh_shape = GH_EVALUATOR
  CONTAINS
  PROCEDURE, NOPASS :: adj_rhs_sample_eos_code
END TYPE
  private

  public :: adj_rhs_sample_eos_code

  contains
    subroutine adj_rhs_sample_eos_code( &
         nlayers, rhs_eos, exner, rho, theta, moist_dyn_gas, ls_exner, &
         ls_rho, ls_theta, ls_moist_dyn_gas, kappa, rd, p_zero, &
         ndf_w3, undf_w3, map_w3, w3_basis, basis_w3_on_wtheta, &
         ndf_wt, undf_wt, map_wt, wt_basis, basis_wtheta_on_wtheta)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    real(kind=r_def), dimension(1,ndf_w3,ndf_w3), intent(in) :: w3_basis
    real(kind=r_def), dimension(1,ndf_wt,ndf_w3), intent(in) :: wt_basis
    REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,ndf_wt) :: basis_w3_on_wtheta
    REAL(KIND=r_def), intent(in), dimension(1,ndf_wt,ndf_wt) :: basis_wtheta_on_wtheta
    real(kind=r_def), dimension(undf_w3), intent(inout) :: rhs_eos
    real(kind=r_def), dimension(undf_wt), intent(inout) :: theta
    real(kind=r_def), dimension(undf_wt), intent(inout) :: moist_dyn_gas
    real(kind=r_def), dimension(undf_w3), intent(inout) :: rho
    real(kind=r_def), dimension(undf_w3), intent(inout) :: exner
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_theta
    real(kind=r_def), dimension(undf_wt), intent(in) :: ls_moist_dyn_gas
    real(kind=r_def), dimension(undf_w3), intent(in) :: ls_rho
    real(kind=r_def), dimension(undf_w3), intent(in) :: ls_exner
    real(kind=r_def), intent(in) :: kappa
    real(kind=r_def), intent(in) :: rd
    real(kind=r_def), intent(in) :: p_zero
    integer(kind=i_def) :: df
    integer(kind=i_def) :: df3
    integer(kind=i_def) :: dft
    integer(kind=i_def) :: k
    real(kind=r_def), dimension(ndf_wt) :: theta_vd_e
    real(kind=r_def), dimension(ndf_wt) :: ls_theta_vd_e
    real(kind=r_def), dimension(ndf_w3) :: rho_e
    real(kind=r_def), dimension(ndf_w3) :: exner_e
    real(kind=r_def) :: rho_cell
    real(kind=r_def) :: theta_vd_cell
    real(kind=r_def) :: exner_cell
    real(kind=r_def), dimension(ndf_w3) :: ls_rho_e
    real(kind=r_def), dimension(ndf_w3) :: ls_exner_e
    real(kind=r_def) :: ls_rho_cell
    real(kind=r_def) :: ls_theta_vd_cell
    real(kind=r_def) :: ls_exner_cell
    real(kind=r_def) :: p0_over_rd
    real(kind=r_def) :: onemk_over_k
    real(kind=r_def) :: ls_eos

    exner_cell = 0.0_r_def
    theta_vd_cell = 0.0_r_def
    rho_cell = 0.0_r_def
    rho_e = 0.0_r_def
    exner_e = 0.0_r_def
    theta_vd_e = 0.0_r_def
    p0_over_rd = p_zero / rd
    onemk_over_k = -1 + 1.0 / kappa
    do k = nlayers - 1, 0, -1
      do df = 1, ndf_wt, 1
        ls_theta_vd_e(df) = ls_moist_dyn_gas(k + map_wt(df)) * ls_theta(k + map_wt(df))
      enddo
      do df = 1, ndf_w3, 1
        ls_exner_e(df) = ls_exner(map_w3(df) + k)
        ls_rho_e(df) = ls_rho(map_w3(df) + k)
      enddo
      do df = ndf_w3, 1, -1
        ls_theta_vd_cell = 0.0_r_def
        do dft = 1, ndf_wt, 1
          ls_theta_vd_cell = ls_theta_vd_cell + ls_theta_vd_e(dft) * wt_basis(1,dft,df)
        enddo
        ls_exner_cell = 0.0_r_def
        ls_rho_cell = 0.0_r_def
        do df3 = 1, ndf_w3, 1
          ls_exner_cell = ls_exner_cell + ls_exner_e(df3) * w3_basis(1,df3,df)
          ls_rho_cell = ls_rho_cell + ls_rho_e(df3) * w3_basis(1,df3,df)
        enddo
        ls_eos = -ls_exner_cell ** onemk_over_k * p0_over_rd / (ls_rho_cell * ls_theta_vd_cell)
        exner_cell = exner_cell + (-rhs_eos(map_w3(df) + k) * ls_eos * onemk_over_k / ls_exner_cell)
        theta_vd_cell = theta_vd_cell + ls_eos * rhs_eos(map_w3(df) + k) / ls_theta_vd_cell
        rho_cell = rho_cell + ls_eos * rhs_eos(map_w3(df) + k) / ls_rho_cell
        rhs_eos(map_w3(df) + k) = 0.0
        do df3 = ndf_w3, 1, -1
          rho_e(df3) = rho_e(df3) + rho_cell * w3_basis(1,df3,df)
          exner_e(df3) = exner_e(df3) + exner_cell * w3_basis(1,df3,df)
        enddo
        rho_cell = 0.0
        exner_cell = 0.0
        do dft = ndf_wt, 1, -1
          theta_vd_e(dft) = theta_vd_e(dft) + theta_vd_cell * wt_basis(1,dft,df)
        enddo
        theta_vd_cell = 0.0
      enddo
      do df = ndf_w3, 1, -1
        rhs_eos(map_w3(df) + k) = 0.0
        rho(map_w3(df) + k) = rho(map_w3(df) + k) + rho_e(df)
        rho_e(df) = 0.0
        exner(map_w3(df) + k) = exner(map_w3(df) + k) + exner_e(df)
        exner_e(df) = 0.0
      enddo
      do df = ndf_wt, 1, -1
        theta(k + map_wt(df)) = theta(k + map_wt(df)) + ls_moist_dyn_gas(k + map_wt(df)) * theta_vd_e(df)
        moist_dyn_gas(k + map_wt(df)) = moist_dyn_gas(k + map_wt(df)) + ls_theta(k + map_wt(df)) * theta_vd_e(df)
        theta_vd_e(df) = 0.0
      enddo
    enddo

  end subroutine adj_rhs_sample_eos_code

end module adj_rhs_sample_eos_kernel_mod
