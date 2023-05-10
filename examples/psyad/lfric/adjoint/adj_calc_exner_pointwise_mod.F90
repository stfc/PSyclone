module adj_calc_exner_pointwise_mod
  use planet_config_mod, only : kappa, Rd, p_zero
  use constants_mod, only : r_def
  implicit none
  private

  public :: adj_calc_exner_pointwise

  contains
  subroutine adj_calc_exner_pointwise(rho, theta, ls_rho, ls_theta, exner)
    real(kind=r_def), intent(inout) :: exner
    real(kind=r_def), intent(inout) :: rho
    real(kind=r_def), intent(inout) :: theta
    real(kind=r_def), intent(in) :: ls_rho
    real(kind=r_def), intent(in) :: ls_theta
    real(kind=r_def) :: ls_exner

    ls_exner = (ls_rho * ls_theta * rd / p_zero) ** (kappa / (1.0 - kappa))
    rho = rho + kappa * ls_exner * exner / (-kappa * ls_rho + 1.0 * ls_rho)
    theta = theta + kappa * ls_exner * exner / (-kappa * ls_theta + 1.0 * ls_theta)
    exner = 0.0

  end subroutine adj_calc_exner_pointwise

end module adj_calc_exner_pointwise_mod
