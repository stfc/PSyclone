!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

! This tweaked version of the code 1) modifies the code into a
! subroutine and 2) replaces kappa, Rd and P_zero with typed
! declarations so PSyAD knows what type they are, rather than being
! included from a use statement.

module tl_calc_exner_pointwise_mod

use constants_mod,     only : r_def
!RF use planet_config_mod, only : kappa, Rd, p_zero

implicit none

private

public :: tl_calc_exner_pointwise

contains
!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------

!> @brief Function to compute the change in exner pressure from the tangent
!>        linear of the equation of state.
!> @details The nonlinear equation of state is:
!>          exner = ( Rd/p0 * rho * theta ) ^ (  k / ( 1 - k ) )
!>          The tangent linear is:
!>          exner = (k/(1-k)) * ls_exner * ( rho / ls_rho + theta / ls_theta )
!! @param[in] rho      Change in density
!! @param[in] theta    Change in potential temperature
!! @param[in] ls_rho   Linearisation state for density
!! @param[in] ls_theta Linearisation state for potential temperature
!! @return    exner    Change in pressure
subroutine tl_calc_exner_pointwise(rho, theta, ls_rho, ls_theta, exner)

  implicit none

  real(kind=r_def), intent(out) :: exner
  real(kind=r_def), intent(in)  :: rho, theta
  real(kind=r_def), intent(in)  :: ls_rho, ls_theta
  real(kind=r_def)              :: ls_exner

  real(kind=r_def) ::  kappa, Rd, p_zero

  ls_exner = ( ( Rd / p_zero ) * ls_rho * ls_theta ) ** &
             ( kappa / ( 1.0_r_def - kappa ) )

  exner = ( kappa / ( 1.0_r_def - kappa ) ) * ls_exner * &
          ( ( rho / ls_rho ) + ( theta / ls_theta )  )

end subroutine tl_calc_exner_pointwise

end module tl_calc_exner_pointwise_mod
