module exner_adj_test_mod
  use tl_calc_exner_pointwise_mod, only : tl_calc_exner_pointwise
  use adj_calc_exner_pointwise_mod, only : adj_calc_exner_pointwise
  use constants_mod, only : r_def

contains

  subroutine exner_adj_test()

  integer, parameter :: array_extent = 20
  real(kind=r_def), parameter :: overall_tolerance = 1500.0_r_def
  real(kind=r_def) :: inner1
  real(kind=r_def) :: inner2
  real(kind=r_def) :: rho
  real(kind=r_def) :: rho_input
  real(kind=r_def) :: theta
  real(kind=r_def) :: theta_input
  real(kind=r_def) :: ls_rho
  real(kind=r_def) :: ls_rho_input
  real(kind=r_def) :: ls_theta
  real(kind=r_def) :: ls_theta_input
  real(kind=r_def) :: exner
  real(kind=r_def) :: exner_input
  real(kind=r_def) :: MachineTol
  real(kind=r_def) :: relative_diff

  ! Initialise the kernel arguments and keep copies of them
  call random_number(rho)
  rho_input = rho
  call random_number(theta)
  theta_input = theta
  call random_number(ls_rho)
  ls_rho_input = ls_rho
  call random_number(ls_theta)
  ls_theta_input = ls_theta
  call random_number(exner)
  exner_input = exner
  ! Call the tangent-linear kernel
  call tl_calc_exner_pointwise(rho, theta, ls_rho, ls_theta, exner)
  ! Compute the inner product of the results of the tangent-linear kernel
  inner1 = 0.0_r_def
  inner1 = inner1 + rho * rho
  inner1 = inner1 + theta * theta
  inner1 = inner1 + ls_rho * ls_rho
  inner1 = inner1 + ls_theta * ls_theta
  inner1 = inner1 + exner * exner
  ! Call the adjoint of the kernel
  call adj_calc_exner_pointwise(rho, theta, ls_rho, ls_theta, exner)
  ! Compute inner product of results of adjoint kernel with the original inputs to the tangent-linear kernel
  inner2 = 0.0_r_def
  inner2 = inner2 + rho * rho_input
  inner2 = inner2 + theta * theta_input
  inner2 = inner2 + ls_rho * ls_rho_input
  inner2 = inner2 + ls_theta * ls_theta_input
  inner2 = inner2 + exner * exner_input
  ! Test the inner-product values for equality, allowing for the precision of the active variables
  MachineTol = SPACING(MAX(ABS(inner1), ABS(inner2)))
  relative_diff = ABS(inner1 - inner2) / MachineTol
  if (relative_diff < overall_tolerance) then
    WRITE(*, *) 'Test of adjoint of ''tl_calc_exner_pointwise'' PASSED: ', inner1, inner2, relative_diff
  else
    WRITE(*, *) 'Test of adjoint of ''tl_calc_exner_pointwise'' FAILED: ', inner1, inner2, relative_diff
  end if
end subroutine exner_adj_test
end module exner_adj_test_mod
