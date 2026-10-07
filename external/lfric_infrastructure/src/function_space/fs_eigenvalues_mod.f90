










!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Provide access to the eigenvalue bounds for mass matrices
!>        defined on given function spaces

module fs_eigenvalues_mod
  use constants_mod, only: i_def, r_def
  use log_mod,       only: log_event,      &
                           LOG_LEVEL_ERROR

  implicit none

  private
  real(r_def), parameter :: w0_lmin = 0.124_r_def
  real(r_def), parameter :: w0_lmax = 3.377_r_def
  real(r_def), parameter :: w1_lmin = 0.249_r_def
  real(r_def), parameter :: w1_lmax = 3.226_r_def
  real(r_def), parameter :: w2_lmin = 0.498_r_def
  real(r_def), parameter :: w2_lmax = 2.151_r_def
  real(r_def), parameter :: w2v_lmin = 0.500_r_def
  real(r_def), parameter :: w2v_lmax = 1.500_r_def
  real(r_def), parameter :: wt_lmin = 0.499_r_def
  real(r_def), parameter :: wt_lmax = 1.501_r_def

  public :: get_minmax_eigenvalues

contains

  !>@brief Return the min & max eigenvalues for a mass matrix on a given space
  !>@details The bounds of mass matrix eigenvalues are relatively invariant with
  !>         resolution and are can be approximately represented by the
  !>         eigenvalues of the element matrices.
  !>         Therefore this function returns precomputed values based
  !>         upon offline computations using low resolution meshes.
  !>         It would be possible to compute these here using a given mass
  !>         matrix but this introduces an avoidable expense
  !>@param[out] lmin Bound on the minimum eigenvalue
  !>@param[out] lmax Bound on the maximum eigenvalue
  !>@param[in]  function_space Requested function space for the eigenvalues
  subroutine get_minmax_eigenvalues(lmin, lmax, function_space)

    use fs_continuity_mod, only: W0, W1, W2, W3, Wtheta, W2v, W2h

    implicit none

    real(r_def),    intent(out) :: lmin, lmax
    integer(i_def), intent(in)  :: function_space

    select case ( function_space )
      case ( W0 )
            lmin = w0_lmin
            lmax = w0_lmax
      case ( W1 )
            lmin = w1_lmin
            lmax = w1_lmax
      case ( W2, W2h )
            lmin = w2_lmin
            lmax = w2_lmax
      case ( W2v )
            lmin = w2v_lmin
            lmax = w2v_lmax
      case ( Wtheta )
            lmin = wt_lmin
            lmax = wt_lmax
      case default
        call log_event('Eigenvalues not stored for this function space', LOG_LEVEL_ERROR )
    end select

  end subroutine get_minmax_eigenvalues



end module fs_eigenvalues_mod
