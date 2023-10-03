!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Tangent linear for computing the sampling of the pressure field
!!        into the same space as density.

! This tweaked version of the code 1) removes the use of
! tl_calc_exner_pointwise and 2) replaces kappa, Rd and P_zero with
! typed declarations so PSyAD knows what type they are, rather than
! being included from a use statement.

module tl_sample_eos_pressure_kernel_mod

  use argument_mod,      only : arg_type, func_type,       &
                                GH_FIELD,                  &
                                GH_READ, GH_WRITE,         &
                                GH_REAL, GH_BASIS,         &
                                CELL_COLUMN, GH_EVALUATOR
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
  type, public, extends(kernel_type) :: tl_sample_eos_pressure_kernel_type
    private
    type(arg_type) :: meta_args(7) = (/                                      &
         arg_type(GH_FIELD,    GH_REAL, GH_WRITE, W3),                        &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W3),                        &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),                    &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta)                    &
         /)
    type(func_type) :: meta_funcs(2) = (/                                     &
         func_type(W3,          GH_BASIS),                                    &
         func_type(Wtheta,      GH_BASIS)                                     &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_EVALUATOR
  contains
    procedure, nopass :: tl_sample_eos_pressure_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: tl_sample_eos_pressure_code

contains

!> @brief Compute the tangent linear for sample pressure
!! @param[in] nlayers          Number of layers
!! @param[in,out] exner        Change in pressure field
!! @param[in] rho              Change in density
!! @param[in] theta            Change in potential temperature
!! @param[in] moist_dyn_gas    Change in moist dynamics factor
!! @param[in] ls_rho           Lin state for density
!! @param[in] ls_theta         Lin state for potential temperature
!! @param[in] ls_moist_dyn_gas Lin state for moist dynamics factor
!! @param[in] ndf_w3           Number of degrees of freedom per cell for w3
!! @param[in] undf_w3          Number of unique degrees of freedom for w3
!! @param[in] map_w3           Dofmap for the cell at the base of the column for w3
!! @param[in] w3_basis         Basis functions evaluated at Gaussian quadrature points
!! @param[in] ndf_wt           Number of degrees of freedom per cell for theta space
!! @param[in] undf_wt          Number of unique degrees of freedom for theta space
!! @param[in] map_wt           Dofmap for the cell at the base of the column for theta space
!! @param[in] wt_basis         Basis functions evaluated at Gaussian quadrature points
subroutine tl_sample_eos_pressure_code(nlayers,                                &
                                 exner, rho, theta, moist_dyn_gas,             &
                                 ls_rho, ls_theta, ls_moist_dyn_gas,           &
                                 ndf_w3, undf_w3, map_w3, w3_basis,            &
                                 ndf_wt, undf_wt, map_wt, wt_basis             &
                                 )

  !RF use tl_calc_exner_pointwise_mod,only: tl_calc_exner_pointwise
  use coordinate_jacobian_mod, only: coordinate_jacobian

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: ndf_wt, ndf_w3
  integer(kind=i_def), intent(in) :: undf_wt, undf_w3
  integer(kind=i_def), dimension(ndf_wt),  intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_w3),  intent(in) :: map_w3

  real(kind=r_def), dimension(1,ndf_w3, ndf_w3), intent(in) :: w3_basis
  real(kind=r_def), dimension(1,ndf_wt, ndf_w3), intent(in) :: wt_basis

  real(kind=r_def), dimension(undf_w3),  intent(inout) :: exner
  real(kind=r_def), dimension(undf_w3),  intent(in)    :: rho
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: theta
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: moist_dyn_gas
  real(kind=r_def), dimension(undf_w3),  intent(in)    :: ls_rho
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: ls_theta
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: ls_moist_dyn_gas

  ! Internal variables
  integer(kind=i_def) :: df, df3, dft, k

  real(kind=r_def), dimension(ndf_w3)          :: rho_e
  real(kind=r_def), dimension(ndf_wt)          :: theta_vd_e
  real(kind=r_def), dimension(ndf_w3)          :: ls_rho_e
  real(kind=r_def), dimension(ndf_wt)          :: ls_theta_vd_e

  real(kind=r_def) :: rho_cell, theta_vd_cell
  real(kind=r_def) :: ls_rho_cell, ls_theta_vd_cell

  !RF use planet_config_mod, only : kappa, Rd, p_zero
  real(kind=r_def) :: kappa, Rd, p_zero
  real(kind=r_def) :: tmp_ls_exner, tmp_exner

  do k = 0, nlayers-1
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
    end do
    do df = 1, ndf_wt
      theta_vd_e(df) = ( ls_theta( map_wt(df) + k )        &
                     * moist_dyn_gas( map_wt(df) + k ) )   &
                     + ( theta( map_wt(df) + k )           &
                     * ls_moist_dyn_gas( map_wt(df) + k ) )
    end do
    do df = 1, ndf_w3

      ! Linearisation state
      ls_rho_cell = 0.0_r_def
      do df3 = 1, ndf_w3
        ls_rho_cell  = ls_rho_cell + ls_rho_e(df3)*w3_basis(1,df3,df)
      end do
      ls_theta_vd_cell = 0.0_r_def
      do dft = 1, ndf_wt
          ls_theta_vd_cell = ls_theta_vd_cell + ls_theta_vd_e(dft)*wt_basis(1,dft,df)
      end do

      ! Perturbation
      rho_cell = 0.0_r_def
      do df3 = 1, ndf_w3
        rho_cell  = rho_cell + rho_e(df3)*w3_basis(1,df3,df)
      end do
      theta_vd_cell = 0.0_r_def
      do dft = 1, ndf_wt
        theta_vd_cell = theta_vd_cell + theta_vd_e(dft)*wt_basis(1,dft,df)
      end do

      ! Calculation
      tmp_ls_exner = ( ( Rd / p_zero ) * ls_rho_cell * ls_theta_vd_cell ) ** &
             ( kappa / ( 1.0_r_def - kappa ) )

      tmp_exner = ( kappa / ( 1.0_r_def - kappa ) ) * tmp_ls_exner * &
          ( ( rho_cell / ls_rho_cell ) + ( theta_vd_cell / ls_theta_vd_cell )  )

      exner(map_w3(df)+k) = tmp_exner

    end do
  end do

end subroutine tl_sample_eos_pressure_code

end module tl_sample_eos_pressure_kernel_mod
