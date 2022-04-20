!----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!----------------------------------------------------------------------------
!> @brief LFRic water constants module
!----------------------------------------------------------------------------

module driver_water_constants_mod

!> @todo This module contains water related variables that "should" be
!>       universally constant. It should also be set at the top-level driver
!>       (i.e. not specified by any specific scheme). However, there is
!>       currently no driver for LFRic, the driver functionality being spread
!>       across lfric_atm/gungho code. Since gas_constant_h2o is required
!>       to derived a consistent epsilon for MoistDynamics and UM Physics,
!>       gas_constant_h2o, and thus this module must be available when either
!>       gungho or lfric_atm is built.
!>
!>       Once a driver has been established for the LFRic project this module
!>       should be relocated to it's top-level equivalent.
!>
!>       In addition, while this module mirrors water_constants in the UM,
!>       some variables would technically be Planet dependant, i.e. sea-related
!>       variables. Given the UM source code this came from did not deem that
!>       a requirement, a resolution for this issue (if any) should be science
!>       led.

  use constants_mod, only : r_def

  implicit none

  private
  public :: T_freeze_h2o_sea, T_freeze_h2o, density_h2o,                &
            density_h2o_sea, density_ice, latent_heat_h2o_condensation, &
            latent_heat_h2o_fusion, heat_capacity_h2o_vapour,           &
            heat_capacity_h2o, heat_capacity_ice, gas_constant_h2o,     &
            jules_dpsidt

  !> @name Temperature, [K]
  !> @{
  real(r_def), parameter :: T_freeze_h2o_sea =271.35_r_def
  !< Temperature at which sea water freezes, [K]
  real(r_def), parameter :: T_freeze_h2o = 273.15_r_def
  !< Temperature at which fresh water freezes and ice melts, [K]
  !> @}

  !> @name Density, [kg/m^3]
  !> @{
  real(r_def), parameter :: density_h2o = 1000.0_r_def
  !< Density of water [kg/m^3]
  real(r_def), parameter :: density_h2o_sea = 1026.0_r_def
  !< Density of sea water [kg/m^3]
  real(r_def), parameter :: density_ice = 917.0_r_def
  !< Density of ice [kg/m^3]
  !> @}

  !> @name Specific Latent Heat at 0 degC, [J/kg]
  !> @{
  real(r_def), parameter :: latent_heat_h2o_condensation = 2.501e6_r_def
  !< Specific Latent heat of condensation of water at 0 degC [J/kg]

  real(r_def), parameter :: latent_heat_h2o_fusion = 0.334e6_r_def
  !< Specific Latent heat of fusion of water at 0 degC [J/kg]
  !> @}

  !> @name Specific Heat Capacity, [J/kg]
  !> @{
  real(r_def), parameter :: heat_capacity_h2o_vapour = 1850.0_r_def
  !< Specific heat capacity of water vapour, [J/(kg K)]

  real(r_def), parameter :: heat_capacity_h2o = 4180.0_r_def
  !< Specific heat capacity of water, [J/(kg K)]

  real(r_def), parameter :: heat_capacity_ice = 2100.0_r_def
  !< Specific heat capacity of ice [J/(kg K)]
  !> @}

  real(r_def), parameter :: gas_constant_h2o = 461.51_r_def
  !< Gas constant of water [J/(mol K)]

  real(r_def), parameter :: jules_dpsidt = 114.3_r_def
  !< Rate of change of soil matrix potential with temperature at
  !< equilibrium between water and ice in partially frozen soil [m/K].

end module driver_water_constants_mod
