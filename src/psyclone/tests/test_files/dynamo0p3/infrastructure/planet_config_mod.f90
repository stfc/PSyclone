!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the planet namelist.
!>
module planet_config_mod

  use constants_mod, only: i_native, &
                           r_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO
  use mpi_mod,       only: broadcast

  use constants_mod, only: cmdi, emdi, imdi, rmdi, unset_key
  use driver_water_constants_mod, only: gas_constant_h2o

  implicit none

  private
  public :: read_planet_namelist, postprocess_planet_namelist, &
            planet_is_loadable, planet_is_loaded, planet_final

  real(r_def), public, protected :: cp = rmdi
  real(r_def), public, protected :: cv = rmdi
  real(r_def), public, protected :: epsilon = rmdi
  real(r_def), public, protected :: gravity = rmdi
  real(r_def), public, protected :: kappa = rmdi
  real(r_def), public, protected :: omega = rmdi
  real(r_def), public, protected :: one_over_kappa = rmdi
  real(r_def), public, protected :: p_zero = rmdi
  real(r_def), public, protected :: radius = rmdi
  real(r_def), public, protected :: rd = rmdi
  real(r_def), public, protected :: recip_epsilon = rmdi
  real(r_def), public, protected :: scaled_omega = rmdi
  real(r_def), public, protected :: scaled_radius = rmdi
  real(r_def), public, protected :: scaling_factor = rmdi

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_planet_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank )

  end subroutine read_planet_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank )

    use constants_mod, only: i_def, r_def

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank
    integer(i_def)                :: missing_data

    real(r_def) :: buffer_real_r_def(7)

    namelist /planet/ cp, &
                      gravity, &
                      omega, &
                      p_zero, &
                      radius, &
                      rd, &
                      scaling_factor

    integer(i_native) :: condition

    missing_data = 0

    cp = rmdi
    cv = rmdi
    epsilon = rmdi
    gravity = rmdi
    kappa = rmdi
    omega = rmdi
    one_over_kappa = rmdi
    p_zero = rmdi
    radius = rmdi
    rd = rmdi
    recip_epsilon = rmdi
    scaled_omega = rmdi
    scaled_radius = rmdi
    scaling_factor = rmdi

    if (local_rank == 0) then

      read( file_unit, nml=planet, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = cp
    buffer_real_r_def(2) = gravity
    buffer_real_r_def(3) = omega
    buffer_real_r_def(4) = p_zero
    buffer_real_r_def(5) = radius
    buffer_real_r_def(6) = rd
    buffer_real_r_def(7) = scaling_factor

    call broadcast( buffer_real_r_def, 7, 0 )

    cp = buffer_real_r_def(1)
    gravity = buffer_real_r_def(2)
    omega = buffer_real_r_def(3)
    p_zero = buffer_real_r_def(4)
    radius = buffer_real_r_def(5)
    rd = buffer_real_r_def(6)
    scaling_factor = buffer_real_r_def(7)

   ! Parameter name cv: dereferenced_list_vars are: ['cp', 'rd']
    missing_data = 0
    if (kind(cp) == r_def) then
       if (real(cp, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(cp) == i_def) then
       if (int(cp, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if (kind(rd) == r_def) then
       if (real(rd, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(rd) == i_def) then
       if (int(rd, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       cv = rmdi
    else
       cv = cp - rd
    end if
   ! Parameter name epsilon: dereferenced_list_vars are: ['rd']
    missing_data = 0
    if (kind(rd) == r_def) then
       if (real(rd, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(rd) == i_def) then
       if (int(rd, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       epsilon = rmdi
    else
       epsilon = rd / gas_constant_h2o
    end if
   ! Parameter name kappa: dereferenced_list_vars are: ['rd', 'cp']
    missing_data = 0
    if (kind(rd) == r_def) then
       if (real(rd, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(rd) == i_def) then
       if (int(rd, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if (kind(cp) == r_def) then
       if (real(cp, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(cp) == i_def) then
       if (int(cp, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       kappa = rmdi
    else
       kappa = rd / cp
    end if
   ! Parameter name one_over_kappa: dereferenced_list_vars are: ['cp', 'rd']
    missing_data = 0
    if (kind(cp) == r_def) then
       if (real(cp, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(cp) == i_def) then
       if (int(cp, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if (kind(rd) == r_def) then
       if (real(rd, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(rd) == i_def) then
       if (int(rd, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       one_over_kappa = rmdi
    else
       one_over_kappa = cp / rd
    end if
   ! Parameter name recip_epsilon: dereferenced_list_vars are: ['rd']
    missing_data = 0
    if (kind(rd) == r_def) then
       if (real(rd, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(rd) == i_def) then
       if (int(rd, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       recip_epsilon = rmdi
    else
       recip_epsilon = gas_constant_h2o / rd
    end if
   ! Parameter name scaled_omega: dereferenced_list_vars are: ['omega', 'scaling_factor']
    missing_data = 0
    if (kind(omega) == r_def) then
       if (real(omega, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(omega) == i_def) then
       if (int(omega, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if (kind(scaling_factor) == r_def) then
       if (real(scaling_factor, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(scaling_factor) == i_def) then
       if (int(scaling_factor, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       scaled_omega = rmdi
    else
       scaled_omega = omega * scaling_factor
    end if
   ! Parameter name scaled_radius: dereferenced_list_vars are: ['radius', 'scaling_factor']
    missing_data = 0
    if (kind(radius) == r_def) then
       if (real(radius, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(radius) == i_def) then
       if (int(radius, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if (kind(scaling_factor) == r_def) then
       if (real(scaling_factor, r_def) == rmdi) missing_data = missing_data + 1
    else if (kind(scaling_factor) == i_def) then
       if (int(scaling_factor, i_def)  == imdi) missing_data = missing_data + 1
    end if
    if ( missing_data >=1 ) then
       scaled_radius = rmdi
    else
       scaled_radius = radius / scaling_factor
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_planet_namelist()

    implicit none


  end subroutine postprocess_planet_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function planet_is_loadable()

    implicit none

    logical :: planet_is_loadable

    planet_is_loadable = .not. namelist_loaded

  end function planet_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function planet_is_loaded()

    implicit none

    logical :: planet_is_loaded

    planet_is_loaded = namelist_loaded

  end function planet_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine planet_final()

    implicit none

    cp = real(rmdi,r_def)
    cv = real(rmdi,r_def)
    epsilon = real(rmdi,r_def)
    gravity = real(rmdi,r_def)
    kappa = real(rmdi,r_def)
    omega = real(rmdi,r_def)
    one_over_kappa = real(rmdi,r_def)
    p_zero = real(rmdi,r_def)
    radius = real(rmdi,r_def)
    rd = real(rmdi,r_def)
    recip_epsilon = real(rmdi,r_def)
    scaled_omega = real(rmdi,r_def)
    scaled_radius = real(rmdi,r_def)
    scaling_factor = real(rmdi,r_def)

    return
  end subroutine planet_final


end module planet_config_mod
