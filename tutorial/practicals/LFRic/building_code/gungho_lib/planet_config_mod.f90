!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
!> Manages the planet namelist.
!>
module planet_config_mod

  use constants_mod, only: i_native, &
                           r_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: read_planet_namelist, postprocess_planet_namelist, &
            planet_is_loadable, planet_is_loaded, planet_final

  real(r_def), public, protected :: cp
  real(r_def), public, protected :: cv
  real(r_def), public, protected :: epsilon
  real(r_def), public, protected :: gravity
  real(r_def), public, protected :: kappa
  real(r_def), public, protected :: omega
  real(r_def), public, protected :: p_zero
  real(r_def), public, protected :: radius
  real(r_def), public, protected :: rd
  real(r_def), public, protected :: recip_epsilon
  real(r_def), public, protected :: scaled_omega
  real(r_def), public, protected :: scaled_radius
  real(r_def), public, protected :: scaling_factor

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

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    real(r_def) :: buffer_real_r_def(7)

    namelist /planet/ cp, &
                      gravity, &
                      omega, &
                      p_zero, &
                      radius, &
                      rd, &
                      scaling_factor

    integer(i_native) :: condition
    real(r_def) :: gas_constant_h2o = 1.0_r_def

    cp = rmdi
    cv = rmdi
    epsilon = rmdi
    gravity = rmdi
    kappa = rmdi
    omega = rmdi
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

    cp = buffer_real_r_def(1)
    gravity = buffer_real_r_def(2)
    omega = buffer_real_r_def(3)
    p_zero = buffer_real_r_def(4)
    radius = buffer_real_r_def(5)
    rd = buffer_real_r_def(6)
    scaling_factor = buffer_real_r_def(7)

    if ( any([cp, rd] == imdi) .or. &
         any([cp, rd] == rmdi) ) then
      cv = rmdi
    else
      cv = cp - rd
    end if
    if ( any([rd] == imdi) .or. &
         any([rd] == rmdi) ) then
      epsilon = rmdi
    else
      epsilon = rd / gas_constant_h2o
    end if
    if ( any([rd, cp] == imdi) .or. &
         any([rd, cp] == rmdi) ) then
      kappa = rmdi
    else
      kappa = rd / cp
    end if
    if ( any([rd] == imdi) .or. &
         any([rd] == rmdi) ) then
      recip_epsilon = rmdi
    else
      recip_epsilon = gas_constant_h2o / rd
    end if
    if ( any([omega, scaling_factor] == imdi) .or. &
         any([omega, scaling_factor] == rmdi) ) then
      scaled_omega = rmdi
    else
      scaled_omega = omega * scaling_factor
    end if
    if ( any([radius, scaling_factor] == imdi) .or. &
         any([radius, scaling_factor] == rmdi) ) then
      scaled_radius = rmdi
    else
      scaled_radius = radius / scaling_factor
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_planet_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

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

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    cp = real(rmdi,r_def)
    cv = real(rmdi,r_def)
    epsilon = real(rmdi,r_def)
    gravity = real(rmdi,r_def)
    kappa = real(rmdi,r_def)
    omega = real(rmdi,r_def)
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
