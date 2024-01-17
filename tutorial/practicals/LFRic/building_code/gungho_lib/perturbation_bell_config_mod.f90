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
!> Manages the perturbation_bell namelist.
!>
module perturbation_bell_config_mod

  use constants_mod, only: i_native, &
                           r_def, &
                           str_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: read_perturbation_bell_namelist, postprocess_perturbation_bell_namelist, &
            perturbation_bell_is_loadable, perturbation_bell_is_loaded, perturbation_bell_final

  real(r_def), public, protected :: half_width_x
  real(r_def), public, protected :: half_width_y
  real(r_def), public, protected :: perturbation_scale
  real(r_def), public, protected :: perturbation_height
  real(r_def), public, protected :: x_centre
  real(r_def), public, protected :: y_centre
  real(r_def), public, protected :: u_vel
  real(r_def), public, protected :: v_vel

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_perturbation_bell_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank )

  end subroutine read_perturbation_bell_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank )

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    real(r_def) :: buffer_real_r_def(8)

    namelist /perturbation_bell/ half_width_x, &
                                 half_width_y, &
                                 perturbation_scale, &
                                 perturbation_height, &
                                 x_centre, &
                                 y_centre, &
                                 u_vel, &
                                 v_vel

    integer(i_native) :: condition

    half_width_x = rmdi
    half_width_y = rmdi
    perturbation_scale = rmdi
    perturbation_height = rmdi
    x_centre = rmdi
    y_centre = rmdi
    u_vel = rmdi
    v_vel = rmdi

    if (local_rank == 0) then

      read( file_unit, nml=perturbation_bell, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = half_width_x
    buffer_real_r_def(2) = half_width_y
    buffer_real_r_def(3) = perturbation_scale
    buffer_real_r_def(4) = perturbation_height
    buffer_real_r_def(5) = x_centre
    buffer_real_r_def(6) = y_centre
    buffer_real_r_def(7) = u_vel
    buffer_real_r_def(8) = v_vel

    half_width_x = buffer_real_r_def(1)
    half_width_y = buffer_real_r_def(2)
    perturbation_scale = buffer_real_r_def(3)
    perturbation_height = buffer_real_r_def(4)
    x_centre = buffer_real_r_def(5)
    y_centre = buffer_real_r_def(6)
    u_vel = buffer_real_r_def(7)
    v_vel = buffer_real_r_def(8)

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_perturbation_bell_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none


  end subroutine postprocess_perturbation_bell_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function perturbation_bell_is_loadable()

    implicit none

    logical :: perturbation_bell_is_loadable

    perturbation_bell_is_loadable = .not. namelist_loaded

  end function perturbation_bell_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function perturbation_bell_is_loaded()

    implicit none

    logical :: perturbation_bell_is_loaded

    perturbation_bell_is_loaded = namelist_loaded

  end function perturbation_bell_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine perturbation_bell_final()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    half_width_x = real(rmdi,r_def)
    half_width_y = real(rmdi,r_def)
    perturbation_scale = real(rmdi,r_def)
    perturbation_height = real(rmdi,r_def)
    x_centre = real(rmdi,r_def)
    y_centre = real(rmdi,r_def)
    u_vel = real(rmdi,r_def)
    v_vel = real(rmdi,r_def)

    return
  end subroutine perturbation_bell_final


end module perturbation_bell_config_mod
