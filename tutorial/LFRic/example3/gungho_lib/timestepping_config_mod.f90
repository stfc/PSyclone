!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the timestepping namelist.
!>
module timestepping_config_mod

  use constants_mod, only: i_def, &
                           i_native, &
                           r_def
  use log_mod,       only: log_event, log_scratch_space &
                         , LOG_LEVEL_ERROR, LOG_LEVEL_WARNING, LOG_LEVEL_INFO

  implicit none

  private
  public :: read_timestepping_namelist, postprocess_timestepping_namelist, &
            timestepping_is_loadable, timestepping_is_loaded, timestepping_final

  real(r_def), public, protected :: dt
  integer(i_def), public, protected :: timestep_start
  integer(i_def), public, protected :: timestep_end

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] local_rank Rank of current process.
  !>
  subroutine read_timestepping_namelist( file_unit, local_rank )

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    call read_namelist( file_unit, local_rank )

  end subroutine read_timestepping_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, local_rank )

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    integer(i_native), intent(in) :: local_rank

    integer(i_def) :: buffer_integer_i_def(2)
    real(r_def) :: buffer_real_r_def(1)

    namelist /timestepping/ dt, &
                            timestep_start, &
                            timestep_end

    integer(i_native) :: condition

    dt = rmdi
    timestep_start = imdi
    timestep_end = imdi

    if (local_rank == 0) then

      read( file_unit, nml=timestepping, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = dt
    buffer_integer_i_def(1) = timestep_start
    buffer_integer_i_def(2) = timestep_end

    dt = buffer_real_r_def(1)
    timestep_start = buffer_integer_i_def(1)
    timestep_end = buffer_integer_i_def(2)

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_timestepping_namelist()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none


  end subroutine postprocess_timestepping_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function timestepping_is_loadable()

    implicit none

    logical :: timestepping_is_loadable

    timestepping_is_loadable = .not. namelist_loaded

  end function timestepping_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function timestepping_is_loaded()

    implicit none

    logical :: timestepping_is_loaded

    timestepping_is_loaded = namelist_loaded

  end function timestepping_is_loaded

  !> Clear out any allocated memory
  !>
  subroutine timestepping_final()

    use constants_mod, only: cmdi, emdi, imdi, rmdi

    implicit none

    dt = real(rmdi,r_def)
    timestep_start = imdi
    timestep_end = imdi

    return
  end subroutine timestepping_final


end module timestepping_config_mod
