

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief A simple logging facility.
!>
!> If the code is being run serially, the logging information will be written
!> to the terminal. For parallel execution, the logging information will
!> be sent to files - one for each MPI task.
!>
!> @todo  At some point the serial version of Dynamo should also log to a file,
!>        but for now it is easier for developers if the code logs to stdout.


module log_mod

  use constants_mod, only : str_long, str_max_filename
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit

  implicit none

  private
  public initialise_logging, finalise_logging, &
         log_set_info_stream, log_set_alert_stream, &
         log_set_level, log_level, log_event

  !> Named logging level.
  !>
  !> Any integer can be used for the logging level but this name represents
  !> a break between level. Generally you will want to use these names.
  !>
  !> @{
  integer, public, parameter :: LOG_LEVEL_ALWAYS  = 100000
  integer, public, parameter :: LOG_LEVEL_ERROR   = 200
  integer, public, parameter :: LOG_LEVEL_WARNING = 150
  integer, public, parameter :: LOG_LEVEL_INFO    = 100
  integer, public, parameter :: LOG_LEVEL_DEBUG   =  50
  integer, public, parameter :: LOG_LEVEL_TRACE   =   0
  !> @}

  !> Space in which to marshal log messages.
  !>
  !> Although any string can be passed to log_event() this space is provided to
  !> prevent a proliferation of work spaces all over the code. It also means
  !> that should 160 characters be found to be insufficient it need only be
  !> changed in one place.
  !>
  character( str_long + str_max_filename ), public :: log_scratch_space

  integer, private, parameter :: EXIT_CODE_ON_ERROR = 1

  integer, private :: logging_level = LOG_LEVEL_INFO
  integer, private :: info_unit     = output_unit
  integer, private :: alert_unit    = error_unit

  integer, private :: log_unit_number = 10
  logical, private :: is_parallel = .false.
  character(len=:), allocatable :: petno

contains

  !> Set where information goes.
  !>
  !> If this routine is never called then information will default to standard
  !> out.
  !>
  !> @param unit The unit to send information to
  !>
  subroutine log_set_info_stream(unit)

    implicit none

    integer, intent( in ) :: unit

    info_unit = unit

  end subroutine log_set_info_stream

  !> Set where alerts go.
  !>
  !> If this routine is never called then alerts will default to standard
  !> error.
  !>
  !> @param unit The unit to send alerts to
  !>
  subroutine log_set_alert_stream(unit)

    implicit none

    integer, intent( in ) :: unit

    alert_unit = unit

  end subroutine log_set_alert_stream

  !> Set the level this logger responds to.
  !>
  !> Events ranked lower than the logging level will be accepted and dropped
  !> on the floor.
  !>
  !> @param level The new logging level to adopt.
  !>
  subroutine log_set_level(level)

    implicit none

    integer, intent( in ) :: level

    logging_level = level

  end subroutine log_set_level

  !> Gets the current logging level.
  !>
  !> Primarily used for testing purposes.
  !>
  !> @returns The logging level.
  !>
  function log_level()

    implicit none

    integer :: log_level

    log_level = logging_level

  end function


  !> Initialise logging functionality by opening the log files
  !> @param this_rank The number of the local rank
  !> @param total_ranks The total number pf ranks in the job
  !> @param app_name The name of the application. This will form part of the
  !>                 log file name(s)
  subroutine initialise_logging(this_rank, total_ranks, app_name)
    implicit none
    integer, intent(in) :: this_rank, total_ranks
    character(len=*), intent(in) :: app_name
    integer :: ios
    integer :: ilen
    character(len=:), allocatable :: logfilename
    character(len=12) :: fmt

    if (total_ranks > 1 ) then
      is_parallel = .true.
      ilen=int(log10(real(total_ranks-1)))+1
      write(fmt,'("(i",i0,".",i0,")")')ilen, ilen
      allocate(character(len=ilen) :: petno)
      write(petno,fmt)this_rank
      allocate(character(len=ilen+len_trim(app_name)+8) :: logfilename)
      write(logfilename,"(a,a,a,a,a)")"PET",petno,".",trim(app_name),".Log"
      open(unit=log_unit_number, file=logfilename, status='unknown', iostat=ios)
      if ( ios /= 0 )then
        write(error_unit,"('Cannot open logging file. iostat = ',i0)")ios
        stop EXIT_CODE_ON_ERROR
      end if
      call log_event('LFRic Logging System Version 1.0',LOG_LEVEL_ALWAYS)
    else
      is_parallel = .false.
    end if

  end subroutine initialise_logging

  !> Finalise logging functionality by closing the log files
  subroutine finalise_logging()
    implicit none
    integer :: ios
    if ( is_parallel ) then
      close(unit=log_unit_number,iostat=ios)
      if ( ios /= 0 )then
        write(error_unit,"('Cannot close logging file. iostat = ',i0)")ios
        stop EXIT_CODE_ON_ERROR
      end if
    end if
  end subroutine finalise_logging

  !> Log an event
  !>
  !> If the code is running on multiple MPI ranks, the event description will
  !> be sent to a log file. For serial executions, the event description is
  !> sent to the terminal along with timestamp and level information.
  !> For the most serious events (a severity level equal to
  !> or greater than LOG_LEVEL_ERROR), execution of the code will be aborted.
  !>
  !> @param message A description of the event.
  !> @param level   The severity of the event. Defaults to cInfoLevel.
  !>
  subroutine log_event(message, level)

    use, intrinsic :: iso_fortran_env, only : output_unit, error_unit

    implicit none

    character (*), intent( in ) :: message
    integer,       intent( in ) :: level

    integer        :: unit
    character (5)  :: tag
    character (8)  :: date_string
    character (10) :: time_string
    character (5)  :: zone_string

    logical :: abort_run = .false.

    if (level >= logging_level) then

      select case (level)
        case ( : LOG_LEVEL_DEBUG - 1)
          unit = info_unit
          tag  = 'TRACE'
        case (LOG_LEVEL_DEBUG : LOG_LEVEL_INFO - 1 )
          unit = info_unit
          tag  = 'DEBUG'
        case ( LOG_LEVEL_INFO : LOG_LEVEL_WARNING - 1 )
          unit = info_unit
          tag  = 'INFO '
        case ( LOG_LEVEL_WARNING : LOG_LEVEL_ERROR - 1)
          unit = alert_unit
          tag  = 'WARN '
        case ( LOG_LEVEL_ERROR : LOG_LEVEL_ALWAYS - 1)
          unit = alert_unit
          tag  = 'ERROR'
          abort_run = .true.
        case ( LOG_LEVEL_ALWAYS : )
          unit = info_unit
          tag  = 'INFO'
      end select

      call date_and_time( date=date_string, time=time_string, zone=zone_string)

      if(is_parallel)then
        unit = log_unit_number
        write (unit, '(A," ",A," ",A,"            PET",A," ",A)') &
                   date_string, time_string, tag, petno, trim( message )
      else
        write (unit, '(A,A,A,":",A,": ",A)') &
                   date_string, time_string, zone_string, tag, trim( message )
      end if

      ! If the severity level of the event is serious enough, stop the code.
      if ( abort_run ) then
        stop EXIT_CODE_ON_ERROR
      end if

    end if

  end subroutine log_event

end module log_mod
