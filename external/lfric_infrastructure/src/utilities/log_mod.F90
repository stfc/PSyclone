!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief A simple logging facility.
!>
!> If the code is being run serially, the logging information will be written
!> to the terminal. For parallel execution, the logging information will
!> be sent to files - one for each MPI task.
!>
!> Optionally, the initialise can be called with a logical flag to limit
!> parallel logging to only write to one rank (rank 0 by default, configurable)
!> which is useful for performance runs at large rank counts.
!>
!> @todo  At some point the serial version of a model should also log to a file,
!>        but for now it is easier for developers if the code logs to stdout.
!>
!> @todo There are many global variables here which must be removed in order
!>       to support multi-instance models. This almost certainly requires a
!>       ground-up rewrite of the logging framework.
!>
module log_mod

  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit

  use constants_mod,   only : i_def,      &
                              i_timestep, &
                              str_long,   &
                              str_max_filename
#ifdef NO_MPI
  ! No uses of the mpi library and no calls to parallel_abort a non-mpi build
#else
  use lfric_abort_mod, only : parallel_abort
  use mpi, only : mpi_comm_rank, mpi_comm_size
#endif

  implicit none

  private
  public finalise_logging, initialise_logging,      &
         log_set_alert_stream, log_set_info_stream, &
         log_set_timestep, log_forget_timestep,     &
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
  logical, private :: warning_trace = .false.
  integer(i_def),      private, allocatable :: mpi_communicator
  character(len=:),    private, allocatable :: petno
  integer(i_timestep), private, allocatable :: timestep
  !> Control optional log suppression by rank, all ranks log by default.
  logical, private :: emit_log_message

contains

  !> @brief Sets up logging for operation in a parallel regime.
  !>
  !> @param[in] communicator MPI communicator to operate in.
  !> @param[in] file_name Appears in output filename.
  !> @param[in] trace_on_warnings Output a backtrace on warnings.
  !> @param[in] log_to_rank_zero_only Only log to rank zero.
  !>
  subroutine initialise_logging( communicator, &
                                 file_name,    &
                                 trace_on_warnings, &
                                 log_to_rank_zero_only)

    implicit none

    integer(i_def),           intent(in) :: communicator
    character(*),             intent(in) :: file_name
    logical, optional,        intent(in) :: trace_on_warnings
    logical, optional,        intent(in) :: log_to_rank_zero_only

#ifdef NO_MPI
    ! The logging is running with no MPI communications
    ! If no MPI then default behaviour is the only behaviour: do emit log
    emit_log_message = .true.
#else
    integer(i_def) :: status
    integer(i_def) :: ilen
    character(len=:), allocatable :: logfilename
    character(len=12)             :: fmt
    integer(i_def)                :: this_rank
    integer(i_def)                :: total_ranks
    logical                       :: log_to_rank_0_only

    ! default behaviour: log to all ranks
    emit_log_message = .true.

    if (present(log_to_rank_zero_only)) then
      log_to_rank_0_only = log_to_rank_zero_only
    else
      log_to_rank_0_only = .false.
    end if

    ! Check the number of ranks in the incomming communicator, as this
    ! determines whether logging goes to a file, or the terminal
    call mpi_comm_size( communicator, total_ranks, ierror=status )
    if (status /= 0) then
      write( error_unit, &
             "('Cannot determine communicator size. (',i0,')')" ) status
      call abort_model()
    end if

    if (total_ranks /= 1) then
      allocate( mpi_communicator )
      ! Duplicate the communicator  - so we can't do any damage to the original
      call mpi_comm_dup(communicator, mpi_communicator, status)
      if (status /= 0) then
        write( error_unit, &
               "('Cannot duplicate the communicator. (',i0,')')" ) status
        call abort_model()
      end if
      call mpi_comm_rank( mpi_communicator, this_rank, ierror=status )
      if (status /= 0) then
        write( error_unit, "('Cannot determine rank. iostat = ',i0)" ) status
        call abort_model()
      end if

      if (allocated(mpi_communicator)) then
        if (log_to_rank_0_only) then
          if (this_rank /= 0) then
            ! Sole rank identified for logging, do not log this rank.
            emit_log_message = .false.
          end if
        endif
      endif
      ! Only initialise log file if emit_log_message.
      if (emit_log_message) then
        ilen = int( log10( real( total_ranks - 1 ) ) ) + 1
        write( fmt, '("(i",i0,".",i0,")")' ) ilen, ilen
        allocate( character(len=ilen) :: petno )
        write( petno, fmt ) this_rank

        allocate( character(ilen + len_trim(file_name) + 8) :: logfilename )
        write( logfilename, '("PET", a, ".", a, ".Log")' ) petno, trim(file_name)
        open( unit=log_unit_number, file=logfilename, &
              action="write", status='unknown', iostat=status )
        if ( status /= 0 ) then
          write( error_unit, &
                 '("Cannot open logging file. iostat = ", i0)' ) status
          call abort_model()
        end if
      endif
    end if
#endif

    call base_initialise( trace_on_warnings)

  end subroutine initialise_logging

  !> @brief Initialisation common to all regimes.
  !>
  !> @param[in] trace_on_warnings Output a backtrace on warnings.
  !>
  subroutine base_initialise( trace_on_warnings )

    implicit none

    logical, optional, intent(in) :: trace_on_warnings

    if (present(trace_on_warnings)) then
      warning_trace = trace_on_warnings
    end if

  end subroutine base_initialise

  !> Finalise logging functionality by closing the log files.
  !>
  subroutine finalise_logging()

#ifdef NO_MPI
    ! No "use mpi" in non-mpi build
#else
    use mpi, only : mpi_barrier
#endif

    implicit none

    integer :: ios

    ! Only finalise log file if MPI communicator and emit_log_message
    if (allocated(mpi_communicator)) then
      if (emit_log_message) then
        close( unit=log_unit_number, iostat=ios )
        if ( ios /= 0 )then
          write( error_unit, "('Cannot close logging file. iostat = ', i0)" ) ios
          call abort_model()
        end if
        call log_forget_timestep()
        deallocate( petno )
      endif

#ifdef NO_MPI
      ! No barriers required in non-mpi build
#else
      call mpi_barrier( mpi_communicator, ierror=ios )
#endif
      deallocate( mpi_communicator )
    end if

  end subroutine finalise_logging

  !> Sets the current timestep.
  !>
  subroutine log_set_timestep( new_timestep )

    implicit none

    integer(i_timestep), intent(in) :: new_timestep

    ! We could check to make sure that timesteps only ever increase or even
    ! that they are monotonically increasing. It seems best to not bother,
    ! at least to start with. If we do checks we then have to report when
    ! those checks fail which means calling the logging framework from itself.
    !
    ! There shouldn't be a problem with that but one thing at a time.
    !
    if (.not. allocated(timestep)) then
      allocate(timestep)
    end if
    timestep = new_timestep

  end subroutine log_set_timestep


  !> Makes the logger forget about the current timestep.
  !>
  subroutine log_forget_timestep

    implicit none

    if (allocated(timestep)) deallocate(timestep)

  end subroutine log_forget_timestep


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
    use traceback_mod, only : traceback

    implicit none

    character (*), intent( in ) :: message
    integer,       intent( in ) :: level

    integer        :: unit
    character (5)  :: tag
    character (8)  :: date_string
    character (10) :: time_string
    character (5)  :: zone_string

    logical :: abort_run = .false.
    logical :: trace

    trace = .false.

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
          if(warning_trace) trace = .true.
        case ( LOG_LEVEL_ERROR : LOG_LEVEL_ALWAYS - 1)
          unit = alert_unit
          tag  = 'ERROR'
          trace = .true.
          abort_run = .true.
        case ( LOG_LEVEL_ALWAYS : )
          unit = info_unit
          tag  = 'INFO'
      end select

      if (emit_log_message) then

        if (allocated(petno)) unit = log_unit_number

        call date_and_time( date=date_string, time=time_string, zone=zone_string)
        write( unit, '(A, A, A)', &
               advance='no' ) date_string, time_string, zone_string

        if (allocated(petno)) then
          write( unit, '(":P", A)', advance='no' ) petno
        end if

        if (allocated(timestep)) then
          write( unit, '(":S", I0)', advance='no' ) timestep
        end if

        write ( unit, '(":",A,": ",A)') tag, trim( message )

        if (logging_level <= LOG_LEVEL_DEBUG) then
          flush(unit)
        end if

      endif

      if (trace) then
        call traceback()
      end if

      ! If the severity level of the event is serious enough, stop the code.
      if ( abort_run ) then
        call abort_model()
      end if

    end if

  end subroutine log_event


  ! @brief   Close output unit and stop the model
  !
  subroutine abort_model()

    implicit none

    integer             :: ios
    logical             :: file_opened

    if (allocated(petno)) then
      ! Close the parallel output files if opened
      inquire( unit = log_unit_number, opened = file_opened )
      if ( file_opened ) then
        flush( log_unit_number )
        close( unit = log_unit_number , iostat = ios )
        if ( ios /= 0 )then
          write(error_unit,"('Cannot close logging file. iostat = ',i0)")ios
        end if
      end if

#ifdef NO_MPI
      ! No "parallel_abort()" calls in a non-mpi build
#else
      ! Abort parallel applications
      call parallel_abort( EXIT_CODE_ON_ERROR )
#endif

    else
      ! Stop serial applications
      stop EXIT_CODE_ON_ERROR

    end if

  end subroutine abort_model

end module log_mod
