!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> Provides basic I/O utilitiy functions.
!>
module io_utility_mod

  use constants_mod, only : i_native, str_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR

  implicit none

  private
  public :: claim_io_unit, release_io_unit, open_file, close_file, read_line

  ! Unit 10 is used for logging - so start giving out unit numbers from 11
  integer(i_native), save :: next_unit = 11

contains

  !> Gets an I/O unit number and marks it as no longer free. Once I/O is
  !> complete and the unit is no longer needed use
  !> <code>release_io_unit</code>.
  !>
  !> @return The unit number as a native length integer.
  !>
  function claim_io_unit() result(unit)

    implicit none

    integer(i_native) :: unit

    unit = next_unit
    if (unit == 99) then
      next_unit = 103
    else
      next_unit = next_unit + 1
    end if

  end function claim_io_unit

  !> Frees an I/O unit number previously claimed with
  !> <code>claim_io_unit</code>. The unit number may be re-used once it is
  !> released.
  !>
  !> @param unit [in] The unit to release.
  !>
  subroutine release_io_unit( unit )

    implicit none

    integer(i_native), intent(inout) :: unit

    unit = -1
    ! We do not currently re-use unit numbers but we may in the future so this
    ! needs to be in the API.

  end subroutine

  !> Opens a file with built-in error handling. Inability to open the file is
  !> considered an "error" level occurance.
  !>
  !> @todo Currently it is assumed the file is opened for reading. The API
  !>       may have to be extended to support additional options.
  !>
  !> @param filename [in] A string holding the filename to open.
  !> @param unit [in] An optional unit number to open the file on. If no unit
  !>                  is provided one will be assigned.
  !>
  !> @return The unit the file was opened on.
  !>
  function open_file( filename, use_unit )

    implicit none

    character(*),                intent(in) :: filename
    integer(i_native), optional, intent(in) :: use_unit
    integer(i_native) :: open_file

    integer(i_native)  :: unit
    integer(i_native)  :: rc
    character(str_def) :: error_message

    if (present(use_unit)) then
      unit = use_unit
    else
      unit = claim_io_unit()
    end if
    open( unit, file=filename, action='read', iostat=rc, iomsg=error_message )
    if (rc /= 0) then
      write( log_scratch_space, '(A)' ) error_message
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    open_file = unit

  end function open_file

  !> Closes a file and release the associated I/O unit with built-in error
  !> handling. Inability to close the file is considered an "error" level
  !> occurance.
  !>
  !> @param unit [in] The I/O unit to which the file is attached. This unit is
  !>                  released as part of this call.
  !>
  subroutine close_file( unit )

    implicit none

    integer(i_native), intent(inout) :: unit

    integer(i_native)  :: rc
    character(str_def) :: error_message

    close( unit, iostat=rc, iomsg=error_message )
    if (rc /= 0) then
      write( log_scratch_space, '("Unable to close file: ", A)'  ) error_message
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    call release_io_unit( unit )

  end subroutine close_file

  !> Reads a line of text from a file. This does not work on binary files.
  !>
  !> An error will be raised if the file can not be read from.
  !>
  !> \param [in]  unit I/O unit to which the file is attached.
  !> \param [out] buffer Read line is put in this string.
  !> \return True if there is more to read. i.e. if EOF has not been reached.
  !>
  function read_line( unit, buffer )

    implicit none

    integer(i_native), intent(in)  :: unit
    character(*),      intent(out) :: buffer
    logical                        :: read_line

    integer(i_native)  :: rc
    character(str_def) :: error_message

    read( unit, '(A)', iostat=rc, iomsg=error_message ) buffer
    if (is_iostat_end( rc )) then
      read_line = .False.
      return
    else if (rc /= 0) then
      write( log_scratch_space, '("Unable to read file: ", A)' ) error_message
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    read_line = .True.

  end function read_line

end module io_utility_mod
