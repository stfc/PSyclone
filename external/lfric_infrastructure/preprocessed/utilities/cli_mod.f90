










!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Parses the commandline for arguments.
!>
module cli_mod

  use, intrinsic :: iso_fortran_env, only : error_unit

  implicit none

  private
  public :: get_initial_filename

contains

  !> Gets the first (and only) command line argument as a filename.
  !>
  !> Also able to interpret and fulfill requests for help using some of the
  !> frequently used methods.
  !>
  !> @param [inout] filename Holds the filename on exit. Will be allocated as
  !>                         needed, replacing any existing allocation.
  !> @param [in] description Optional description of the expected filename. If
  !>                         not specified, master namelist is assumed.
  !> @param [inout] component_name Optional component-component name
  !>
  subroutine get_initial_filename( filename, description, component_name )

    implicit none

    character(:), allocatable,           intent(inout) :: filename
    character(*), optional,              intent(in)    :: description
    character(:), allocatable, optional, intent(inout) :: component_name

    character(:), allocatable :: oname
    character(:), allocatable :: arg

    character(:), allocatable :: program_name
    character(:), allocatable :: filename_description

    character(0) :: dummy
    integer      :: length
    integer      :: status
    integer      :: argument_tally
    integer      :: iarg
    logical      :: filename_set

    if (present(description)) then
      allocate( filename_description, source=description )
    else
      allocate( filename_description, source='Master namelist file' )
    endif

    call get_command_argument( 0, dummy, length, status )
    allocate( character(length) :: program_name )
    call get_command_argument( 0, program_name, length, status )

    argument_tally = command_argument_count()

    if (argument_tally == 0 .and. .not. allocated(filename)) then
      write( error_unit, '("No filename specified")' )
      write( error_unit, '()' )
      call print_usage( error_unit, program_name, filename_description )
      stop 2
    end if

    iarg = 1
    filename_set=.false.

    do
      if( iarg > argument_tally ) exit

      if(allocated(arg)) deallocate(arg)
      call get_command_argument( iarg, dummy, length, status )
      allocate( character(length) :: arg )
      call get_command_argument( iarg, arg, length, status )

      if (arg == '-help' &
          .or. arg == '-h' &
          .or. arg == '--help' ) then
        call print_usage( error_unit, program_name, filename_description )
        stop 2
      else if (arg == '-c' ) then
        call get_command_argument( iarg+1, dummy, length, status )
        allocate( character(length) :: oname )
        call get_command_argument( iarg+1, oname, length, status )
        iarg = iarg + 2
      else if (arg(1:17) == '--component-name=' ) then
        allocate(oname, source=arg(18:) )
        iarg = iarg + 1
      else
        if(.not.filename_set)then
          if(allocated(filename)) deallocate(filename)
          allocate(filename,source=arg)
          iarg = iarg + 1
          filename_set = .true.
        else
          write( error_unit, '("Too many arguments supplied")' )
          write( error_unit, '()' )
          call print_usage( error_unit, program_name, filename_description )
          stop 2
        end if
      end if
    end do

    deallocate( program_name )

    if(present(component_name))then
      if(allocated(oname))allocate(component_name,source=oname)
    end if

  end subroutine get_initial_filename

  subroutine print_usage( to_unit, program_name, filename_description )

    implicit none

    integer,      intent(in) :: to_unit
    character(*), intent(in) :: program_name
    character(*), intent(in) :: filename_description

    write( to_unit, '("Usage:")' )
    write( to_unit, '("    ", A, " [-help|-h|--help] [-c name|--component-name=name] <filename>")' ) &
            program_name
    write( to_unit, '()' )
    write( to_unit, &
           '("    -help, -h, --help - Print this usage information")' )
    write( to_unit, '()' )
    write( to_unit, &
           '("    -c name, --component-name=name - Sets the coupler-component name")' )
    write( to_unit, '()' )
    write( to_unit, '("    filename - ", A)' ) filename_description
    write( to_unit, '()' )


  end subroutine print_usage

end module cli_mod
