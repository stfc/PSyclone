!-----------------------------------------------------------------------------
! (C) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief gives access to counter functionality
!>
!> @todo An alternative to global variables will likely be necessary to
!>       support mulit-instance models. Alternatively the counter module may
!>       be superceded by Vernier.
!>
module count_mod

  use constants_mod,      only: i_def, str_def, str_max_filename, &
                                cmdi

  implicit none
  private

  integer(i_def), parameter :: num_counters = 100
  character(str_max_filename), parameter  :: &
                                         default_counter_suffix = "counter.txt"

  type, public :: count_type
    private
    !> number of counters currently in use
    integer(i_def) :: num_counters_in_use
    !> names used to differentiate different counting reporting sections
    character(len=str_def) :: section_name(num_counters)
    !> flag describes whether counting for a section is active or not
    logical :: start_stop(num_counters)
    !> value of the counter  at the start of the counting reporting section
    integer(i_def) :: start_count(num_counters)
    !> value of the running total of the counter for the counting reporting section
    integer(i_def) :: total_count(num_counters)
    !> overall name of counter
    character(len=str_def) :: name
    !> counter for any desired purpose
    integer(i_def) :: overall_counter

  contains
    !> starts/stops recording a counter for a given section
    procedure, public :: counter
    !> Outputs all the counter information
    procedure, public :: output_counters
    !> Increments the overall counter
    procedure, public :: counter_inc
  end type count_type

  interface count_type
    module procedure count_constructor
  end interface

  type(count_type), public, allocatable :: halo_calls

contains

  function count_constructor(name) result(count)
    implicit none
    character(len=*), intent(in)  :: name
    type(count_type)  :: count

    count%name = name
    count%overall_counter = 0
    count%num_counters_in_use = 0
    count%start_stop(:) = .false.
  end function count_constructor

!=============================================================================!
!> @brief starts/stops recording a counter for a given section
!> @param[in] cname Name of the counting section to start/stop
  subroutine counter(self, cname)

    use log_mod,    only: log_event,         &
                          LOG_LEVEL_ERROR

    implicit none

    class(count_type), intent(inout)  :: self
    character(len=*), intent(in)  :: cname

    integer(i_def) :: k

    do k = 1, self%num_counters_in_use
       if( cname == self%section_name(k) ) exit
     end  do

    if( k > self%num_counters_in_use ) then
    ! named section not in list so initialise
      self%num_counters_in_use = k
      if( self%num_counters_in_use > num_counters ) then
        call log_event( "Run out of counters", LOG_LEVEL_ERROR )
      end if
      self%section_name(k) = cname
      self%start_count(k) = self%overall_counter
      self%start_stop(k) = .true.
      self%total_count(k)   = 0
    else
      ! named section found in list
      ! check to see if its the start or end of a counting section
      if( self%start_stop(k) ) then
        self%total_count(k) = self%total_count(k) + &
                               self%overall_counter - self%start_count(k)
        self%start_stop(k) = .false.
      else
        self%start_stop(k) = .true.
        self%start_count(k) = self%overall_counter
      endif
    endif

  end subroutine counter

!=============================================================================!
  !> @brief write out timer information to file
  !> @param in opt_unit (optional) optional unit number to which the output
  !>                               should be written
  subroutine output_counters(self, opt_suffix, opt_unit)
    use lfric_mpi_mod,  only: global_mpi
    use log_mod,        only: log_event,         &
                              LOG_LEVEL_ERROR,   &
                              LOG_LEVEL_WARNING, &
                              log_scratch_space
    use io_utility_mod, only: claim_io_unit, release_io_unit
    implicit none
    class(count_type),           intent(inout) :: self
    character(*),      optional, intent(in)    :: opt_suffix
    integer(i_def),    optional, intent(in)    :: opt_unit

    character(str_max_filename) :: suffix
    integer(i_def)    :: k, unit_no, stat

    ! Check all timers are closed
    do k = 1, self%num_counters_in_use
      if( self%start_stop(k) ) then
        write( log_scratch_space, '(A,A,A)') &
                   'Counter for section ',trim(self%section_name(k)), &
                   ' not closed. Counting information will be incorrect'
        call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      end if
    end do

    if ( global_mpi%get_comm_rank() == 0 ) then
      if(present(opt_unit)) then
        unit_no=opt_unit
      else
        unit_no=claim_io_unit()
        if (present(opt_suffix)) then
          if( opt_suffix == cmdi )then
            suffix = default_counter_suffix
          else
            suffix = opt_suffix
          end if
        else
          suffix = default_counter_suffix
        end if
        open( unit_no, file=trim(self%name)//'_'//trim(suffix), status="replace", iostat=stat)
        if (stat /= 0) then
          call log_event( "Unable to open counter file", LOG_LEVEL_ERROR )
        end if
      end if
      ! Write out timer information in wiki formatted table
      write(unit_no,'(A)')  &
      '||=  Section name =||= Count within section =||'
      do k = 1, self%num_counters_in_use
        write(unit_no,('(A,A50,A,i14,A)')) &
               '||', trim(self%section_name(k)),'||', &
                     self%total_count(k),'||'
      end do
      if(.not.present(opt_unit)) then
        close( unit_no )
        call release_io_unit( unit_no )
      end if
    end if
  end subroutine output_counters

!=============================================================================!
  !> @brief increments the overall counter
  subroutine counter_inc(self)
    implicit none
    class(count_type), target, intent(inout)  :: self
    self%overall_counter = self%overall_counter + 1
  end subroutine counter_inc

!=============================================================================!

end module count_mod
