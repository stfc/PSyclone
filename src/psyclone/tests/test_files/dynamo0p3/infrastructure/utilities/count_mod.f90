!-----------------------------------------------------------------------------
! (C) Crown copyright 2018-2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by J. Henrichs, Bureau of Meteorology

!
!> @brief gives access to counter functionality

module count_mod

  use constants_mod,      only: i_def, i_native, str_def

  implicit none
  private

  integer(i_native), parameter :: num_counters = 100

  type, public :: count_type
    private
    !> number of counters currently in use
    integer(i_native) :: num_counters_in_use
    !> names used to differentiate different counting reporting sections
    character(len=str_def) :: section_name(num_counters)
    !> flag describes whether counting for a section is active or not
    logical :: start_stop(num_counters)
    !> value of the counter  at the start of the counting reporting section
    integer(i_native) :: start_count(num_counters)
    !> value of the running total of the counter for the counting reporting section
    integer(i_native) :: total_count(num_counters)
    !> overall name of counter
    character(len=str_def) :: name
    !> counter for any desired purpose
    integer(i_native) :: overall_counter

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

    integer(i_native) :: k

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
  subroutine output_counters(self, opt_unit)
    use mpi_mod, only: get_comm_rank
    use log_mod,    only: log_event,         &
                          LOG_LEVEL_ERROR,   &
                          LOG_LEVEL_WARNING,    &
                          log_scratch_space
    implicit none
    class(count_type),           intent(inout) :: self
    integer(i_native), optional, intent(in)    :: opt_unit

    integer(i_native)    :: k, unit_no, stat

    ! check all timers are closed
    do k = 1, self%num_counters_in_use
      if( self%start_stop(k) ) then
        write( log_scratch_space, '(A,A,A)') &
                   'Counter for section ',trim(self%section_name(k)), &
                   ' not closed. Counting information will be incorrect'
        call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      end if
    end do

    if ( get_comm_rank() == 0 ) then
      if(present(opt_unit)) then
        unit_no=opt_unit
      else
        unit_no=123
        open( unit_no, file=trim(self%name)//'_counter.txt', status="replace", iostat=stat)
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
