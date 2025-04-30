!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief A Simple subroutine timer based upon calls to cpu time
!>
!> @todo An alternative to global variables will likely be necessary to
!>       support mulit-instance models. Alternatively the timer module may
!>       be superceded by Vernier.
!>
module timer_mod
   use constants_mod, only: i_def, str_def, str_max_filename, &
                            i_long, r_double, cmdi

   implicit none

   private

   character(str_max_filename), parameter      :: &
                                              default_timer_path = "timer.txt"
   integer(i_def),        parameter            :: num_subs       = 300

   integer(i_def)                              :: num_tim_in_use = 0
   character(len=str_def)                      :: routine_name(num_subs)
   integer(i_long),       dimension(num_subs)  :: isystem_clock_time
   real(r_double),        dimension(num_subs)  :: min_system_time
   real(r_double),        dimension(num_subs)  :: mean_system_time
   real(r_double),        dimension(num_subs)  :: max_system_time
   integer(i_long),       dimension(num_subs)  :: iprev_time
   integer(i_long),       dimension(num_subs)  :: num_calls
   logical,               dimension(num_subs)  :: start_stop

   ! File parameters for timer output
   character(str_max_filename) :: timer_path
   integer                     :: timer_file_unit = 9
   real(r_double)              :: clock_rate

   public  :: timer, init_timer
   public  :: output_timer
   private :: convert_to_lower

   ! These routines only need to be public for unit test
   public :: calculate_timer_stats
   public :: get_routine_name
   public :: get_routine_total_calls
   public :: get_mean_time
   public :: reset_timer

contains

!=============================================================================!
!> @brief initialize system clock-related variables
!> @param[in] timer_path  Path to the output file for the timer
  subroutine init_timer(timer_output_path)

    use log_mod,    only: log_event,         &
                          log_scratch_space, &
                          LOG_LEVEL_DEBUG

    implicit none

    character(*), optional, intent(in) :: timer_output_path

    integer(i_long) :: count_rate

    ! Set timer path
    if (present(timer_output_path)) then
      if( timer_output_path == cmdi )then
        timer_path = default_timer_path
      else
        timer_path = timer_output_path
      end if
    else
      timer_path = default_timer_path
    end if

    ! Get the number of clock ticks per unit time from system_clock
    call system_clock(count_rate=count_rate)
    clock_rate = real(count_rate, r_double)

    ! Only need to output this for debugging purposes
    write(log_scratch_space,'(A,I0)') "system_clock rate for timers: ", count_rate
    call log_event(log_scratch_space, LOG_LEVEL_DEBUG)

  end subroutine init_timer

!=============================================================================!
!> @brief start/stop recording the runtime of a give section
!> @param[in] cname Name of the timing section to start/stop
   subroutine timer(cname)

     use log_mod,    only: log_event,         &
                           LOG_LEVEL_ERROR

     implicit none

     character(len=*),          intent(in)  :: cname
     character(len=str_def)                 :: lowname
     integer                                :: k
     integer(i_long)                        :: itime

     ! check if this is the first call to timer
     if( num_tim_in_use == 0_i_def ) start_stop(:) = .false.

     ! convert cname to lower case
     lowname = convert_to_lower(cname)

     do k = 1, num_tim_in_use
        if( lowname == routine_name(k) ) exit
      end  do

     if( k > num_tim_in_use ) then
     ! subroutine not in list so initialise
       num_tim_in_use = k
       if( num_tim_in_use > num_subs ) then
         call log_event( "Run out of timers, increase num_subs", &
                         LOG_LEVEL_ERROR )
       end if
       routine_name(k) = lowname
       call system_clock(iprev_time(k))
       start_stop(k) = .true.
       isystem_clock_time(k)   = 0_i_long
       num_calls(k)  = 1_i_long
     else

       ! Found routine check to see if its the start or end of
       ! a timing section
       if( start_stop(k) ) then
         call system_clock(itime)
         isystem_clock_time(k) = isystem_clock_time(k) + (itime - iprev_time(k))
         start_stop(k) = .false.
       else
         start_stop(k) = .true.
         call system_clock(iprev_time(k))
         num_calls(k)  = num_calls(k) + 1_i_long
       endif
     endif

   end subroutine timer

!=============================================================================!
!> @brief Calculate statistics required for output
   subroutine calculate_timer_stats()
     use lfric_mpi_mod, only: global_mpi
     use log_mod,       only: log_event,         &
                              LOG_LEVEL_ERROR,   &
                              log_scratch_space

     implicit none

     integer :: k
     ! use a fixed precision (64 bit `r_double` kind) to ensure that the
     ! output is invariant to `r_def`
     real(r_double)    :: time_sum
     real(r_double)    :: time_real_tmp
     integer(i_def)    :: total_ranks

     total_ranks = global_mpi%get_comm_size()

     ! check all timers are closed
     do k = 1, num_tim_in_use
       if( start_stop(k) ) then
         write( log_scratch_space, '(A,A,A)') &
                    'Timer for routine ',trim(routine_name(k)), &
                    ' not closed.'
         call log_event( log_scratch_space, LOG_LEVEL_ERROR )
       end if
     end do

     do k = 1, num_tim_in_use
       time_real_tmp = real(isystem_clock_time(k)/clock_rate, r_double)
       call global_mpi%global_sum(time_real_tmp, time_sum)
       mean_system_time(k) = time_sum/total_ranks
       call global_mpi%global_min(time_real_tmp, min_system_time(k))
       call global_mpi%global_max(time_real_tmp, max_system_time(k))
     end do

   end subroutine calculate_timer_stats

!=============================================================================!
   !> @brief write out timer information to file
   subroutine output_timer()
     use lfric_mpi_mod,  only: global_mpi
     use log_mod,        only: log_event,       &
                               LOG_LEVEL_ERROR, &
                               LOG_LEVEL_INFO,  &
                               log_scratch_space
     use io_utility_mod, only: claim_io_unit, close_file

     implicit none

     integer(i_def)    :: k
     real(r_double)    :: pc_time

     integer(i_def)    :: stat
     real(r_double)    :: time_real_tmp

     ! LOG local times
     write(log_scratch_space,'(A2,A32,4(A2,A21),A2)')                        &
     '||','=           Routine            =',                                &
     '||','=   time(s)     =',                                               &
     '||','=     No. calls     =',                                           &
     '||','= time per call(s)  =',                                           &
     '||'
     call log_event(log_scratch_space, LOG_LEVEL_INFO)
     do k = 1, num_tim_in_use
       time_real_tmp = real(isystem_clock_time(k)/clock_rate, r_double)
       write(log_scratch_space,                                              &
            ('(A2,A32,A2,f21.2,A2,i21,A2,f21.4,A2)'))                  &
            '||', trim(routine_name(k)),                                     &
            '||', time_real_tmp,                                             &
            '||', num_calls(k),                                              &
            '||', time_real_tmp/REAL(num_calls(k),r_double),                 &
            '||'
       call log_event(log_scratch_space, LOG_LEVEL_INFO)
     end do

     call calculate_timer_stats()

     if ( global_mpi%get_comm_rank() == 0 ) then
       timer_file_unit = claim_io_unit()
       open( timer_file_unit, file=trim(timer_path), status="replace", iostat=stat)
       if (stat /= 0) then
         call log_event( "Unable to open timer file", LOG_LEVEL_ERROR )
       end if

       ! Write out timer information in wiki formatted table
       write(timer_file_unit,'(A2,A32,7(A2,A21),A2)')                          &
       '||','=           Routine            =',                                &
       '||','=   min time(s)     =',                                           &
       '||','=   mean time(s)    =',                                           &
       '||','=   max time(s)     =',                                           &
       '||','=     No. calls     =',                                           &
       '||','=       %time       =',                                           &
       '||','= time per call(s)  =',                                           &
       '||'
       do k = 1, num_tim_in_use
         pc_time = mean_system_time(k)/mean_system_time(1)*100.0_r_double
         write(timer_file_unit,                                                &
              ('(A2,A32,A2,3(f21.2,A2),i21,2(A2,f21.4),A2)'))                  &
              '||', trim(routine_name(k)),                                     &
              '||', min_system_time(k),                                        &
              '||', mean_system_time(k),                                       &
              '||', max_system_time(k),                                        &
              '||', num_calls(k),                                              &
              '||', pc_time,                                                   &
              '||', mean_system_time(k)/REAL(num_calls(k),r_double),           &
              '||'
       end do
       call close_file(timer_file_unit)
     end if
   end subroutine output_timer

!=============================================================================!
!> @brief Resets the values of the timer to zero.
   subroutine reset_timer()

     implicit none

     isystem_clock_time(:) = 0_i_def
     min_system_time(:) = 0.0_r_double
     mean_system_time(:) = 0.0_r_double
     max_system_time(:) = 0.0_r_double
     iprev_time(:) = 0_i_long
     num_calls(:) = 0_i_long
     start_stop(:) = .false.
     num_tim_in_use = 0_i_def

   end subroutine reset_timer

!=============================================================================!
!> @brief Changes a string to lower case
!> @param[in] str Input string to convert
!> @result string Lower case string
  pure function convert_to_lower (str) Result (string)

    implicit none
    character(*), intent(in) :: str
    character(len(str))      :: string

    integer :: ic, i

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

    string = str
    do i = 1, len_trim(str)
        ic = index(cap, str(i:i))
        if (ic > 0) string(i:i) = low(ic:ic)
    end do

  end function convert_to_lower
!=============================================================================!

!> @brief return the routine name for a given index
!> @param[in] idx index of routine
!> @result nme name of routine
  pure function get_routine_name(idx) result(nme)
    implicit none
    integer(i_def), intent(in) :: idx
    character(str_def)         :: nme
    nme = routine_name(idx)
  end function get_routine_name
!=============================================================================!

!> @brief return the total number of calls for a given index
!> @param[in] idx index of routine
!> @result n number of calls
  pure function get_routine_total_calls(idx) result(n)
    implicit none
    integer(i_def), intent(in) :: idx
    integer(i_long)          :: n
    n = num_calls(idx)
  end function get_routine_total_calls
!=============================================================================!

!> @brief return the mean time for a given index
!> @param[in] idx index of routine
!> @result n mean recorded time for the routine
  pure function get_mean_time(idx) result(mean_time)
    implicit none
    integer(i_def), intent(in) :: idx
    real(r_double)             :: mean_time
    mean_time = mean_system_time(idx)
  end function get_mean_time
!=============================================================================!

end module timer_mod
