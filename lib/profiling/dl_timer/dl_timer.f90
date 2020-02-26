! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018, Australian Bureau of Meteorology
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
! Authors J. Henrichs, Bureau of Meteorology


!> An implemention of the PSyData API for profiling, which wraps the use
!> of the dl_timer library (https://bitbucket.org/apeg/dl_timer).

module profile_psy_data_mod
  type :: profile_PSyDataType
     character(:), allocatable :: module_name
     character(:), allocatable :: region_name
     integer                   :: timer_index
     logical                   :: registered = .false.
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart, PostEnd
  end type profile_PSyDataType


contains
  ! ---------------------------------------------------------------------------
  !> The initialisation subroutine. It is not called directly from
  !! any PSyclone created code, so a call to ProfileInit must be inserted
  !! manually by the developer.
  !!
  subroutine ProfileInit()
    use dl_timer, only :timer_init

    implicit none
    call timer_init()

  end subroutine ProfileInit

  ! ---------------------------------------------------------------------------
  !> Starts a profiling area. The module and region name can be used to create
  !! a unique name for each region.
  !! Parameters:
  !! @param[inout] this This PSyData instance.
  !! @param[in] module_name Name of the module in which the region is
  !! @param[in] region_name Name of the region (could be name of an invoke, or
  !!            subroutine name).
  !! @param[in] num_pre_vars The number of variables that are declared and
  !!            written before the instrumented region.
  !! @param[in] num_post_vars The number of variables that are also declared
  !!            before an instrumented region of code, but are written after
  !!            this region.
  subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                      num_post_vars)
    use dl_timer, only : timer_register, timer_start
    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character*(*) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if( .not. this%registered) then
       call timer_register(this%timer_index, &
                           label=module_name//":"//region_name)
       this%registered = .true.
    endif
    call timer_start(this%timer_index)
  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a PSyDataType type that corresponds to
  !! to the PreStart call.
  !! @param[inout] this This PSyData instance.
  ! 
  subroutine PostEnd(this)
    use dl_timer, only : timer_stop
    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    
    call timer_stop(this%timer_index)
  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> Called at the end of the execution of a program, usually to generate
  !! all output for the profiling library. Calls timer_report in dl_timer.
  subroutine ProfileFinalise()
    use dl_timer, only : timer_report
    implicit none
    call timer_report()

  end subroutine ProfileFinalise

end module profile_psy_data_mod
