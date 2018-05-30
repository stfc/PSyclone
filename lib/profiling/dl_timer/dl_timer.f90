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


!> An implemention of profile_mod which wraps the use of the dl_timer
!> library (https://bitbucket.org/apeg/dl_timer).
module profile_mod
  type :: ProfileData
     character(:), allocatable :: module_name
     character(:), allocatable :: region_name
     integer                   :: timer_index
     logical                   :: registered = .false.
  end type ProfileData


contains
  ! ---------------------------------------------------------------------------
  ! The initialisation subroutine. It is not called directly from
  ! any PSyclone created code, so a call to ProfileInit must be inserted
  ! manually by the developer.
  !
  subroutine ProfileInit()
    use dl_timer, only :timer_init

    implicit none
    call timer_init()

  end subroutine ProfileInit

  ! ---------------------------------------------------------------------------
  ! Starts a profiling area. The module and region name can be used to create
  ! a unique name for each region.
  ! Parameters: 
  ! module_name:  Name of the module in which the region is
  ! region_name:  Name of the region (could be name of an invoke, or
  !               subroutine name).
  ! profile_data: Persistent data used by the profiling library.
  subroutine ProfileStart(module_name, region_name, profile_data)
    use dl_timer, only : timer_register, timer_start
    implicit none

    character*(*) :: module_name, region_name
    type(ProfileData) :: profile_data

    if( .not. profile_data%registered) then
       call timer_register(profile_data%timer_index, &
                           label=module_name//":"//region_name)
       profile_data%registered = .true.
    endif
    call timer_start(profile_data%timer_index)
  end subroutine ProfileStart

  ! ---------------------------------------------------------------------------
  ! Ends a profiling area. It takes a ProfileData type that corresponds to
  ! to the ProfileStart call.
  ! profile_data: Persistent data used by the profiling library.
  ! 
  subroutine ProfileEnd(profile_data)
    use dl_timer, only : timer_stop
    implicit none

    type(ProfileData) :: profile_data
    
    call timer_stop(profile_data%timer_index)
  end subroutine ProfileEnd

  ! ---------------------------------------------------------------------------
  subroutine ProfileFinalise()
    use dl_timer, only : timer_report
    implicit none
    call timer_report()

  end subroutine ProfileFinalise

end module profile_mod
