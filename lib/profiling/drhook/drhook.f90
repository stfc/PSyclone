! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019, Australian Bureau of Meteorology
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


!> An implemention of profile_mod which wraps the use of Dr Hook.

module profile_mod

  use parkind1, only : jprb
  type :: ProfileData
     ! The opaque DrHook handle for a specific region
     real(kind=jprb) :: zhook_handle
     ! The name of the subroutine and module to be used by DrHook
     character(:), allocatable :: name
     ! True if this instance of ProfileData has the name already
     ! initialised. This way the copy of subroutine name is only
     ! done first time ProfileStart is called.
     logical                   :: initialised = .false.
  end type ProfileData


contains
  ! ---------------------------------------------------------------------------
  ! The initialisation subroutine. It is not called directly from
  ! any PSyclone created code, so a call to ProfileInit must be inserted
  ! manually by the developer. In case of Dr Hook an initialisation is not
  ! necessary.
  !
  subroutine ProfileInit()
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
    use yomhook, only : lhook, dr_hook
    implicit none

    character*(*), intent(in) :: module_name, region_name
    type(ProfileData), intent(inout) :: profile_data

    if (lhook .and. .not. profile_data%initialised) then
      profile_data%name = module_name//":"//region_name
      profile_data%initialised = .true.
    endif
    if(lhook) call dr_hook(profile_data%name, 0, profile_data%zhook_handle)
  end subroutine ProfileStart

  ! ---------------------------------------------------------------------------
  ! Ends a profiling area. It takes a ProfileData type that corresponds to
  ! to the ProfileStart call.
  ! profile_data: Persistent data used by the profiling library.
  ! 
  subroutine ProfileEnd(profile_data)
    use yomhook, only : lhook, dr_hook
    implicit none

    type(ProfileData), intent(inout) :: profile_data
    
    if(lhook) call dr_hook(profile_data%name, 1, profile_data%zhook_handle)
  end subroutine ProfileEnd

  ! ---------------------------------------------------------------------------
  ! Called at the end of the execution of a program, usually to generate
  ! all output for the profiling library. Not required in the case of Dr Hook.
  subroutine ProfileFinalise()
  end subroutine ProfileFinalise

end module profile_mod
