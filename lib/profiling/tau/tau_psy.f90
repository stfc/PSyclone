! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
! Author J. Henrichs, Bureau of Meteorology


!> An implementation of the PSyData API for profiling which wraps the use of TAU.

module profile_psy_data_mod

  implicit none

  type :: profile_PSyDataType
     ! The TAU profiling handler
     integer, dimension(2) :: profiler = (/0,0/)

     ! True if this instance of PSyDataType has the name already
     ! initialised. This way the copy of subroutine name is only
     ! done first time PreStart is called.
     logical              :: initialised = .false.
  contains
      ! The profiling API uses only the two following calls:
      procedure           :: PreStart
      procedure           :: PostEnd
  end type profile_PSyDataType

contains

  ! ---------------------------------------------------------------------------
  !> The initialisation subroutine. It is not called directly from
  !! any PSyclone created code, so a call to profile_PSyDataInit must be
  !! inserted manually by the developer. In case of TAU an initialisation
  !! is not necessary.

  subroutine profile_PSyDataInit()
    implicit none
    call TAU_PROFILE_INIT();
  end subroutine profile_PSyDataInit

  ! ---------------------------------------------------------------------------
  !> Starts a profiling area. The module and region name can be used to create
  !! a unique name for each region.
  !! Parameters:
  !! @param[in,out] this This PSyData instance.
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

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character(len=*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if (.not. this%initialised) then
      call TAU_PROFILE_TIMER(this%profiler, module_name//":"//region_name)
      this%initialised = .true.
    endif
    call TAU_PROFILE_START(this%profiler)

  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !! Ends a profiling area. It takes a PSyDataType type that corresponds to
  !! to the PreStart call.
  !! @param[in,out] this This PSyData instance.
  !
  subroutine PostEnd(this)

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this

    call TAU_PROFILE_STOP(this%profiler)

  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> Called at the end of the execution of a program, usually to generate
  !! all output for the profiling library. Not required in the case of TAU.
  subroutine profile_PSyDataShutdown()
    implicit none
  end subroutine profile_PSyDataShutdown

  ! ---------------------------------------------------------------------------
  !> Enable profiling
  subroutine profile_PSyDataStart()

    implicit none
    call TAU_ENABLE_INSTRUMENTATION()
  end subroutine profile_PSyDataStart

  ! ---------------------------------------------------------------------------
  !> Disable profiling
  subroutine profile_PSyDataStop()

    implicit none
    call TAU_DISABLE_INSTRUMENTATION()

  end subroutine profile_PSyDataStop

end module profile_psy_data_mod

