! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


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

