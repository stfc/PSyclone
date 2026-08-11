! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module profile_psy_data_mod

  use iso_c_binding, only : C_CHAR, C_INT, C_INT64_T, C_NULL_CHAR

  implicit none

  private

  !> The derived type passed to us from the profiled application. Required for
  !! consistency with the PSyclone Profiling interface and to prevent repeated string
  !! operations.
  type, public :: profile_PSyDataType
     !> Whether or not we've seen this region before
     logical :: initialised = .false.
     !> Name assigned to the region
     character(kind=C_CHAR, len=256) :: name = ""
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart
      procedure :: PostEnd
  end type profile_PSyDataType

  ! ROCTx C API bindings (rocprofiler-sdk-roctx)
  ! See: https://rocm.docs.amd.com/projects/rocprofiler-sdk/en/latest/how-to/using-rocprofiler-sdk-roctx.html
  interface
     ! Push a new nested range with a name string
     ! NOTE: roctxRangePush is a macro that expands to roctxRangePushA in the rocprofiler-sdk-roctx library.
     !       This is why we use roctxRangePushA here and not roctxRangePush.
     integer(C_INT) function roctxRangePushA(name) bind(C, name='roctxRangePushA')
       use iso_c_binding
       character(kind=C_CHAR), intent(in) :: name(*)
     end function roctxRangePushA

     ! Pop the current nested range
     integer(C_INT) function roctxRangePop() bind(C, name='roctxRangePop')
       use iso_c_binding
     end function roctxRangePop

     ! Get the current thread ID (returns 0 on success)
     integer(C_INT) function roctxGetThreadId(tid) bind(C, name='roctxGetThreadId')
       use iso_c_binding
       integer(C_INT64_T), intent(out) :: tid
     end function roctxGetThreadId

     ! Request profiling tool to pause data collection (tid=0 for all threads)
     integer(C_INT) function roctxProfilerPause(tid) bind(C, name='roctxProfilerPause')
       use iso_c_binding
       integer(C_INT64_T), value, intent(in) :: tid
     end function roctxProfilerPause

     ! Request profiling tool to resume data collection (tid=0 for all threads)
     integer(C_INT) function roctxProfilerResume(tid) bind(C, name='roctxProfilerResume')
       use iso_c_binding
       integer(C_INT64_T), value, intent(in) :: tid
     end function roctxProfilerResume
  end interface

  ! Only the routines making up the PSyclone profiling API are public
  public profile_PSyDataInit, profile_PSyDataShutdown, &
       profile_PSyDataStart, profile_PSyDataStop

contains

  !> An optional initialisation subroutine. This is not used for the ROCTx
  !! library.
  subroutine profile_PSyDataInit()
    implicit none
    return
  end subroutine profile_PSyDataInit

  !> Enables profiling (if it is not already enabled). May be manually added
  !! to source code in order to limit the amount of profiling performed at
  !! run time. Uses roctxProfilerResume to request the profiling tool to
  !! resume data collection.
  subroutine profile_PSyDataStart()
    implicit none
    integer(C_INT64_T) :: tid
    integer(C_INT) :: ierr

    ierr = roctxGetThreadId(tid)
    ierr = roctxProfilerResume(tid)

  end subroutine profile_PSyDataStart

  !> Turns off profiling. All subsequent calls to the profiling API
  !! will have no effect. Use in combination with profile_PSyDataStart() to
  !! limit the amount of profiling performed at runtime. Uses roctxProfilerPause
  !! to request the profiling tool to pause data collection.
  subroutine profile_PSyDataStop()
    implicit none
    integer(C_INT64_T) :: tid
    integer(C_INT) :: ierr

    ierr = roctxGetThreadId(tid)
    ierr = roctxProfilerPause(tid)

  end subroutine profile_PSyDataStop

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

    class(profile_PSyDataType), target, intent(inout) :: this
    character(len=*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars
    ! Locals
    integer(C_INT) :: range_id

    if (.not. this%initialised) then
       ! This is the first time we've seen this region. Construct and
       ! save its name to save on future string operations.
       this%initialised = .true.
       this%name = trim(module_name)//":"//trim(region_name)//C_NULL_CHAR
    end if

    range_id = roctxRangePushA(this%name)

  end subroutine PreStart

  !> Ends a profiling area.
  !! @param[in,out] this: Persistent data, not used in this case.
  subroutine PostEnd(this)

    implicit none

    class(profile_PSyDataType), target :: this
    integer(C_INT) :: range_id

    range_id = roctxRangePop()

  end subroutine PostEnd

  !> The finalise function would normally print the results. However, this
  !> is unnecessary for the ROCTx library so we do nothing.
  subroutine profile_PSyDataShutdown()
    implicit none
    return
  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
