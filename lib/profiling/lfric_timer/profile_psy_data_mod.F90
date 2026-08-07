! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026, Australian Bureau of
!                         Meteorology
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> An implementation of the PSyData API for profiling, which wraps the use
!> of the LFRic timer code.

module profile_psy_data_mod

  implicit none

  type :: profile_PSyDataType

     character(:), allocatable :: name
     logical                   :: initialised = .false.

  contains

      ! The profiling API uses only the two following calls:
      procedure :: PreStart
      procedure :: PostEnd

  end type profile_PSyDataType

contains

  ! ---------------------------------------------------------------------------
  !> The initialisation subroutine. It is not called directly from
  !! any PSyclone created code, so a call to profile_PSyDataInit must be
  !! inserted manually by the developer.
  !!
  subroutine profile_PSyDataInit()

    use timer_mod, only : init_timer

    implicit none

    call init_timer()

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
  !!            written before the instrumented region, which is always 0 for
  !!            a profiling wrapper, and not  used.
  !! @param[in] num_post_vars The number of variables that are also declared
  !!            before an instrumented region of code, but are written after
  !!            this region, which is always 0 for a profiling wrapper, and
  !!            not used.
  subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                      num_post_vars)

    use timer_mod, only : timer

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character(len=*) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if (.not. this%initialised) then
      this%name = module_name//":"//region_name
      this%initialised = .true.
    endif

    call timer(this%name)

  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a PSyDataType type that corresponds to
  !! to the PreStart call.
  !! @param[in,out] this This PSyData instance.
  !
  subroutine PostEnd(this)

    use timer_mod, only : timer

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this

    call timer(this%name)

  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> Called at the end of the execution of a program, usually to generate
  !! all output for the profiling library. Calls ``output_timer`` in the
  !! LFRic timer code.
  subroutine profile_PSyDataShutdown()

    use timer_mod, only : output_timer

    implicit none

    call output_timer()

  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
