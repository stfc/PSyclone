! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


!> An implementation of the PSyData API for profiling, which wraps the use
!> of the dl_timer library (https://github.com/stfc/dl_timer).

module profile_psy_data_mod

  use psy_data_base_mod, only : PSyDataBaseType, profile_PSyDataStart, &
                                profile_PSyDataStop, is_enabled

  implicit none

  type, extends(PSyDataBaseType) :: profile_PSyDataType
      integer                   :: timer_index
      logical                   :: registered = .false.
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

    use dl_timer, only : timer_init

    implicit none

    call timer_init()

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

    use dl_timer, only : timer_register, timer_start

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character(*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if ( .not. this%registered) then
       call this%PSyDataBaseType%PreStart(module_name, region_name, &
                                          num_pre_vars, num_post_vars)
       call timer_register(this%timer_index, &
                           label=module_name//":"//region_name)
       this%registered = .true.
    endif
    if (is_enabled) call timer_start(this%timer_index)

  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a PSyDataType type that corresponds to
  !! to the PreStart call.
  !! @param[in,out] this This PSyData instance.
  !
  subroutine PostEnd(this)

    use dl_timer, only : timer_stop

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this

    if (is_enabled) call timer_stop(this%timer_index)

  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> Called at the end of the execution of a program, usually to generate
  !! all output for the profiling library. Calls timer_report in dl_timer.
  subroutine profile_PSyDataShutdown()

    use dl_timer, only : timer_report

    implicit none

    call timer_report()

  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
