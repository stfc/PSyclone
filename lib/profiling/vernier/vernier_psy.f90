! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------


!> An implementation of the PSyData API for profiling which wraps the use of Vernier.

module profile_psy_data_mod

  ! The Vernier handle type
  use vernier_mod, only : vik
  implicit none

  type :: profile_PSyDataType
     ! The opaque Vernier handle for a specific region
     integer (kind=vik) :: vernier_handle
     ! The name of the subroutine and module to be used by Vernier
     character(:), allocatable :: name
     ! True if this instance of PSyDataType has the name already
     ! initialised. This way the copy of subroutine name is only
     ! done first time PreStart is called.
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

  subroutine profile_PSyDataInit()
    use vernier_mod, only: vernier_init
        implicit none
        call vernier_init(1)
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

    use vernier_mod, only : vernier_start
    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character(len=*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if (.not. this%initialised) then
      ! Venier only supports a single name, so we store the concatenated
      ! strings to reduce runtime overhead
      this%name = module_name//":"//region_name
      this%initialised = .true.
    endif
    call vernier_start(this%vernier_handle, this%name)

  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !! Ends a profiling area. It takes a PSyDataType type that corresponds to
  !! to the PreStart call.
  !! @param[in,out] this This PSyData instance.
  !
  subroutine PostEnd(this)

    use vernier_mod, only : vernier_stop

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this

    call vernier_stop(this%vernier_handle)

  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> Called at the end of the execution of a program, usually to generate
  !! all output for the profiling library.
  subroutine profile_PSyDataShutdown()
    use vernier_mod, only : vernier_finalize, vernier_write

    implicit none
    call vernier_write()
    call vernier_finalize()
  end subroutine profile_PSyDataShutdown

  ! ---------------------------------------------------------------------------
  !> Enable Vernier.
  subroutine profile_PSyDataStart()
    implicit none
  end subroutine profile_PSyDataStart

  ! ---------------------------------------------------------------------------
  !> Disable Vernier.
  subroutine profile_PSyDataStop()
    implicit none
  end subroutine profile_PSyDataStop

end module profile_psy_data_mod
