! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
! Author: J. Henrichs, Bureau of Meteorology
! Modified: I. Kavcic, Met Office

module profile_psy_data_mod

  use psy_data_base_mod, only : PSyDataBaseType, profile_PSyDataStart, &
                                profile_PSyDataStop, is_enabled

  implicit none

  type, extends(PSyDataBaseType) :: profile_PSyDataType
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart
      procedure :: PostEnd
  end type profile_PSyDataType

  logical :: has_been_initialised = .false.

contains

  ! ---------------------------------------------------------------------------
  !> An optional initialisation subroutine. It is not called directly from
  !> any PSyclone created code, but for most existing profiling libraries the
  !> application will have to call it. In this dummy library it is called once
  !> from ProfileStart.
  subroutine profile_PSyDataInit()

    implicit none

    if (is_enabled) then
       write(*,*) "profile_PSyDataInit called"
    else
       write(*,*) "profile_PSyDataInit called, but profiling is disabled"
    endif
    has_been_initialised = .true.

  end subroutine profile_PSyDataInit

  ! ---------------------------------------------------------------------------
  !> Starts a profiling area. The module and region name can be used to create
  !! a unique name for each region.
  !! Parameters:
  !! @param[in,out] this This PSyData instance.
  !! @param[in] module_name Name of the module in which the region is.
  !! @param[in] region_name Name of the region (could be name of an invoke,
  !!                        or subroutine name).
  !! @param[in] num_pre_vars The number of variables that are declared and
  !!                         written before the instrumented region.
  !! @param[in] num_post_vars The number of variables that are also declared
  !!                          before an instrumented region of code, but are
  !!                          written after this region.
  subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                      num_post_vars)

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this
    character(len=*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if ( .not. has_been_initialised ) then
       call profile_PSyDataInit()
    endif

    call this%PSyDataBaseType%PreStart(module_name, region_name, 0, 0)
    if (is_enabled) then
       write(*,*) "PreStart called for module '", module_name,  &
                  "' region '", region_name, "'"
    else
       write(*,*) "PreStart called for module '", module_name,  &
                  "' region '", region_name, "', but profiling is disabled"
    endif

  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a ProfileData type that corresponds to
  !> to the ProfileStart call.
  !> @param[in,out] this This PSyData instance.
  !
  subroutine PostEnd(this)

    implicit none

    class(profile_PSyDataType), intent(inout), target :: this

    if (is_enabled) then
       write(*,*) "PostEnd called for module '", trim(this%module_name), &
                  "' region '", trim(this%region_name), "'"
    else
       write(*,*) "PostEnd called for module '", trim(this%module_name), &
                  "' region '", trim(this%region_name), "', but profiling is disabled"
    endif

  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> The finalise function prints the results. This subroutine must be called
  !> for most profiling libraries, otherwise no results will be produced.
  !> This call must be added to the program explicitly, it is not called
  !> automatically.
  subroutine profile_PSyDataShutdown()

    implicit none

    write(*,*) "profile_PSyDataShutdown called"

  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
