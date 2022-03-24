! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
! Author A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office
! Modified N. Nobre, STFC Daresbury Lab

module profile_psy_data_mod

  use iso_c_binding, only : C_CHAR, C_INT, C_INT16_T, C_INT64_T, C_PTR, &
       C_NULL_CHAR, C_LOC

  implicit none

  private

  !> The derived type passed to us from the profiled application. Required for
  !! consistency with the PSyclone Profiling interface and used here to
  !! store the colour assigned to a region and to prevent repeated string
  !! operations.
  type, public :: profile_PSyDataType
     !> Whether or not we've seen this region before
     logical :: initialised = .false.
     !> The colour assigned to this region
     integer :: colour_index = 1
     !> Name assigned to the region
     character(kind=C_CHAR, len=256) :: name = ""
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart
      procedure :: PostEnd
  end type profile_PSyDataType

  ! The colour index of the last region created.
  integer, save :: last_colour = 0

  !> The number of colours in the list.
  integer, parameter :: NUM_COLOURS = 7

  !> List of colours to use for different regions.
  integer :: col(NUM_COLOURS) = [ int(Z'0000ff00'), int(Z'000000ff'), int(Z'00ffff00'), &
                                  int(Z'00ff00ff'), int(Z'0000ffff'), int(Z'00ff0000'), &
                                  int(Z'00ffffff')]

  !> Holds the name of the region being created
  character(kind=C_CHAR, len=256), target :: tempName

  !> Struct passed into nvtxRangePushEx
  type, bind(C):: nvtxEventAttributes
     integer(C_INT16_T):: version=1
     integer(C_INT16_T):: size=48
     integer(C_INT):: category=0
     integer(C_INT):: colorType=1   !> NVTX_COLOR_ARGB = 1
     integer(C_INT):: color
     integer(C_INT):: payloadType=0 !> NVTX_PAYLOAD_UNKNOWN = 0
     integer(C_INT):: reserved0
     integer(C_INT64_T):: payload   !> union uint,int,double
     integer(C_INT):: messageType=1 !> NVTX_MESSAGE_TYPE_ASCII = 1
     type(C_PTR):: message          !> ASCII char
  end type nvtxEventAttributes

  interface nvtxRangePush
     ! Push range with custom label and standard colour
     subroutine nvtxRangePushA(name) bind(C, name='nvtxRangePushA')
       use iso_c_binding
       ! Have to use length of 1 because routine is BIND(C)
       character(kind=C_CHAR, len=1) :: name
     end subroutine nvtxRangePushA

     ! Push range with custom label and custom colour
     subroutine nvtxRangePushEx(event) bind(C, name='nvtxRangePushEx')
       use iso_c_binding
       import :: nvtxEventAttributes
       type(nvtxEventAttributes) :: event
     end subroutine nvtxRangePushEx
  end interface nvtxRangePush

  interface nvtxRangePop
     subroutine nvtxRangePop() bind(C, name='nvtxRangePop')
     end subroutine nvtxRangePop
  end interface nvtxRangePop

  ! Requires that the application be linked with CUDA (-Mcuda flag to
  ! PGI)
  interface cudaProfilerStart
     subroutine cudaProfilerStart() bind(C, name='cudaProfilerStart')
     end subroutine cudaProfilerStart
  end interface cudaProfilerStart

  ! Requires that the application be linked with CUDA (-Mcuda flag to
  ! PGI)
  interface cudaProfilerStop
     subroutine cudaProfilerStop() bind(C, name='cudaProfilerStop')
     end subroutine cudaProfilerStop
  end interface cudaProfilerStop

  ! Only the routines making up the PSyclone profiling API are public
  public profile_PSyDataInit, profile_PSyDataShutdown, &
       profile_PSyDataStart, profile_PSyDataStop

contains

  !> An optional initialisation subroutine. This is not used for the NVTX
  !! library.
  subroutine profile_PSyDataInit()
    implicit none
    return
  end subroutine profile_PSyDataInit

  !> Enables profiling (if it is not already enabled). May be manually added
  !! to source code in order to limit the amount of profiling performed at
  !! run time. Requires that the application be linked with CUDA (-Mcuda flag
  !! to PGI).
  subroutine profile_PSyDataStart()
    implicit none
    call cudaProfilerStart()
  end subroutine profile_PSyDataStart

  !> Turns off profiling. All subsequent calls to the profiling API
  !! will have no effect. Use in combination with profile_PSyDataStart() to
  !! limit the amount of profiling performed at runtime. Requires that the
  !! application be linked with CUDA (-Mcuda flag to PGI).
  subroutine profile_PSyDataStop()
    implicit none
    call cudaProfilerStop()
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
    type(nvtxEventAttributes) :: event

    if (.not. this%initialised) then
       ! This is the first time we've seen this region. Construct and
       ! save its name to save on future string operations.
       this%initialised = .true.

       ! Round-robin the colour of each region created.
       if (last_colour < NUM_COLOURS) then
          last_colour = last_colour + 1
       else
          last_colour = 1
       end if
       this%colour_index = last_colour

       this%name = trim(module_name)//":"//trim(region_name) &
                           &   //C_NULL_CHAR
    end if

    event%color = col(this%colour_index)

    event%message = c_loc(this%name)

    call nvtxRangePushEx(event)

  end subroutine PreStart

  !> Ends a profiling area.
  !! @param[in,out] this: Persistent data, not used in this case.
  subroutine PostEnd(this)

    implicit none

    class(profile_PSyDataType), target :: this

    call nvtxRangePop()

  end subroutine PostEnd

  !> The finalise function would normally print the results. However, this
  !> is unnecessary for the NVTX library so we do nothing.
  subroutine profile_PSyDataShutdown()
    implicit none
    return
  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
