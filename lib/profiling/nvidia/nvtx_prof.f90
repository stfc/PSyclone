! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019, Science and Technology Facilities Council
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

module profile_mod
  use iso_c_binding, only: C_CHAR, C_INT, C_INT16_T, C_INT64_T, C_PTR, &
       C_NULL_CHAR, C_LOC
  implicit none

  private

  !> The derived type passed to us from the profiled application. Required for
  !! consistency with the PSyclone Profiling interface and used here to
  !! prevent repeated string operations.
  type, public :: ProfileData
     !> Whether or not we've seen this region before
     logical :: initialised = .false.
     !> The colour assigned to this region
     integer :: colour_index = 1
     !> Name assigned to the region
     character(kind=C_CHAR, len=256) :: name = ""
  end type ProfileData

  ! The colour index of the last region created.
  integer, save :: last_colour = 0

  !> The number of colours in the list.
  integer, parameter :: NUM_COLOURS = 7

  !> List of colours to use for different regions.
  integer :: col(NUM_COLOURS) = [ Z'0000ff00', Z'000000ff', Z'00ffff00', &
                                  Z'00ff00ff', Z'0000ffff', Z'00ff0000', &
                                  Z'00ffffff']

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
     integer(C_INT):: messageType=1 !> NVTX_MESSAGE_TYPE_ASCII     = 1 
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

  ! Only the routines making up the PSyclone profiling API are public
  public ProfileInit, ProfileFinalise, ProfileStart, ProfileEnd

contains

  !> An optional initialisation subroutine. This is not used for the NVTX
  !! library.
  subroutine ProfileInit()
    implicit none
    return
  end subroutine ProfileInit

  !> Starts a profiling area. The module and region name can be used to create
  !! a unique name for each region.
  !! Parameters: 
  !! module_name:  Name of the module in which the region is
  !! region_name:  Name of the region (could be name of an invoke, or
  !!               subroutine name).
  !! profile_data: Persistent data - holds the selected colour and name of
  !!               this region.
  subroutine ProfileStart(module_name, region_name, profile_data)
    implicit none
    character*(*), intent(in) :: module_name, region_name
    type(ProfileData), target, intent(inout) :: profile_data
    ! Locals
    type(nvtxEventAttributes) :: event

    if(.not. profile_data%initialised)then
       ! This is the first time we've seen this region. Construct and
       ! save its name to save on future string operations.
       profile_data%initialised = .true.

       ! Round-robin the colour of each region created.
       if (last_colour < NUM_COLOURS) then
          last_colour = last_colour + 1
       else
          last_colour = 1
       end if
       profile_data%colour_index = last_colour

       profile_data%name = trim(module_name)//":"//trim(region_name) &
                           &   //C_NULL_CHAR
    end if
    
    event%color = col(profile_data%colour_index)
    event%message = c_loc(profile_data%name)
    
    call nvtxRangePushEx(event)

  end subroutine ProfileStart

  !> Ends a profiling area.
  !! profile_data: Persistent data, not used in this case.
  subroutine ProfileEnd(profile_data)
    implicit none
    type(ProfileData) :: profile_data
    
    call nvtxRangePop()
    
  end subroutine ProfileEnd

  !> The finalise function would normally print the results. However, this
  !> is unnecessary for the NVTX library so we do nothing.
  subroutine ProfileFinalise()
    implicit none
    return
  end subroutine ProfileFinalise

end module profile_mod
