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
  !! consistency with the PSyclone Profiling interface but not actually used
  !! in this wrapper.
  type, public :: ProfileData
     logical :: not_used = .TRUE.
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
  character(len=256), target :: tempName

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
  !! profile_data: Persistent data that can be used by the profiling library
  !!               but isn't in this case.
  subroutine ProfileStart(module_name, region_name, profile_data)
    implicit none
    character*(*), intent(in) :: module_name, region_name
    type(ProfileData) :: profile_data

    ! Round-robin the colour of each region created. Although nvtxStartRange
    ! does a mod() on the id, we don't want to risk overflow so we don't
    ! just blindly increment it.
    if (last_colour < NUM_COLOURS) then
       last_colour = last_colour + 1
    else
       last_colour = 1
    end if
    call nvtxStartRange(trim(module_name)//":"//region_name, &
                        id=last_colour)
    
  end subroutine ProfileStart

  !> Ends a profiling area.
  !! profile_data: Persistent data, not used in this case.
  subroutine ProfileEnd(profile_data)
    implicit none
    type(ProfileData) :: profile_data
    
    call nvtxEndRange()
    
  end subroutine ProfileEnd

  !> The finalise function would normally print the results. However, this
  !> is unnecessary for the NVTX library.
  subroutine ProfileFinalise()
    implicit none
    return
  end subroutine ProfileFinalise

  !> Wrapper routine to create an NVTX Range. If present, id is an index
  !! into the `col` array of colours.
  subroutine nvtxStartRange(name, id)
    implicit none
    !> The name of the region we are starting
    character(kind=C_CHAR, len=*), intent(in) :: name
    !> Which colour to use for this region (indexes into the `col` array)
    integer,             optional, intent(in) :: id
    ! Locals
    type(nvtxEventAttributes) :: event

    tempName = trim(name)//C_NULL_CHAR

    if ( .not. present(id)) then
       call nvtxRangePush(tempName)
    else
       event%color = col(mod(id, NUM_COLOURS) + 1)
       event%message = c_loc(tempName)
       call nvtxRangePushEx(event)
    end if
  end subroutine nvtxStartRange

  !> Wrapper routine to end the Range that's currently on top of
  !! the stack.
  subroutine nvtxEndRange
    implicit none
    call nvtxRangePop()
  end subroutine nvtxEndRange

end module profile_mod
