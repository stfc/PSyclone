!------------------------------------------------------------------------------
! BSD 2-Clause License
! 
! Copyright (c) 2018-2020, Science and Technology Facilities Council.
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
!------------------------------------------------------------------------------
! Author: A. R. Porter, STFC Daresbury Laboratory

!> Stub implementation to be used in place of parallel_utils_mod when NOT
!! compiling with MPI
module parallel_utils_mod
  use kind_params_mod, only: go_wp
  implicit none

  private

  !> MPI rank + 1 of current process
  integer :: rank
  !> Total no. of MPI processes
  integer :: nranks

  integer, parameter :: MSG_UNDEFINED = -99
  integer, parameter :: MSG_REQUEST_NULL = 0
  logical, parameter :: DIST_MEM_ENABLED = .False.

  public parallel_init, parallel_finalise, parallel_abort
  public get_rank, get_num_ranks, get_max_tag, gather
  public msg_wait, msg_wait_all, post_receive, post_send, global_sum
  public MSG_UNDEFINED, MSG_REQUEST_NULL, DIST_MEM_ENABLED

contains

  !================================================

  !> Fake parallel initialisation routine. Does nothing apart from
  !! initialise number of ranks to 1 and the rank of this process to 1.
  subroutine parallel_init()

    write (*,*) "parallel_init: Not running with MPI"
    rank = 1
    nranks = 1

  end subroutine parallel_init

  !================================================

  !> Empty routine. For compatibility with parallel_mod.
  subroutine parallel_finalise()
  end subroutine parallel_finalise
  
  !================================================

  integer function get_max_tag()
    !> Stub for function that returns maximum available MPI tag value.
    get_max_tag = 1
  end function get_max_tag
    
  !================================================

  subroutine msg_wait(nmsg, flags, irecv)
    integer, intent(in) :: nmsg
    integer, dimension(:), intent(inout) :: flags
    integer, intent(out) :: irecv
    call parallel_abort('msg_wait should not be called in a serial build')
  end subroutine msg_wait
    
  !================================================

  subroutine msg_wait_all(nmsg, flags)
    integer, intent(in) :: nmsg
    integer, dimension(:), intent(inout) :: flags
    call parallel_abort('msg_wait should not be called in a serial build')
  end subroutine msg_wait_all
  
  !================================================

  subroutine post_receive(nrecv, source, tag, exch_flag, rbuff, ibuff)
    real(kind=go_wp), dimension(:), optional, intent(inout) :: rbuff
    integer, dimension(:), optional, intent(inout) :: ibuff
    integer, intent(in) :: nrecv
    integer, intent(in) :: tag
    integer, intent(in) :: source
    integer :: exch_flag
    call parallel_abort('post_receive should not be called in a serial build')
  end subroutine post_receive
    
  !================================================
  
  subroutine post_send(sendBuff,nsend,destination,tag, &
                       exch_flag)
    integer, intent(in) :: nsend, destination
    real(kind=go_wp), dimension(nsend), intent(in) :: sendBuff
    integer :: tag ! intent is?
    integer :: exch_flag ! intent is??
    call parallel_abort('post_send should not be called in a serial build')
  end subroutine post_send
  
  !================================================

  !> Stop a serial model run. Prints supplied msg to stderr and calls stop
  !! @param[in] msg Message to print to stderr - reason we're stopping
  subroutine parallel_abort(msg)
    use iso_fortran_env, only : error_unit ! access computing environment
    character(len=*), intent(in) :: msg

    write(error_unit, *) msg
    stop

  end subroutine parallel_abort
  
  !================================================

  function get_rank()
    integer :: get_rank
    get_rank = rank
  end function get_rank

  !================================================

  function get_num_ranks() result(num)
    integer :: num
    num = nranks
  end function get_num_ranks

  !================================================

  subroutine global_sum(var)
    real(go_wp), intent(inout) :: var
  end subroutine global_sum

  !================================================

  subroutine gather(send_buffer, recv_buffer)
    !> Gathers the data in the send buffer from all
    !> processes into the receive buffer.
    real(go_wp), dimension(:) :: send_buffer, recv_buffer

    recv_buffer = send_buffer

  end subroutine gather


end module parallel_utils_mod
