!------------------------------------------------------------------------------
! BSD 2-Clause License
! 
! Copyright (c) 2018, Science and Technology Facilities Council
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

!> Stub implementation to be used in place of parallel_mod when NOT
!! compiling with MPI
module parallel_mod
  use parallel_common_mod, only: get_rank, get_num_ranks,      &
                                 set_proc_grid, get_proc_grid, &
                                 nranks, rank, nprocx, nprocy
  implicit none

  private

  public parallel_init, parallel_finalise, parallel_abort
  ! Make selected routines from parallel_common available to
  !! USE'rs of this module.
  public get_num_ranks, get_rank
  public set_proc_grid, get_proc_grid

contains

  !================================================

  !> Fake parallel initialisation routine. Does nothing apart from
  !! initialise number of ranks to 1 and the rank of this process to 1.
  subroutine parallel_init()

    write (*,*) "parallel_init: Not running with MPI"
    rank = 1
    nranks = 1
    nprocx = 1
    nprocy = 1

  end subroutine parallel_init

  !================================================

  !> Empty routine. For compatibility with parallel_mod.
  subroutine parallel_finalise()
  end subroutine parallel_finalise

  !================================================

  !> Stop a serial model run. Prints supplied msg to stderr and calls stop
  !! @param[in] msg Message to print to stderr - reason we're stopping
  subroutine parallel_abort(msg)
    use iso_fortran_env, only : error_unit ! access computing environment
    character(len=*), intent(in) :: msg

    write(error_unit, *) msg
    stop

  end subroutine parallel_abort

end module parallel_mod
