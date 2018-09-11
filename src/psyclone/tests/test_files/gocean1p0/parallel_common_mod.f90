!------------------------------------------------------------------------------
! BSD 2-Clause License
! 
! Copyright (c) 2018, Science and Technology Facilities Council.
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

!> Module holding elements of the parallel infrastructure that
!! are independent of whether or not we are building with MPI.
module parallel_common_mod
  implicit none

  ! Everything in this module is public
  public

  !> MPI rank + 1 of current process
  integer :: rank
  !> Total no. of MPI processes
  integer :: nranks
  !> The dimensions of the (regular) processor grid
  integer :: nprocx, nprocy

contains

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

  subroutine set_proc_grid(nx, ny)
    !> Setter for the dimensions of the processor grid
    integer, intent(in) :: nx, ny
    nprocx = nx
    nprocy = ny
  end subroutine set_proc_grid

  !================================================

  subroutine get_proc_grid(nx, ny)
    !> Getter for the dimensions of the processor grid
    integer, intent(out) :: nx, ny
    nx = nprocx
    ny = nprocy
  end subroutine get_proc_grid

end module parallel_common_mod
