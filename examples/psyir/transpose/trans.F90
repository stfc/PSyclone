! BSD 3-Clause License
!
! Copyright (c) 2025, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: M. Naylor, University of Cambridge, UK

! This a program to demonstrate PSyclone's loop tiling transformation on
! a simple subroutine for matrix transposition. It generates a random 2D
! matrix with the specified dimensions, calls the subroutine, and checks
! that the output is as expected (i.e., equivalent to the output of
! Fortran's transpose intrinsic). It outputs the time taken for the
! subroutine to complete.

program trans_example
  use omp_lib

  implicit none

  interface
    subroutine trans(m_in, m_out)
      real, intent(in) :: m_in(:,:)
      real, intent(out) :: m_out(:,:)
    end subroutine
  end interface

  ! Matrix dimensions 
  integer, parameter :: dim_x = 10000
  integer, parameter :: dim_y = 9000

  ! Input and output (transposed) matrices
  real, dimension(:,:), allocatable :: m, m_t, gold

  ! Loop variables
  integer :: x, y

  ! Timing
  real(kind=8) :: start, fin

  ! Check correctness
  logical :: ok

  allocate(m(dim_x, dim_y))
  allocate(m_t(dim_y, dim_x))
  allocate(gold(dim_y, dim_x))

  ! Initialise input matrix
  call random_number(m)

  start = omp_get_wtime()
  call trans(m, m_t)
  fin = omp_get_wtime()

  ! Check result
  gold = transpose(m)
  ok = .true.
  do x = 1, dim_x
    do y = 1, dim_y
      ok = ok .and. m_t(y, x) == gold(y, x)
    end do
  end do

  ! Report success/fail and run time
  if (ok) then
    print "('Passed', f7.4, 's')", fin - start
  else
    print *, "Failed"
  end if
end program trans_example

subroutine trans(m_in, m_out)
  implicit none

  real, intent(in) :: m_in(:,:)
  real, intent(out) :: m_out(:,:)
  integer :: x, y

  do y = 1, size(m_in, 2)
    do x = 1, size(m_in, 1)
      m_out(y, x) = m_in(x, y)
    end do
  end do
end subroutine
