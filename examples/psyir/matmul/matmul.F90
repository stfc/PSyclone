! -----------------------------------------------------------------------------
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
! a simple subroutine for matrix multiplication. It generates a random 2D
! matrix with the specified dimensions, calls the subroutine, and checks
! that the output is as expected (i.e., equivalent to the output of
! Fortran's matmul intrinsic). It outputs the time taken for the
! subroutine to complete.

program matmul_example
  use omp_lib

  implicit none

  interface
    subroutine my_matmul(a, b, c)
      integer, intent(in) :: a(:,:), b(:,:)
      integer, intent(out) :: c(:,:)
    end subroutine
  end interface

  ! Matrix dimensions 
  integer, parameter :: n = 1500
  integer, parameter :: m = 1400
  integer, parameter :: p = 1300

  ! Input and output (transposed) matrices
  integer, dimension(:, :), allocatable :: a, b, c, gold

  ! Local variables
  integer :: x, y, k, acc
  real :: r

  ! Timing
  real(kind=8) :: start, fin

  ! Check correctness
  logical :: ok

  allocate(a(n, m))
  allocate(b(p, n))
  allocate(c(p, m))
  allocate(gold(p, m))

  ! Initialise first input matrix
  do y = 1, m
    do x = 1, n
      call random_number(r)
      a(x, y) = int(r * 100)
    end do
  end do

  ! Initialise second input matrix
  do y = 1, n
    do x = 1, p
      call random_number(r)
      b(x, y) = int(r * 100)
    end do
  end do

  start = omp_get_wtime()
  call my_matmul(a, b, c)
  fin = omp_get_wtime()

  ! Check result against built-in matmul
  ok = .true.
  gold = matmul(b, a)
  do y = 1, m
    do x = 1, p
      ok = ok .and. c(x, y) == gold(x, y)
    end do
  end do

  ! Report success/fail and run time
  if (ok) then
    print "('Passed', f8.3, 's')", fin - start
  else
    print *, "Failed"
  end if
end program matmul_example

subroutine my_matmul(a, b, c)
  integer, intent(in) :: a(:,:), b(:,:)
  integer, intent(out) :: c(:,:)
  integer :: x, y, k
  c(:, :) = 0
  do y = 1, size(a, 2)
    do x = 1, size(b, 1)
      do k = 1, size(a, 1)
        c(x, y) = c(x, y) + a(k, y) * b(x, k)
      end do
    end do
  end do
end subroutine
