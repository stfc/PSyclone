! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

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
