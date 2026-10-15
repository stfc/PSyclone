! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Illustration of performing a tri-diagonal solve in the vertical (k
! dimension). This is a Fortran implementation of the Dawn Python
! example.
program tridiagonal_solve
  implicit none
  integer, parameter :: n=10
  integer :: i,j,k
  real :: m
  real, dimension(n,n,n) :: a,b,c,d
  do k=1,n
     do j=1,n
        do i=1,n
           c(i,j,k) = c(i,j,k)/b(i,j,k)
        end do
     end do
  end do
  do k=2,n
     do j=1,n
        do i=1,n
           m = 1.0/(b(i,j,k)-a(i,j,k)*c(i,j,k-1))
           c(i,j,k) = c(i,j,k)*m
           d(i,j,k) = (d(i,j,k)-a(i,j,k)*d(i,j,k-1))*m
        end do
     end do
  end do  
  do k=n-1,1,-1
     do j=1,n
        do i=1,n
           d(i,j,k) = d(i,j,k) - c(i,j,k)*d(i,j,k+1)
        end do
     end do
  end do
  
end program tridiagonal_solve
