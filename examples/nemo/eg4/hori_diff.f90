! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Illustration of computing horizontal diffusion using the
! laplacian. This is a Fortran implementation of the Dawn Python
! example.
program hori_diff
  implicit none
  integer, parameter :: n=10
  integer :: i,j,k
  real, dimension(0:n+1,0:n+1,0:n+1) :: lap,fin,coeff,fout
  do k=1,n
     do j=1,n
        do i=1,n
           lap(i,j,k)=(-4.0)*fin(i,j,k)+coeff(i,j,k)*(fin(i+1,j,k)+fin(i-1,j,k)+fin(i,j+1,k)+fin(i,j-1,k))
           fout(i,j,k)=(-4.0)*lap(i,j,k)+coeff(i,j,k)*(lap(i+1,j,k)+lap(i-1,j,k)+lap(i,j+1,k)+lap(i,j-1,k))
        end do
     end do
  end do
end program hori_diff
