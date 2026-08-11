! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Illustration of copying one field to another with some form of
! staggering. This is a Fortran implementation of the Dawn Python
! example.
program copy_stencil
  implicit none
  integer, parameter :: n=10
  integer :: i,j,k
  real, dimension(n+1,n,n) :: out,in
  do k=1,n
     do j=1,n
        do i=1,n
           out(i,j,k) = in(i+1,j,k)
        end do
     end do
  end do
end program copy_stencil
