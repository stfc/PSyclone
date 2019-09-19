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
! Author R. W. Ford, STFC Daresbury Lab

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
