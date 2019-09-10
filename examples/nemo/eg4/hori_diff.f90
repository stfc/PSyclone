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
