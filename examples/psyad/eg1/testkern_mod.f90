! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
! ------------------------------------------------------------------------------
! Author: A. R. Porter, STFC Daresbury Lab
! Modified by: R. W. Ford, STFC Daresbury Lab

! Example module containing a very simple kernel subroutine that
! contains assignments within a loop over array elements.

module testkern_mod
  implicit none

contains

  subroutine testkern_code(ascalar, field1, field2, field3, npts)
    real, intent(in) :: ascalar
    integer, intent(in) :: npts
    real, intent(inout), dimension(npts) :: field2
    ! Deliberately leave off the intent for 'field1' to demonstrate that
    ! it is correctly set in the generated adjoint code.
    real, dimension(npts) :: field1
    ! This argument must be made intent(inout) in the adjoint
    real, intent(in), dimension(npts) :: field3
    ! Locals
    real :: tmp, tmp2, tmp3
    integer :: i

    tmp = ascalar*ascalar
    do i=1,npts
       tmp2 = tmp*i
       field1(i) = tmp*field1(i) + field2(i) + field3(i)
       tmp3 = tmp2*3.0
       field2(i) = field2(i) + field1(i)/tmp2
    end do
    field2(npts) = field2(npts) + field1(1)
    field2(:) = field1(:) - tmp*field3(:)

  end subroutine testkern_code
  
end module testkern_mod
