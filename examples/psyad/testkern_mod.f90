! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021, Science and Technology Facilities Council.
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

! Example module containing a very simple, one-line kernel subroutine.

module testkern_mod
  implicit none

contains

  subroutine testkern_code(ascalar, field1, field2, npts)
    real, intent(in) :: ascalar
    integer, intent(in) :: npts
    ! issue #1429. Active variables need to be declared as inout as
    ! the intents can change in the adjoint version and PSyclone does
    ! not currently deal with this.
    real, intent(inout), dimension(npts) :: field2
    real, intent(inout), dimension(npts) :: field1
    real :: tmp, tmp2

    ! issue #1430. Array notation does not work with the assignment
    ! transformation so temporarily change the assignment to a single
    ! index, e.g. previously field1(:) = ascalar*field1(:) + field2(:)
    tmp = ascalar*ascalar
    field1(1) = tmp*field1(1) + field2(1)
    tmp2 = tmp*3.0
    field2(1) = field2(1) + field1(1)/tmp2
  end subroutine testkern_code
  
end module testkern_mod
