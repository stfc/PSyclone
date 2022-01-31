! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022, Science and Technology Facilities Council.
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
! Author S. Siso, STFC Daresbury Lab

program implicit_do_structures
  use my_mod, only: mystruct
  implicit none
  integer, parameter :: jpi=10, jpj=10, jpk=10
  real(kind=kind(1.0d0)), dimension(jpi,jpj,jpk) :: umask
  integer, dimension(jpj) :: indices = 3

  ! Test code with implicit do loop with structures
  umask(:,:,:) = mystruct%field(:,:,:) + mystruct%field2%field(:,:,:)

  ! Test code with implicit do loop with structures in the LHS
  mystruct%field2%field(:,:,:) = 0.0d0

  ! Test code with implicit do loop with AoSoA in the LHS
  mystruct%field3(:,:,:)%field4 = 0.0d0

  ! Test code mixing AoSoA in the LHS and SoA in the RHS
  mystruct%field3(:,:,:)%field4 = mystruct%field2%field(:,:,:)

  ! Test code with implicit do loop with 2 arrays in the LHS structure, one
  ! without ranges
  mystruct%field2(4, 3)%field(:,:,:) = 0.0d0

  ! Test code with implicit do loop with 2 arrays in the LHS structure, both
  ! with ranges
  mystruct%field2(:,:)%field(:,:,:) = 0.0d0

  ! Test code with implicit do loop with range in a nested array
  mystruct%field5(indices(:)) = 0.0d0

  ! Test code with implicit with nested structures of arrays
  umask(:,mystruct%field2%field3(:),:) = mystruct%field(mystruct%field2%field3(:),:,:)

end program implicit_do_structures
