! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2024-2025, Science and Technology Facilities Council
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
!-------------------------------------------------------------------------------
! Author: A. R. Porter, STFC Daresbury Lab

program single_invoke

  ! Invokes four kernels, three of which include the halo in their iteration
  ! space.
  use constants_mod, only: r_def
  use field_mod,     only: field_type
  use testkern_mod,  only: testkern_type
  use testkern_halo_only_mod,  only: testkern_halo_only_type
  use testkern_halo_and_owned_mod, only: testkern_halo_and_owned_type
  implicit none

  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  integer :: hdepth, other_depth

  call invoke( testkern_halo_only_type(a, f1, f2, m1, m2, hdepth), &
               testkern_type(a, f1, f2, m1, m2), &
               testkern_halo_and_owned_type(a, f1, f2, m1, m2, hdepth), &
               testkern_halo_and_owned_type(a, f1, f2, m1, m2, other_depth))

end program single_invoke
