! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018-2024, Science and Technology Facilities Council
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
! -----------------------------------------------------------------------------
! Author R. W. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office

program halo_inc_times3

  ! Description: three kernel calls where the associated loops iterate
  ! over cells. Each kernel call increments a continuous field and
  ! reads from a contrinuous field. Field f1 has gh_inc to gh_read and
  ! gh_read to gh_inc dependencies. Field f3 has unknown to gh_inc and
  ! gh_inc to unknown dependencies.

  use field_mod,       only: field_type
  use testkern_w0_mod, only: testkern_w0_type

  implicit none

  type(field_type) :: f1, f2, f3, f4

  call invoke(                    &
       testkern_w0_type(f1, f2),  &
       testkern_w0_type(f3, f1),  &
       testkern_w0_type(f1, f4)   &
          )

end program halo_inc_times3
