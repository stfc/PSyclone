! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2025, Science and Technology Facilities Council
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
! -----------------------------------------------------------------------------
! Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

program single_invoke_annexed

  ! Description: f1 and f2 are written to over cells and then read. f1
  ! is on the w0 function space and f2 is on the w2 function space, so
  ! both are continuous and therefore have annexed dofs. By default
  ! loops over cells write to the level 1 halo in order to ensure that
  ! annexed dofs are correct (clean). Therefore halo exchanges will
  ! not be required so that for both f1 and f2 are clean when they are
  ! read.
  use constants_mod,        only: r_def
  use field_mod,            only: field_type
  use testkern_w3_mod,      only: testkern_w3_type
  use testkern_w2_only_mod, only: testkern_w2_only_type

  implicit none

  type(field_type) :: f1, f2, f3, f4, m1, m2
  real(r_def)      :: a

  call invoke(                             &
       ! update f1 locally
       setval_c(f1, 0.0),                  &
       ! update f2 in l1 halo
       testkern_w2_only_type(f2, f3),      &
       ! read f1 and f2 annexed dofs
       ! no halo exchange should be added for f2
       testkern_w3_type(a, f1, f2, m1, m2) &
          )

end program single_invoke_annexed
