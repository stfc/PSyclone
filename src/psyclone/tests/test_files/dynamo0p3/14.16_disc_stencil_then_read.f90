! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022-2024, Science and Technology Facilities Council
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
! Author R. W. Ford, STFC Daresbury Lab

program stencil_then_read

  ! Test that a discontinuous stencil 'read' (which happens to be
  ! 'any_discontinuous_space' but does not matter) followed by a
  ! discontinuous read (which happens to be 'readwrite' but does not
  ! matter) results in correct halo exchange declarations. The example
  ! is field f4. There was a bug here before this test and fix was
  ! added.

  use constants_mod, only: r_def
  use field_mod,     only: field_type

  use testkern_different_any_dscnt_space_stencil_mod, only : &
                     testkern_different_any_dscnt_space_stencil_type
  use testkern_w3_mod, only : testkern_w3_type

  type(field_type) :: f1, f2, f3, f4
  real(r_def)      :: a, extent

  call invoke(                                                     &
      testkern_different_any_dscnt_space_stencil_type(f1,          &
                                                      f2, extent,  &
                                                      f4, extent), &
      testkern_w3_type(a, f1, f2, f3, f4)                          &
      )

end program stencil_then_read
