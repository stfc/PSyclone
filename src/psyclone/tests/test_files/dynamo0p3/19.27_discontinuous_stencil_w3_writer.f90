! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022, Science and Technology Facilities Council
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
! Author I. Kavcic, Met Office

program discontinuous_stencil_w3_writer
  ! Description: Check stencil depth list when a kernel call with discontinuous
  ! stencil readers is followed by a kernel call with a discontinuous writer in
  ! the same invoke. Also check the reverse order of kernel calls.

  use constants_mod,      only: i_def, r_solver
  use r_solver_field_mod, only: r_solver_field_type
  use testkern_w3_mod,    only: testkern_w3_type
  use testkern_different_any_dscnt_space_stencil_mod, &
                          only: testkern_different_any_dscnt_space_stencil_type

  implicit none

  type(r_solver_field_type) :: f1, f2, f3, f4
  real(r_solver)            :: a
  integer(i_def)            :: extent = 2

  call invoke(name="stencils_first",                                &
       testkern_different_any_dscnt_space_stencil_type(f1,          &
                                                       f2, extent,  &
                                                       f4, extent), &
       testkern_w3_type(a, f1, f2, f3, f4)                          &
       )

  call invoke(name="stencils_second",                               &
       testkern_w3_type(a, f1, f2, f3, f4),                         &
       testkern_different_any_dscnt_space_stencil_type(f1,          &
                                                       f2, extent,  &
                                                       f4, extent)  &
       )

end program discontinuous_stencil_w3_writer
