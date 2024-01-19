! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council
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
! Modified I. Kavcic, Met Office

program multiple_stencils
  ! Description: multiple stencils specified with the same direction
  ! provided in two cases and a different direction in the third.
  ! Note: it is currently not possible to specify kind for an integer
  ! literal stencil depth in a kernel call. This will be enabled when
  ! addressing issue #753.
  use field_mod,                   only: field_type
  use flux_direction_mod,          only: x_direction, y_direction
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type

  type(field_type) :: f1, f2, f3, f4

  call invoke(                                                       &
       testkern_stencil_xory1d_type(f1, f2, 2, x_direction, f3, f4), &
       testkern_stencil_xory1d_type(f1, f2, 2, x_direction, f3, f4), &
       testkern_stencil_xory1d_type(f1, f2, 2, y_direction, f3, f4)  &
       )

end program multiple_stencils
