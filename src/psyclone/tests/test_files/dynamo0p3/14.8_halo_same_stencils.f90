! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017, Science and Technology Facilities Council
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
! Authors R. Ford and A. R. Porter, STFC Daresbury Lab

program halo_same_stencils

  ! Description: two stencil accesses in different kernels associated
  ! with the same field and therefore halo exchange when distributed
  ! memory is used. The stencils are of different types so region is
  ! returned to ensure that both stencil accesses are covered. This
  ! could be improved by noticing particular cases e.g. stencil_y +
  ! stencil_cross stays as stencil_cross (which would happen in this
  ! example). However, the halo exchange library does make use of this
  ! information at the moment in any case.
  use testkern_stencil_w3_mod, only: testkern_stencil_w3_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3
  integer :: f2_extent=2

  call invoke(                                   &
       set_field_scalar(0.0, f2),                &
       ! f1 is w3 and is written to
       ! f2 is w2 and is read with stencil cross
       ! f3 is w3 and is written to
       testkern_stencil_w3_type(f1,f2,f2_extent),   &
       testkern_stencil_w3_type(f3,f2,f2_extent)    &
          )

end program halo_same_stencils
