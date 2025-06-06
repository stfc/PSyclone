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
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

program multiple_stencil
  ! Description: multiple kernel calls with the same and different
  ! extent and direction names.
  use constants_mod,                only: i_def
  use field_mod,                    only: field_type
  use flux_direction_mod,           only: x_direction, y_direction
  use testkern_stencil_xory1d_mod,  only: testkern_stencil_xory1d_type
  use testkern_stencil_multi_mod,   only: testkern_stencil_multi_type
  use testkern_stencil_multi_2_mod, only: testkern_stencil_multi_2_type

  implicit none

  type(field_type) :: f1, f2, f3, f4
  integer(i_def)   :: extent = 2,    &
                      f2_extent = 1, &
                      f3_extent = 1
  integer(i_def)   :: direction = y_direction, &
                      f3_direction = x_direction

  call invoke(                                                   &
        testkern_stencil_xory1d_type(f1,                         &
                                     f2, f2_extent, x_direction, &
                                     f3, f4),                    &
        testkern_stencil_multi_type(f1, f2, f2_extent,           &
                                    f3, f3_extent, f3_direction, &
                                    f4, 1),                      &
        testkern_stencil_multi_2_type(f1,                        &
                                      f2, extent, direction,     &
                                      f3, extent, direction,     &
                                      f4, extent, direction)     &
       )

end program multiple_stencil
