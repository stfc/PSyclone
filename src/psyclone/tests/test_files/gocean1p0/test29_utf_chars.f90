! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council
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
! Author A. R. Porter, STFC Daresbury Lab

PROGRAM single_invoke_test

  ! Fake Fortran program for testing PSyclone when algorithm file and
  ! kernel file contain non-ascii characters. This is non-standard Fortran
  ! but most compilers are happy with it.
  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_utf_char_mod, only: kernel_utf_char
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: ufld, vfld, hfld

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  ufld = r2d_field(model_grid, GO_U_POINTS)
  vfld = r2d_field(model_grid, GO_V_POINTS)
  hfld = r2d_field(model_grid, GO_T_POINTS)

  ! Write statement containing a non-ascii char
  write(*,*) 'max reachable coeff. (at the Equator) for e1=1°)'

  call invoke( kernel_utf_char(ufld, vfld, hfld) )

END PROGRAM single_invoke_test
