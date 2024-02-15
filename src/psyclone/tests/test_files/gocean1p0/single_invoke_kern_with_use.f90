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
! Author: A. R. Porter, STFC Daresbury Lab

program single_invoke_test

  ! Fake Fortran program for testing the use of PSyclone with a kernel
  ! that accesses a variable via a use statement.
  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_with_use_mod, only: kernel_with_use
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: oldu_fld, u_fld, cu_fld

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  oldu_fld = r2d_field(model_grid, GO_T_POINTS)
  u_fld = r2d_field(model_grid, GO_U_POINTS)
  cu_fld = r2d_field(model_grid, GO_U_POINTS)

  call invoke( kernel_with_use(oldu_fld, cu_fld, u_fld) )

end program single_invoke_test
