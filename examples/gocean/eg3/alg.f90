! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab.

! A simple, single Invoke example to demonstrate the generation of an
! OpenCL driver PSy layer.
program simple

  use grid_mod
  use field_mod
  use compute_cu_mod,   only: compute_cu
  use compute_cv_mod,   only: compute_cv
  use compute_z_mod,    only: compute_z
  use compute_h_mod,    only: compute_h
  implicit none

  type(grid_type), target :: model_grid

  type(r2d_field) :: p_fld
  type(r2d_field) :: u_fld, v_fld
  type(r2d_field) :: cu_fld, cv_fld
  type(r2d_field) :: z_fld
  type(r2d_field) :: h_fld

  integer :: ncycle

  model_grid = grid_type(GO_ARAKAWA_C,                                 &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/), &
                         GO_OFFSET_SW)

  ! Create fields on this grid
  p_fld  = r2d_field(model_grid, T_POINTS)
  u_fld  = r2d_field(model_grid, U_POINTS)
  v_fld  = r2d_field(model_grid, V_POINTS)
  cu_fld = r2d_field(model_grid, U_POINTS)
  cv_fld = r2d_field(model_grid, V_POINTS)
  z_fld = r2d_field(model_grid, F_POINTS)
  h_fld = r2d_field(model_grid, T_POINTS)

  write(*,*) "Simulation start"
  do ncycle=1, 100
    call invoke( compute_cu(CU_fld, p_fld, u_fld),      &
                 compute_cv(CV_fld, p_fld, v_fld),      &
                 compute_z(z_fld, p_fld, u_fld, v_fld), &
                 compute_h(h_fld, p_fld, u_fld, v_fld) )
  end do
  write(*,*) "Simulation end"

end program simple
