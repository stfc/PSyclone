! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
! Author A. Porter STFC Daresbury Lab
! Modified A. B. G. Chalk STFC Daresbury Lab
! -----------------------------------------------------------------------------

PROGRAM dependent_invoke_test

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system. This program
  ! tests psyclones ability to handle multiple dependent
  ! loops within the same invoke

  use kind_params_mod
  use grid_mod
  use field_mod
  use compute_cu_mod,  only: compute_cu
  implicit none

  type(grid_type), target :: model_grid
  !> We create two copies of each field type to use
  !> in a multi-call invoke to create a specific
  !> dependency pattern
  !> Two copies of a "pressure" field
  type(r2d_field) :: p_fld, p2_fld
  !> Two copies of an "x velocity" field
  type(r2d_field) :: u_fld, u2_fld
  !> Two copies of a "x mass flux" field
  type(r2d_field) :: cu_fld, cu_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  p_fld    = r2d_field(model_grid, GO_T_POINTS)

  u_fld    = r2d_field(model_grid, GO_U_POINTS)

  cu_fld    = r2d_field(model_grid, GO_U_POINTS)

  p2_fld  = r2d_field(model_grid, GO_T_POINTS)

  u2_fld = r2d_field(model_grid, GO_U_POINTS)

  cu2_fld = r2d_field(model_grid, GO_U_POINTS)


  !  ** Start of time loop ** 
  DO ncycle=1,100

    call invoke( compute_cu(cu_fld, p_fld, u_fld),    &
                 compute_cu(cu2_fld, p2_fld, u2_fld), &
                 compute_cu(cu2_fld, p2_fld, u2_fld), &
                 compute_cu(cu_fld, p_fld, u_fld) )

  END DO

  !===================================================

END PROGRAM dependent_invoke_test
