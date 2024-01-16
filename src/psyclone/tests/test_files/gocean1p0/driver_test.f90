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
! Author A. R. Porter, STFC Daresbury Lab
! Modified by J. Henrichs, Bureau of Meteorology
! -----------------------------------------------------------------------------

PROGRAM extract_example_with_various_variable_access_patterns

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_driver_test,  only: compute_kernel
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: out_fld, in_out_fld, in_fld, v_fld
  type(r2d_field) :: out_fld_post, out_fld_post0

  ! This field will potentially create a name clash in the driver:
  ! The kernel takes the 'dx' grid property as parameter, so we
  ! have to test that the driver does not create a local variable 'dx'
  ! for the field AND for the grid property, one of them must be
  ! renamed!
  type(r2d_field) :: dx

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  in_out_fld    = r2d_field(model_grid, GO_T_POINTS)

  in_fld       = r2d_field(model_grid, GO_U_POINTS)
  dummy_in_fld = r2d_field(model_grid, GO_U_POINTS)
  v_fld        = r2d_field(model_grid, GO_V_POINTS)
  out_fld      = r2d_field(model_grid, GO_U_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( compute_kernel(out_fld, in_out_fld, in_fld, dx))

  END DO
  call invoke( compute_kernel(out_fld, in_out_fld, out_fld_post, dx))
  call invoke( compute_kernel(out_fld, out_fld_post, out_fld_post0, dx))
  call invoke( compute_kernel(out_fld, in_out_fld, in_fld, dx),    &
               compute_kernel(out_fld, in_out_fld, in_fld, dx),    &
               compute_kernel(out_fld, in_out_fld, in_fld, dx)      )

  !===================================================

END PROGRAM extract_example_with_various_variable_access_patterns
