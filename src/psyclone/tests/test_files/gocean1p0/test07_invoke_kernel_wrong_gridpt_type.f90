!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

PROGRAM kernel_wrong_gridpt_type_test

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_wrong_gridpt_type,  only: compute_cu
  implicit none

  type(grid_type), target :: model_grid
  !> Pressure at current time step
  type(r2d_field) :: p_fld
  !> Velocity in x direction at current time step
  type(r2d_field) :: u_fld
  !> Mass flux in x direction at current time step
  type(r2d_field) :: cu_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(ARAKAWA_C,                        &
                         (/BC_PERIODIC,BC_PERIODIC,BC_NONE/) )

  ! Create fields on this grid
  p_fld    = r2d_field(model_grid, T_POINTS)

  u_fld    = r2d_field(model_grid, U_POINTS)

  cu_fld   = r2d_field(model_grid, U_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( compute_cu(cu_fld, p_fld, u_fld) )

  END DO

  !===================================================

END PROGRAM kernel_wrong_gridpt_type_test
