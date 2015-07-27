!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

PROGRAM single_invoke_different_iterates_over

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  ! These two kernels have different values of ITERATES_OVER
  use kernel_ne_offset_mod, only: compute_cv
  use kernel_scalar_int,    only: bc_ssh
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
  p_fld  = r2d_field(model_grid, T_POINTS)
  v_fld  = r2d_field(model_grid, v_POINTS)
  cv_fld = r2d_field(model_grid, V_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( compute_cv(cv_fld, p_fld, v_fld), &
                 bc_ssh(ncycle, p_fld) )

  END DO

  !===================================================

END PROGRAM single_invoke_different_iterates_over
