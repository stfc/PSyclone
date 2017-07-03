!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

PROGRAM single_invoke_scalar_int_test

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_scalar_int, only: bc_ssh
  implicit none

  type(grid_type), target :: model_grid
  !> Sea-surface height
  type(r2d_field) :: ssh_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(ARAKAWA_C,                        &
                         (/BC_PERIODIC,BC_PERIODIC,BC_NONE/) )

  ! Create fields on this grid
  ssh_fld   = r2d_field(model_grid, T_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( bc_ssh(ncycle, ssh_fld) )

  END DO

  !===================================================

END PROGRAM single_invoke_scalar_int_test
