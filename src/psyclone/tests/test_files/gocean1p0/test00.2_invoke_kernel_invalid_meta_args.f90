!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

PROGRAM kernel_invalid_meta_args_test

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use grid_mod
  use field_mod
  use kernel_invalid_meta_args,  only: bc_ssh
  implicit none

  type(grid_type), target :: model_grid
  type(r2d_field) :: u_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  ! Create the model grid
  model_grid = grid_type(GO_ARAKAWA_C,                        &
                         (/GO_BC_PERIODIC,GO_BC_PERIODIC,GO_BC_NONE/) )

  ! Create fields on this grid
  u_fld    = r2d_field(model_grid, GO_U_POINTS)

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
    call invoke( bc_ssh(ncycle, u_fld) )

  END DO

  !===================================================

END PROGRAM kernel_invalid_meta_args_test
