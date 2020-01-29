!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @mainpage Dynamo
!> Illustration of the PsyKAl (Parallel-system/Kernel/Algorithm) architecture
!> for Gung Ho. Whilst the computational and optimisation infrastructure is
!> being developed, the science code is being developed using 
!> a hand-rolled Psy layer, Psy-lite. A PsyKAl-lite needs a dynamo!
!> Eventually, PsyKAl-lite will be replaced with the real Psy and Dynamo
!> will become Gung Ho.

!> @brief Main program used to illustrate dynamo functionality.

!> @details Creates the function spaces and calls <code>set_up</code> to
!> populate them (either read in or compute) then individual calls to the
!> psy-layer with kernels as if the code has been pre-processed by Psyclone.

program dynamo

  use dynamo_algorithm_mod, only : dynamo_algorithm
  use field_mod,            only : field_type
  use lfric
  use log_mod,              only : log_event, LOG_LEVEL_INFO, & 
                                   log_set_level, LOG_LEVEL_DEBUG
  use set_up_mod,           only : set_up

  implicit none

  type( function_space_type )      :: v3_function_space, v2_function_space, &
                                      v1_function_space, v0_function_space
  type( field_type )               :: pressure_density, rhs
  type( field_type )               :: flux_velocity, rhs_v2
  type( field_type )               :: rhs_v1, circulation
  type( gaussian_quadrature_type ) :: gq
  integer                          :: num_layers

  call log_event( 'Dynamo running...', LOG_LEVEL_INFO )

  gq = gaussian_quadrature_type( )

  call set_up( v0_function_space, v1_function_space, v2_function_space,    &
               v3_function_space, num_layers, gq )

  pressure_density = field_type( vector_space = v3_function_space,         &
                                 gq = gq,                                  &
                                 num_layers = num_layers)

  rhs = field_type( vector_space = v3_function_space,                      &
                    gq = gq,                                               &
                    num_layers = num_layers )

  flux_velocity = field_type( vector_space = v2_function_space,            &
                         gq = gq,                                          &
                         num_layers = num_layers )

  
  rhs_v2 = field_type( vector_space = v2_function_space,                   &
                       gq = gq,                                            &
                       num_layers = num_layers )

  circulation = field_type( vector_space = v1_function_space,              &
                            gq = gq,                                       &
                            num_layers = num_layers )

  rhs_v1 = field_type( vector_space = v1_function_space,                   &
                       gq = gq,                                            &
                       num_layers = num_layers )

  call dynamo_algorithm( pressure_density, rhs,                            &
                         flux_velocity, rhs_v2,                            &
                         circulation,rhs_v1 )

  call rhs%print_field( 'RHS field...' )
  call pressure_density%print_field( 'LHS field...' )

  call rhs_v2%print_field( 'RHS field...v2' )
  call flux_velocity%print_field('flux_velocity ...')

  call rhs_v1%print_field("RHS v1 ...")
  call circulation%print_field("circulation ...")
  

  call log_event( 'Dynamo completed', LOG_LEVEL_INFO )

end program dynamo
