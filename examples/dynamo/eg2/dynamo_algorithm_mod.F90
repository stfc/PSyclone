!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> A simple algorithm for testing the psy layer.
!! Computes the Galerkin projection for different W spaces

module dynamo_algorithm_mod

  use lfric
  use log_mod,    only: log_event, log_scratch_space, LOG_LEVEL_INFO
  !RF use psy,        only: invoke_rhs_v3, invoke_v3_solver_kernel,            &
  !RF                       invoke_rhs_v2, invoke_rhs_v1
  use v3_rhs_kernel_mod, only : v3_rhs_kernel_type
  use v3_solver_kernel_mod, only : v3_solver_kernel_type
  use v2_kernel_mod, only : v2_kernel_type
  use v1_kernel_mod, only : v1_kernel_type
  use solver_mod, only: solver_algorithm

  implicit none

  private
  public :: dynamo_algorithm

contains

  !> Galerkin projections for different W spaces
  !>
  subroutine dynamo_algorithm( pressure_density, rhs, &
                               flux_velocity, rhs_v2, &
                               circulation,   rhs_v1)

    implicit none

    type( field_type ), intent( in ) :: pressure_density
    type( field_type ), intent( in ) :: rhs
    type( field_type ), intent( inout ) :: flux_velocity
    type( field_type ), intent( inout ) :: rhs_v2
    type( field_type ), intent( inout ) :: circulation
    type( field_type ), intent( inout ) :: rhs_v1


    call log_event( "Dynamo: Galerkin Projection for W3 ", LOG_LEVEL_INFO )
    !Construct PSy layer given a list of kernels. This is the line the code
    !generator may parse and do its stuff.
    
    call invoke ( v3_rhs_kernel_type(rhs),                            &
                  v3_solver_kernel_type(pressure_density,rhs) )
    !RF call invoke_rhs_v3( rhs )
    !RF call invoke_v3_solver_kernel( pressure_density, rhs )

    call log_event( "Dynamo:Starting Galerkin projection for W2 ...",      &
         LOG_LEVEL_INFO)

    call invoke( v2_kernel_type(rhs_v2) )
    !RF call invoke_rhs_v2(rhs_v2)
    call solver_algorithm(flux_velocity, rhs_v2)
    
    call log_event( "Dynamo:Starting Galerkin projection for W1 ...",      &
         LOG_LEVEL_INFO)

    call invoke( v1_kernel_type(rhs_v1) )
    !RF call invoke_rhs_v1(rhs_v1)
    call solver_algorithm(circulation, rhs_v1)

  end subroutine dynamo_algorithm

end module dynamo_algorithm_mod
