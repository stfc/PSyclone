!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! Modified by I. Kavcic, Met Office

!> A simple algorithm for testing the psy layer.
!! Computes the Galerkin projection for different W spaces

module dynamo_algorithm_mod

  use lfric
  use log_mod,    only: log_event, log_scratch_space, LOG_LEVEL_INFO
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

    type( field_type ), intent( in )    :: pressure_density
    type( field_type ), intent( in )    :: rhs
    type( field_type ), intent( in )    :: flux_velocity
    type( field_type ), intent( in )    :: rhs_v2
    type( field_type ), intent( inout ) :: circulation
    type( field_type ), intent( in )    :: rhs_v1


    call log_event( "Dynamo: Galerkin Projection for W3 ", LOG_LEVEL_INFO )
    ! Construct PSy layer given a list of kernels. This is the line the code
    ! generator may parse and do its stuff.

    call invoke ( v3_rhs_kernel_type(rhs),                            &
                  v3_solver_kernel_type(pressure_density,rhs) )

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
