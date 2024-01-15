!-------------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
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
! Modified: R. W. Ford and A. R. Porter, STFC Daresbury Lab
!           I. Kavcic, Met Office

!>@brief compute the locally assembled SI operators
module si_operators_alg_mod

  use constants_mod,             only: i_def, r_def
  use operator_mod,              only: operator_type
  use field_mod,                 only: field_type
  use finite_element_config_mod, only: wtheta_on
  use log_mod,                   only: log_event, &
                                       LOG_LEVEL_INFO, &
                                       LOG_LEVEL_ERROR
  use solver_config_mod,         only: preconditioner, &
                                       solver_preconditioner_tridiagonal, &
                                       normalise
  implicit none

  private

  ! Variables private to this module that can only be accessed by public
  ! functions returning pointers to them 
  type(operator_type), target :: m3_rho_star
  type(operator_type), target :: m3_exner_star
  type(operator_type), target :: div_star
  type(operator_type), target :: p2theta
  type(operator_type), target :: ptheta2
  type(operator_type), target :: p3theta
  type(operator_type), target :: compound_div
  type(field_type),    target :: rho_at_u
  type(field_type),    target :: mt_lumped
  type(field_type),    target :: tri_precon(3)
  type(field_type),    target :: Helm_diag

 ! Public functions to create and access the module contents
  public :: create_si_operators
  public :: compute_si_operators
  public :: get_m3_rho_star
  public :: get_m3_exner_star 
  public :: get_div_star
  public :: get_p2theta
  public :: get_ptheta2 
  public :: get_p3theta
  public :: get_compound_div 
  public :: get_rho_at_u 
  public :: get_mt_lumped
  public :: get_tri_precon
  public :: get_helm_diag

contains

!> Subroutine to create the si operators
subroutine create_si_operators(mesh_id)

  use function_space_mod,        only: function_space_type
  use fs_continuity_mod,         only: W0, W2, W3, Wtheta
  use finite_element_config_mod, only: element_order
  use function_space_collection_mod, &
                                 only: function_space_collection

  implicit none

  integer(i_def), intent(in) :: mesh_id

  type(function_space_type), pointer     :: w2_fs => null()
  type(function_space_type), pointer     :: w3_fs => null()
  type(function_space_type), pointer     :: wtheta_fs => null()

  call log_event( "Dynamo: creating si_operators", LOG_LEVEL_INFO )

  w2_fs     => function_space_collection%get_fs( mesh_id, element_order, W2 )
  w3_fs     => function_space_collection%get_fs( mesh_id, element_order, W3 )
  if (wtheta_on) then
    wtheta_fs => function_space_collection%get_fs( mesh_id, element_order, Wtheta )
  else
    wtheta_fs => function_space_collection%get_fs( mesh_id, element_order, W0 )
  end if
  ! Should change 0 to theta
 
  m3_rho_star   = operator_type( w3_fs, w3_fs )
  m3_exner_star = operator_type( w3_fs, w3_fs )
  div_star      = operator_type( w3_fs, w2_fs )
  p2theta       = operator_type( w2_fs, wtheta_fs )
  ptheta2       = operator_type( wtheta_fs, w2_fs )
  p3theta       = operator_type( w3_fs, wtheta_fs )
  compound_div  = operator_type( w3_fs, w2_fs )

  rho_at_u  = field_type(vector_space = w2_fs)
  mt_lumped = field_type(vector_space = wtheta_fs)

  if ( preconditioner == solver_preconditioner_tridiagonal ) then
    if ( element_order /= 0 ) then
      call log_event( "tridiagonal precon only valid for order 0", &
                      LOG_LEVEL_ERROR )
    end if
    tri_precon(1) = field_type(vector_space = w3_fs)
    tri_precon(2) = field_type(vector_space = w3_fs)
    tri_precon(3) = field_type(vector_space = w3_fs)
  end if
  helm_diag = field_type(vector_space = w3_fs)

end subroutine create_si_operators

!> Subroutine to compute the si operators
subroutine compute_si_operators(ref_state)
  use quadrature_mod,                  only: quadrature_type, GAUSSIAN
  use evaluator_xyz_mod,               only: evaluator_xyz_type
  use weighted_m3_rho_kernel_mod,      only: weighted_m3_rho_kernel_type
  use weighted_m3_exner_kernel_mod,    only: weighted_m3_exner_kernel_type
  use weighted_div_kernel_mod,         only: weighted_div_kernel_type
  use weighted_proj_2theta_kernel_mod, only: weighted_proj_2theta_kernel_type
  use weighted_proj_theta2_kernel_mod, only: weighted_proj_theta2_kernel_type
  use weighted_proj_3theta_kernel_mod, only: weighted_proj_3theta_kernel_type
  use compound_operator_kernel_mod,    only: compound_operator_kernel_type
  use psykal_lite_mod,                 only: invoke_sample_flux_kernel, &
                                             invoke_compute_tri_precon_kernel, &
                                             invoke_weighted_div_bd_kernel_type
  use multiplicity_kernel_mod,         only: multiplicity_kernel_type
  use finite_element_config_mod,       only: element_order, wtheta_on
  use function_space_mod,              only: function_space_type
  use function_space_collection_mod,   only: function_space_collection
  use matrix_vector_kernel_mod,        only: matrix_vector_kernel_type
  use runtime_constants_mod,           only: get_coordinates, &
                                             get_div, &
                                             get_mass_matrix
  use derived_config_mod,              only: bundle_size
  implicit none

  type(field_type), target, intent(in) :: ref_state(bundle_size) ! (u, theta, rho)
  type(field_type),            pointer :: theta, rho => null()
  type(quadrature_type)                :: qr
  type(field_type),            pointer :: chi(:) => null()
  type(operator_type),         pointer :: m3_inv, div, mt => null()
  integer(i_def)                       :: mesh
  integer(i_def)                       :: fs_handle
  type(field_type)                     :: ones, w2_multiplicity
  type(function_space_type),   pointer :: u_fs, theta_fs, rho_fs => null()
  type(evaluator_xyz_type)             :: evaluator

  qr = quadrature_type(element_order+3, GAUSSIAN)
  theta  => ref_state(2)
  rho    => ref_state(3)  
  chi    => get_coordinates()
  m3_inv => get_mass_matrix(4)
  div    => get_div()

  call invoke( weighted_proj_theta2_kernel_type(ptheta2, theta, qr) )

end subroutine compute_si_operators


 !> Function to return a pointer to the m3_rho_star
 !> @return The operator
  function get_m3_rho_star() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => m3_rho_star
  end function get_m3_rho_star

 !> Function to return a pointer to the m3_exner_star
 !> @return The operator
  function get_m3_exner_star() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => m3_exner_star
  end function get_m3_exner_star

 !> Function to return a pointer to the div_star
 !> @return The operator
  function get_div_star() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => div_star
  end function get_div_star

 !> Function to return a pointer to the p2theta
 !> @return The operator
  function get_p2theta() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => p2theta
  end function get_p2theta

 !> Function to return a pointer to the ptheta2
 !> @return The operator
  function get_ptheta2() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => ptheta2
  end function get_ptheta2

 !> Function to return a pointer to the p3theta
 !> @return The operator
  function get_p3theta() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => p3theta
  end function get_p3theta

 !> Function to return a pointer to the compound div
 !> @return The operator
  function get_compound_div() result(op)
    implicit none
    type(operator_type), pointer ::op
    op => compound_div
  end function get_compound_div

 !> Function to return a pointer to the rho_at_u
 !> @return The field
  function get_rho_at_u() result(field)
    implicit none
    type(field_type), pointer :: field
    field => rho_at_u
  end function get_rho_at_u

 !> Function to return a pointer to the mt_lumped
 !> @return The field
  function get_mt_lumped() result(field)
    implicit none
    type(field_type), pointer :: field
    field => mt_lumped
  end function get_mt_lumped

 !> Function to return a pointer to the tri_precon
 !> @return The field
  function get_tri_precon() result(field)
    implicit none
    type(field_type), pointer :: field(:)
    field => tri_precon
  end function get_tri_precon

 !> Function to return a pointer to the Helm diagonal
 !> @return The field
  function get_helm_diag() result(field)
    implicit none
    type(field_type), pointer :: field
    field => helm_diag
  end function get_helm_diag

end module si_operators_alg_mod
