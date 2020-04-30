!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
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
!------------------------------------------------------------------------------
! Modified by I. Kavcic, Met Office

module oned_conservative_flux_alg_mod

  use constants_mod,                     only: r_def, i_def
  use field_mod,                         only: field_type
  use function_space_mod,                only: function_space_type
  use function_space_collection_mod,     only: function_space_collection
  use quadrature_mod,                    only: quadrature_type, GAUSSIAN
  use fs_continuity_mod,                 only: W0, W3
  use finite_element_config_mod,         only: element_order
  use subgrid_config_mod,                only: transport_stencil_length,       &
                                               rho_stencil_length

  implicit none

  private

  contains

  subroutine oned_conservative_flux_alg( direction,    &
                                         u,            &
                                         dep_pts,      &
                                         rho_in,       &
                                         mass_flux,    &
                                         mesh_id)

    use subgrid_coeffs_kernel_mod, only : subgrid_coeffs_kernel_type
    use conservative_flux_kernel_mod, only : conservative_flux_kernel_type

    implicit none

    integer(i_def),      intent(in)     :: direction
    type(field_type),    intent(in)     :: u
    type(field_type),    intent(in)     :: dep_pts
    type(field_type),    intent(in)     :: rho_in
    type(field_type),    intent(in)     :: mass_flux
    integer(i_def),      intent(in)     :: mesh_id

    type( field_type ) :: a0, a1, a2

    type(function_space_type), pointer :: rho_fs   => null()

    rho_fs => function_space_collection%get_fs( mesh_id, element_order,      &
                                              rho_in%which_function_space() )

    a0 = field_type( vector_space = rho_fs )
    a1 = field_type( vector_space = rho_fs )
    a2 = field_type( vector_space = rho_fs )

    call invoke(                                                                 &
                                                                                 &
        subgrid_coeffs_kernel_type(a0,a1,a2,rho_in,rho_stencil_length),          &
                                                                                 &
        conservative_flux_kernel_type(mass_flux,dep_pts,u,                       &
                                      rho_in,transport_stencil_length,direction, &
                                      a0,transport_stencil_length,direction,     &
                                      a1,transport_stencil_length,direction,     &
                                      a2,transport_stencil_length,direction)     &
               )

  end subroutine oned_conservative_flux_alg

end module oned_conservative_flux_alg_mod
