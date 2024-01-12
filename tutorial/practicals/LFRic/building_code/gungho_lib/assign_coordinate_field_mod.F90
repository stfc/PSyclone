!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities Council.
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
!
!-------------------------------------------------------------------------------
!> @brief Module to assign the values of the coordinates of the mesh to a field
module assign_coordinate_field_mod

  use base_mesh_config_mod, only : geometry, &
                                   geometry_spherical
  use constants_mod,        only : r_def, i_def, i_native
  use log_mod,              only : log_event, LOG_LEVEL_ERROR
  use planet_config_mod,    only : scaled_radius
  use mesh_mod,             only : mesh_type

  implicit none

contains
!> @brief Subroutine which assigns the values of the coordinates of the mesh
!! to a field
!> @details An array of size 3 for the type field is passed in to be populated.
!! The field proxy is used to break encapsulation and access the function space
!! and the data atributes of the field so that its values can be assigned.
!! calls two subroutines, get_cell_coords from the mesh generator and then
!! assign_coordinate on a column by column basis
!! @param[in]  mesh Mesh on which this field is attached
!! @param[out] chi  Real array of size 3 (x,y,z) of fields

  subroutine assign_coordinate_field(chi, mesh)

    use field_mod,             only: field_type, field_proxy_type
    use reference_element_mod, only: reference_element_type
    use mesh_constructor_helper_functions_mod, &
                               only: domain_size_type
    implicit none

    type( field_type ), intent( inout ) :: chi(3)
    type( mesh_type ),  intent(in)      :: mesh

    integer(i_def),                pointer :: map(:)            => null()
    real(r_def),                   pointer :: dof_coords(:,:)   => null()
    class(reference_element_type), pointer :: reference_element => null()

    type(field_proxy_type) :: chi_proxy(3)
    type(domain_size_type) :: domain_size

    real(r_def), allocatable :: column_coords(:,:,:)
    real(r_def), allocatable :: dz(:)  ! dz(nlayers) array
    real(r_def), allocatable :: vertex_coords(:,:)

    integer(i_def) :: cell
    integer(i_def) :: undf, ndf, nlayers
    integer(i_def) :: nverts

    integer(i_native) :: alloc_error
    integer(i_def)    :: depth

    ! Break encapsulation and get the proxy.
    chi_proxy(1) = chi(1)%get_proxy()
    chi_proxy(2) = chi(2)%get_proxy()
    chi_proxy(3) = chi(3)%get_proxy()
    undf    = chi_proxy(1)%vspace%get_undf()
    ndf     = chi_proxy(1)%vspace%get_ndf( )
    nlayers = chi_proxy(1)%vspace%get_nlayers()

    allocate ( dz(nlayers), STAT = alloc_error )
    if ( alloc_error /= 0 ) then
      call log_event( " assign_coordinate_field: Unable to allocate "// &
                      "local array dz(nlayers) ", LOG_LEVEL_ERROR )
    end if

    call mesh%get_dz(dz)

    reference_element => mesh%get_reference_element()
    call reference_element%get_vertex_coordinates( vertex_coords )
    nverts = reference_element%get_number_vertices()

    allocate( column_coords(3,nverts,nlayers ) )
    dof_coords => chi_proxy(1)%vspace%get_nodes( )

    domain_size =  mesh%get_domain_size()

    do cell = 1,chi_proxy(1)%vspace%get_ncell()
       map => chi_proxy(1)%vspace%get_cell_dofmap( cell )

       call mesh%get_column_coords(cell,column_coords)

       call assign_coordinate( nlayers,                 &
                               ndf,                     &
                               nverts,                  &
                               undf,                    &
                               map,                     &
                               dz,                      &
                               chi_proxy(1)%data,       &
                               chi_proxy(2)%data,       &
                               chi_proxy(3)%data,       &
                               column_coords,           &
                               dof_coords,              &
                               vertex_coords,           &
                               domain_size%maximum%x,   &
                               domain_size%minimum%y )
    end do
    ! Loop over all the cells

    ! As we have correctly set the chi fields into their full halos,
    ! mark their halos as clean, out to the full halo depth
    depth = mesh%get_halo_depth()
    call chi_proxy(1)%set_clean(depth)
    call chi_proxy(2)%set_clean(depth)
    call chi_proxy(3)%set_clean(depth)

    deallocate ( dz, column_coords, vertex_coords )

  end subroutine assign_coordinate_field

!> @brief Determines and assigns the coordinates for a single column
!! @param[in]  nlayers       integer: loop bound
!! @param[in]  ndf           integer: array size and loop bound
!! @param[in]  nverts        integer: array size and loop bound
!! @param[in]  undf          integer: array size and loop bound
!! @param[in]  map           integer array: indirection map
!! @param[in]  dz            Mesh layer thickness
!! @param[out] chi_1         real array: size undf x coord
!! @param[out] chi_2         real array: size undf y coord
!! @param[out] chi_3         real array: size undf z coord
!! @param[in]  column_coords real array: (3,nverts,nlayers)
!! @param[in]  chi_hat_node  real array: (3,ndf)
!! @param[in]  chi_hat_vert  real array: (nverts,3)
  subroutine assign_coordinate(nlayers,ndf,nverts,undf,map,dz, &
             chi_1,chi_2,chi_3,column_coords,chi_hat_node,chi_hat_vert, &
             domain_x, domain_y)

    use reference_element_mod, only: SWB, SEB, NEB, NWB, SWT, SET, NET, NWT

    implicit none

    ! Arguments
    integer, intent(in) :: nlayers, ndf, nverts, undf
    integer, intent(in) :: map(ndf)
    real(kind=r_def), intent(in)  :: dz(nlayers)
    real(kind=r_def), intent(out) :: chi_1(undf), chi_2(undf), chi_3(undf)
    real(kind=r_def), intent(in)  :: column_coords(3,nverts,nlayers)
    real(kind=r_def), intent(in)  :: chi_hat_node(3,ndf), chi_hat_vert(nverts,3)
    real(kind=r_def), intent(in)  :: domain_x, domain_y


    ! Internal variables
    integer          :: k, df, dfk, vert

    real(kind=r_def) :: interp_weight, x, y, z, radius_correction
    real(kind=r_def) :: vertex_local_coords(3,nverts)
    radius_correction = 1.0_r_def

    ! Compute the representation of the coordinate field
    do k = 0, nlayers-1
       vertex_local_coords(:,:) = column_coords(:,:,k+1)
       if (  geometry /= geometry_spherical ) then
         ! Check if point cell is on right or bottom boundary,
         ! assumes a monotonic coordinate field
         if ( column_coords(1,SEB,k+1) < column_coords(1,SWB,k+1)) then
         ! On x boundary
           vertex_local_coords(1,SEB) = domain_x
           vertex_local_coords(1,NEB) = domain_x
           vertex_local_coords(1,SET) = domain_x
           vertex_local_coords(1,NET) = domain_x
         end if
         if ( column_coords(2,SWB,k+1) > column_coords(2,NWB,k+1)) then
         ! On y boundary
           vertex_local_coords(2,SWB) = domain_y
           vertex_local_coords(2,SEB) = domain_y
           vertex_local_coords(2,SWT) = domain_y
           vertex_local_coords(2,SET) = domain_y
         end if
       end if
       do df = 1, ndf
          ! Compute interpolation weights
          x = 0.0_r_def
          y = 0.0_r_def
          z = 0.0_r_def
          do vert = 1,nverts
             interp_weight = &
                   (1.0_r_def - abs(chi_hat_vert(vert,1) - chi_hat_node(1,df))) &
                  *(1.0_r_def - abs(chi_hat_vert(vert,2) - chi_hat_node(2,df))) &
                  *(1.0_r_def - abs(chi_hat_vert(vert,3) - chi_hat_node(3,df)))

             x = x + interp_weight*vertex_local_coords(1,vert)
             y = y + interp_weight*vertex_local_coords(2,vert)
             z = z + interp_weight*vertex_local_coords(3,vert)
          end do
          ! For spherical domains we need to project x,y,z back onto
          ! spherical shells
          if ( geometry == geometry_spherical ) then
             radius_correction = scaled_radius + &
                                 sum(dz(1:k)) + chi_hat_node(3,df)*dz(k+1)
             radius_correction = radius_correction/sqrt(x*x + y*y + z*z)
          end if
          dfk = map(df)+k
          chi_1(dfk) = x*radius_correction
          chi_2(dfk) = y*radius_correction
          chi_3(dfk) = z*radius_correction
       end do
    end do

  end subroutine assign_coordinate

end module assign_coordinate_field_mod
