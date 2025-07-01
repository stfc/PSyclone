










!-----------------------------------------------------------------------------
! (c) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used
!-----------------------------------------------------------------------------

!> @brief Create/destroy global collections
!> @description An LFRic app which uses field objects will require a hierarchy
!>  of other objects which field objects rely upon. Collections of these objects
!>  are made globally visible as they will be used extensively within an LFRic
!>  app.
!>
!>  This module allows for standard methods for allocation/deallocation of these
!>  collections.
!>
module driver_collections_mod

  ! Global collections
  use function_space_collection_mod, only: function_space_collection_type, &
                                           function_space_collection
  use global_mesh_collection_mod,    only: global_mesh_collection_type,    &
                                           global_mesh_collection
  use halo_routing_collection_mod,   only: halo_routing_collection_type,   &
                                           halo_routing_collection
  use local_mesh_collection_mod,     only: local_mesh_collection_type,     &
                                           local_mesh_collection
  use mesh_collection_mod,           only: mesh_collection_type,           &
                                           mesh_collection

  implicit none

  private

  public  :: init_collections, final_collections

contains

!> @brief  Allocates global collections used to construct LFRic fields.
subroutine init_collections()

  implicit none

  allocate( global_mesh_collection,    source=global_mesh_collection_type() )
  allocate( local_mesh_collection,     source=local_mesh_collection_type() )
  allocate( mesh_collection,           source=mesh_collection_type() )
  allocate( function_space_collection, source=function_space_collection_type() )
  allocate( halo_routing_collection,   source=halo_routing_collection_type() )

end subroutine init_collections

!> @brief  Finalises global collections used to construct LFRic fields.
subroutine final_collections()

  implicit none

  if (allocated( global_mesh_collection )) then
    call global_mesh_collection%clear()
    deallocate( global_mesh_collection )
  end if

  if (allocated( local_mesh_collection )) then
    call local_mesh_collection%clear()
    deallocate( local_mesh_collection )
  end if

  if (allocated( mesh_collection ))then
    call mesh_collection%clear()
    deallocate( mesh_collection )
  end if

  if (allocated( function_space_collection )) then
    call function_space_collection%clear()
    deallocate( function_space_collection )
  end if

  if (allocated( halo_routing_collection )) then
    call halo_routing_collection%clear()
    deallocate( halo_routing_collection )
  end if

end subroutine final_collections

end module driver_collections_mod
