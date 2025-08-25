!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief   Holds and manages the multiple mesh maps
!>
!> @details A container which holds a collection of mesh maps
!>          It will handle the creation and storing of requested maps
!>          between two meshes of differing resolutions.
!
module mesh_map_collection_mod

use constants_mod,       only: i_def, l_def
use mesh_map_mod,        only: mesh_map_type
use partition_mod,       only: partition_type
use linked_list_mod,     only: linked_list_type, linked_list_item_type
use log_mod,             only: log_event, log_scratch_space, &
                               LOG_LEVEL_ERROR, LOG_LEVEL_TRACE

implicit none

private

!-------------------------------------------------------------------------------
! Type which is is a collection of mesh_map_type objects held in a linked list
!-------------------------------------------------------------------------------
type, public :: mesh_map_collection_type

  private

  !> Linked list mesh_map_type objects.
  type(linked_list_type)      :: mesh_map_list

contains ! Type-bound procedures for mesh_map_collection_type

  !> @brief Adds the specified mesh map to the collection with the
  !>        source/target identifiers if not already in the collection
  !> @param[in] source_mesh_id ID of the source mesh object
  !> @param[in] target_mesh_id ID of the target mesh object
  !> @param[in] map            LID-LID cell mapping between specified
  !>                           source/target meshes.
  procedure, public :: add_mesh_map
  !>
  !> @brief  Returns pointer to mesh_map_type which maps the specified
  !>         source/target meshes
  !> @param[in] source_mesh_id ID of the source mesh
  !> @param[in] target_mesh_id ID of the target mesh
  !> @retval mesh_map_type<<pointer>>
  procedure, public :: get_mesh_map
  !>
  !> @brief  Returns true if a mesh map from meshes with
  !>         the specified mesh_map_id (generated from the
  !>         source/target ids)exists in the collection
  !> @param[in] mesh_map_id ID of the mesh_map to query for.
  !> @retval mesh_map_exists<<logical>>
  procedure, public :: query
  !>
  !> @brief Forced clear of all the objects in the collection.
  !>        This routine should not need to be called manually except
  !>        (possibly) in pfunit tests
  procedure, public :: clear

  !> @brief Finalizer routine, should be called automatically by
  !>        code when the object is out of scope
  final :: mesh_map_collection_destructor


end type mesh_map_collection_type

interface mesh_map_collection_type
  module procedure mesh_map_collection_constructor
end interface

!===============================================================================
contains  ! Module procedures

!-------------------------------------------------------------------------------
! Construct the mesh_map_collection object
!-------------------------------------------------------------------------------
!
!> @brief  Constructs the mesh map collection object
!> @return mesh_map_collection_type object
!===============================================================================
function mesh_map_collection_constructor () result ( instance )

implicit none

type(mesh_map_collection_type) :: instance

instance%mesh_map_list = linked_list_type()

return
end function mesh_map_collection_constructor



!===============================================================================
subroutine add_mesh_map ( self, source_mesh_id, target_mesh_id, map )

implicit none

class(mesh_map_collection_type), intent(inout) :: self

integer(i_def), intent(in) :: source_mesh_id
integer(i_def), intent(in) :: target_mesh_id
integer(i_def), intent(in) :: map(:,:,:)

type(mesh_map_type) :: mesh_map


logical(l_def) :: mesh_map_exists
integer(i_def) :: mesh_map_id


mesh_map_id = (10000*source_mesh_id) + target_mesh_id

mesh_map_exists = self%query(mesh_map_id)

if (.not. mesh_map_exists) then

  mesh_map = mesh_map_type( source_mesh_id, target_mesh_id, map )

  write(log_scratch_space, '(A,I0,A)') &
      'Adding mesh map (id:', mesh_map_id,')'
  call log_event(log_scratch_space, LOG_LEVEL_TRACE)

  call self%mesh_map_list%insert_item(mesh_map)

end if

return
end subroutine add_mesh_map



!===============================================================================
function get_mesh_map (self, source_mesh_id, target_mesh_id) result(mesh_map)

implicit none

class(mesh_map_collection_type), intent(in) :: self
integer(i_def),                  intent(in) :: source_mesh_id
integer(i_def),                  intent(in) :: target_mesh_id

type(mesh_map_type), pointer :: mesh_map


integer(i_def) :: mesh_map_id
integer(i_def) :: list_item_id
type(linked_list_item_type), pointer :: loop ! Temp pointer for looping
logical(l_def) :: mesh_map_exists

mesh_map_id = (10000*source_mesh_id) + target_mesh_id

mesh_map_exists = self%query(mesh_map_id)

if (.not. mesh_map_exists) then

  write(log_scratch_space, '(A,I0,A)') &
      'Mesh map (id:',mesh_map_id,') not present in this map collection.'

  call log_event(log_scratch_space, LOG_LEVEL_ERROR)
  mesh_map => null()
  loop => self%mesh_map_list%get_tail()

else

  ! Cycle through the linked list of mesh maps until it
  ! finds the one with the relevant mesh ids.

  ! Point at the head of the mesh map linked list
  loop => self%mesh_map_list%get_head()

  ! Loop through list
  do

    ! Get the payload id and test
    list_item_id = loop%payload%get_id()

    if ( list_item_id == mesh_map_id ) then
      ! 'cast' to mesh_map_type
      select type(v => loop%payload)
      type is (mesh_map_type)
        mesh_map => v
      end select

      exit
    end if

    loop => loop%next
  end do

end if

return
end function get_mesh_map



!===============================================================================
function query (self, mesh_map_id) result(mesh_map_exists)

implicit none

class(mesh_map_collection_type), intent(in) :: self
integer(i_def),                  intent(in) :: mesh_map_id
logical(l_def) :: mesh_map_exists

mesh_map_exists = self%mesh_map_list%item_exists(mesh_map_id)

return
end function query



!===============================================================================
subroutine clear(self)

  implicit none
  class(mesh_map_collection_type), intent(inout) :: self

  call self%mesh_map_list%clear()

  return
end subroutine clear



!===============================================================================
subroutine mesh_map_collection_destructor(self)

implicit none
type(mesh_map_collection_type), intent(inout) :: self

call self%clear()

return
end subroutine mesh_map_collection_destructor



end module mesh_map_collection_mod

