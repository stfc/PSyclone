!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!>
!> @brief   Holds and manages the multiple local mesh maps
!>
!> @details A container which holds a collection of local mesh maps
!>          It will handle the creation and storing of requested maps
!>          between two local meshes of differing resolutions.
!
module local_mesh_map_collection_mod

  use constants_mod,      only: i_def, l_def
  use linked_list_mod,    only: linked_list_type, linked_list_item_type
  use local_mesh_map_mod, only: local_mesh_map_type, generate_local_mesh_map_id
  use log_mod,            only: log_event, log_scratch_space, &
                                LOG_LEVEL_TRACE, LOG_LEVEL_ERROR

  implicit none

  private

  type, public :: local_mesh_map_collection_type
    private
    !> Linked list of local_mesh_map_type objects.
    type(linked_list_type) :: local_mesh_map_list
  contains
    procedure, public :: add_local_mesh_map
    procedure, public :: get_local_mesh_map
    procedure, public :: clear
    final             :: local_mesh_map_collection_destructor

  end type local_mesh_map_collection_type

  interface local_mesh_map_collection_type
    module procedure local_mesh_map_collection_constructor
  end interface

contains

!==============================================================================
!> @brief  Constructs the collection object for objects of
!>         local_mesh_map_type
!> @return The constructed local_mesh_map_collection object
function local_mesh_map_collection_constructor() result(self)

  implicit none

  type(local_mesh_map_collection_type) :: self

  self%local_mesh_map_list = linked_list_type()

end function local_mesh_map_collection_constructor

!==============================================================================
!> @brief     Adds a local_mesh_map object to the collection
!> @param[in] source_local_mesh_id ID of the source local mesh
!>                                  object for this local mesh map object.
!> @param[in] target_local_mesh_id ID of the target local mesh
!>                                  object for this local mesh map object.
!> @param[in] map
!>            Local cell ids of the target local mesh object which
!>            overlap with source local mesh cells. This array
!>            should have dimensions of
!>            [number of target cells for each source cell,
!>             number of source cells]
subroutine add_local_mesh_map( self,                  &
                               source_local_mesh_id, &
                               target_local_mesh_id, &
                               map )

  implicit none

  class(local_mesh_map_collection_type), intent(inout) :: self
  integer(i_def), intent(in) :: source_local_mesh_id
  integer(i_def), intent(in) :: target_local_mesh_id
  integer(i_def), intent(in) :: map(:,:,:)

  type(local_mesh_map_type) :: local_mesh_map

  integer(i_def) :: local_mesh_map_id
  logical(l_def) :: local_mesh_map_exists

  ! Create the local mesh map id
  local_mesh_map_id = generate_local_mesh_map_id( source_local_mesh_id, &
                                                  target_local_mesh_id )

  ! Query the local mesh map collection to see if this
  ! local mesh map exists
  local_mesh_map_exists = &
      self%local_mesh_map_list%item_exists(local_mesh_map_id)

  if (local_mesh_map_exists) then
    ! Do nothing as map already exists
    write(log_scratch_space, '(A,I0,A)')                          &
        'Skipping task: Local mesh map (id: ', local_mesh_map_id, &
        ') already exists.'
    call log_event(log_scratch_space, LOG_LEVEL_TRACE)
    return

  else

    ! Create the local_mesh_map object and add to the linked list
    local_mesh_map = local_mesh_map_type( source_local_mesh_id, &
                                          target_local_mesh_id, &
                                          map )

    call self%local_mesh_map_list%insert_item( local_mesh_map )

  end if

  return
end subroutine add_local_mesh_map

!==============================================================================
!> @brief     Returns a pointer to a local_mesh_map object which maps local
!>            cell ids in the target local mesh to local cell ids in the
!>            source local mesh.
!> @param[in] source_local_mesh_id ID of source local mesh
!>                                  object of requested local_mesh_map
!>                                  object.
!> @param[in] target_local_mesh_id ID of target local mesh
!>                                  object of requested local_mesh_map
!>                                  object.
!> @return    local_mesh_map_type A pointer to a local_mesh_map object
function get_local_mesh_map( self,                  &
                             source_local_mesh_id,  &
                             target_local_mesh_id ) &
                      result( local_mesh_map )

  implicit none

  class(local_mesh_map_collection_type), intent(in) :: self
  integer(i_def), intent(in) :: source_local_mesh_id
  integer(i_def), intent(in) :: target_local_mesh_id

  type(local_mesh_map_type), pointer :: local_mesh_map
  integer(i_def) :: local_mesh_map_id
  logical(l_def) :: local_mesh_map_exists

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type),pointer :: loop => null()

  local_mesh_map => null()

  local_mesh_map_id = generate_local_mesh_map_id( source_local_mesh_id, &
                                                  target_local_mesh_id )

  local_mesh_map_exists = &
      self%local_mesh_map_list%item_exists(local_mesh_map_id)

  if (local_mesh_map_exists) then

    loop => self%local_mesh_map_list%get_head()

    do
      ! Loop over list looking of correct item id
      if ( local_mesh_map_id == loop%payload%get_id() ) then

        select type(m => loop%payload)
        type is (local_mesh_map_type)
          local_mesh_map => m
        end select
        exit

      end if

      loop => loop%next
    end do

  else

    ! Requested map does not exist in this collection
    nullify(local_mesh_map)
    nullify(loop)
    write(log_scratch_space, '(A,I0,A)')                     &
        'Requested local mesh map (id: ', local_mesh_map_id, &
        ') does not exist.'
    call log_event(log_scratch_space, LOG_LEVEL_TRACE)
    return

  end if

  nullify(loop)

end function get_local_mesh_map

!==============================================================================
!> @brief Routine to destroy local_mesh_map_collection.
subroutine clear(self)

  ! Clear all items from the linked list in the collection
  implicit none

  class(local_mesh_map_collection_type), intent(inout) :: self

  call self%local_mesh_map_list%clear()

  return
end subroutine clear


!-------------------------------------------------------------------------------
!> @brief Finalizer routine - simply calls clear()
subroutine local_mesh_map_collection_destructor(self)

  implicit none

  type(local_mesh_map_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine local_mesh_map_collection_destructor

end module local_mesh_map_collection_mod
