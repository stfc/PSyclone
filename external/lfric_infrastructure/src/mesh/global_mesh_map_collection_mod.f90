










!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief   Holds and manages the multiple global mesh maps
!>
!> @details A container which holds a collection of global mesh maps
!>          It will handle the creation and storing of requested global maps
!>          between two global meshes of differing resolutions.
!
module global_mesh_map_collection_mod

  use constants_mod,       only: i_def, l_def
  use linked_list_mod,     only: linked_list_type, linked_list_item_type
  use global_mesh_map_mod, only: global_mesh_map_type, generate_global_mesh_map_id
  use log_mod,             only: log_event, log_scratch_space, &
                                 LOG_LEVEL_TRACE, LOG_LEVEL_ERROR

  implicit none

  private

  type, public :: global_mesh_map_collection_type
    private

    !> Linked list of global_mesh_map_type objects.
    type(linked_list_type) :: global_mesh_map_list

  contains

    !> @brief     Adds a global_mesh_map object to the collection
    !> @param[in] source_global_mesh_id ID of the source global mesh
    !>                                  object for this global mesh map object.
    !> @param[in] target_global_mesh_id ID of the target global mesh
    !>                                  object for this global mesh map object.
    !> @param[in] map
    !>            Global cell ids of the target global mesh object which
    !>            overlap with source global mesh cells. This arrays
    !>            should be in the dimensions of
    !>            [number of target cells for each source cell,
    !>             number of source cells]
    procedure, public :: add_global_mesh_map

    !> @brief     Returns pointer to global_mesh_map object which maps global
    !>            cell ids in the target global mesh to global cell ids in the
    !>            source global mesh.
    !> @param[in] source_global_mesh_id ID of source global mesh
    !>                                  object of requested global_mesh_map
    !>                                  object.
    !> @param[in] target_global_mesh_id ID of target global mesh
    !>                                  object of requested global_mesh_map
    !>                                  object.
    !> @return    global_mesh_map_type <<pointer>>
    procedure, public :: get_global_mesh_map

    procedure, public :: clear
    final             :: global_mesh_map_collection_destructor

  end type global_mesh_map_collection_type

  interface global_mesh_map_collection_type
    module procedure global_mesh_map_collection_constructor
  end interface

contains

!==============================================================================
!> @brief  Constructs the collection object for objects of
!>         global_mesh_map_type
!> @return The constructed global_mesh_map_collection object
function global_mesh_map_collection_constructor() result(self)

  implicit none

  type(global_mesh_map_collection_type) :: self

  self%global_mesh_map_list = linked_list_type()

end function global_mesh_map_collection_constructor

!==============================================================================

subroutine add_global_mesh_map( self,                  &
                                source_global_mesh_id, &
                                target_global_mesh_id, &
                                map )

  implicit none

  class(global_mesh_map_collection_type), intent(inout) :: self
  integer(i_def), intent(in) :: source_global_mesh_id
  integer(i_def), intent(in) :: target_global_mesh_id
  integer(i_def), intent(in) :: map(:,:,:)

  type(global_mesh_map_type) :: global_mesh_map

  integer(i_def) :: global_mesh_map_id
  logical(l_def) :: global_mesh_map_exists

  ! Create the global mesh map id
  global_mesh_map_id = generate_global_mesh_map_id( source_global_mesh_id, &
                                                    target_global_mesh_id )

  ! Query the global mesh map collection to see if this
  ! global mesh map exists
  global_mesh_map_exists = &
      self%global_mesh_map_list%item_exists(global_mesh_map_id)

  if (global_mesh_map_exists) then
    ! Do nothing as map already exists
    write(log_scratch_space, '(A,I0,A)')                            &
        'Skipping task: Global mesh map (id: ', global_mesh_map_id, &
        ') already exists.'
    call log_event(log_scratch_space, LOG_LEVEL_TRACE)
    return

  else

    ! Create the global_mesh_map object and add to the linked list
    global_mesh_map = global_mesh_map_type( source_global_mesh_id, &
                                            target_global_mesh_id, &
                                            map )

    call self%global_mesh_map_list%insert_item( global_mesh_map )

  end if

  return
end subroutine add_global_mesh_map

!==============================================================================

function get_global_mesh_map( self,                   &
                              source_global_mesh_id,  &
                              target_global_mesh_id ) &
                      result( global_mesh_map )

  implicit none

  class(global_mesh_map_collection_type), intent(in) :: self
  integer(i_def), intent(in) :: source_global_mesh_id
  integer(i_def), intent(in) :: target_global_mesh_id

  type(global_mesh_map_type), pointer :: global_mesh_map
  integer(i_def) :: global_mesh_map_id
  logical(l_def) :: global_mesh_map_exists

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type),pointer :: loop => null()

  global_mesh_map => null()

  global_mesh_map_id = generate_global_mesh_map_id( source_global_mesh_id, &
                                                    target_global_mesh_id )

  global_mesh_map_exists = &
      self%global_mesh_map_list%item_exists(global_mesh_map_id)

  if (global_mesh_map_exists) then

    loop => self%global_mesh_map_list%get_head()

    do
      ! Loop over list looking of correct item id
      if ( global_mesh_map_id == loop%payload%get_id() ) then

        select type(m => loop%payload)
        type is (global_mesh_map_type)
          global_mesh_map => m
        end select
        exit

      end if

      loop => loop%next
    end do

  else

    ! Requested map does not exist between this collections source
    ! global_mesh and the target global mesh.
    nullify(global_mesh_map)
    nullify(loop)
    write(log_scratch_space, '(A,I0,A)')                       &
        'Requested global mesh map (id: ', global_mesh_map_id, &
        ') does not exist.'
    call log_event(log_scratch_space, LOG_LEVEL_TRACE)
    return

  end if

  nullify(loop)

end function get_global_mesh_map

!==============================================================================
!> @brief Routine to destroy global_mesh_map_collection.
!> @param[in] self, The calling global_mesh_map_collection instance
subroutine clear(self)

  ! Clear all items from the linked list in the collection
  implicit none

  class(global_mesh_map_collection_type), intent(inout) :: self

  call self%global_mesh_map_list%clear()

  return
end subroutine clear


!-------------------------------------------------------------------------------
!> @brief Finalizer routine which should automatically call clear
!>        when object is out of scope.
!> @param[in] self, The calling quadrature_type
subroutine global_mesh_map_collection_destructor(self)

  implicit none

  type(global_mesh_map_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine global_mesh_map_collection_destructor

end module global_mesh_map_collection_mod
