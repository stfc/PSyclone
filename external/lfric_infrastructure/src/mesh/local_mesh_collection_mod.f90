!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds and manages the multiple local meshes used in a model run.
!>
!> @details A container which holds a collection of local meshes
!>          It will handle the storing of requested meshes to prevent them
!>          going out of scope.
!
module local_mesh_collection_mod

  use constants_mod,      only: i_def, str_def
  use local_mesh_mod,     only: local_mesh_type
  use linked_list_mod,    only: linked_list_type, &
                                linked_list_item_type

  implicit none

  private

  type, public :: local_mesh_collection_type
    private
    !> The list of local meshes
    type(linked_list_type) :: local_mesh_list
  contains
    procedure, public  :: add_new_local_mesh
    procedure, public  :: get_mesh_by_name
    procedure, public  :: get_mesh_by_id
    generic,   public  :: get_local_mesh => get_mesh_by_id, &
                                            get_mesh_by_name
    procedure, public  :: get_mesh_names
    procedure, public  :: check_for

    procedure, public  :: clear
    final              :: local_mesh_collection_destructor
  end type local_mesh_collection_type

  interface local_mesh_collection_type
    module procedure local_mesh_collection_constructor
  end interface

  !> @brief Singleton instance of a local mesh collection_type object.
  !>
  type(local_mesh_collection_type), public, allocatable :: &
       local_mesh_collection

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Constructs an empty local_mesh_collection object.
  !>
  !> @return The constructed local mesh collection object.
  !>
  function local_mesh_collection_constructor() result(self)

    implicit none

    type(local_mesh_collection_type) :: self

    self%local_mesh_list = linked_list_type()

  end function local_mesh_collection_constructor


  !===========================================================================
  !> @brief Destructor tears down object prior to being freed.
  !>
  subroutine local_mesh_collection_destructor(self)

    ! Object finalizer
    implicit none

    type (local_mesh_collection_type), intent(inout) :: self

    call self%clear()

    return
  end subroutine local_mesh_collection_destructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Places a copy of the provided local mesh object in the collection
  !>
  !> @param[in] local_mesh_to_add The local mesh object to be added to the
  !>                               collection
  !> @return    local_mesh_id The id of the mesh that has beed added to the
  !>                          collection
  !>
  function add_new_local_mesh( self, local_mesh_to_add ) result( local_mesh_id )
    implicit none

    class(local_mesh_collection_type), intent(inout) :: self
    type (local_mesh_type),            intent(in)    :: local_mesh_to_add

    integer(i_def)          :: local_mesh_id

    ! Add local mesh to end of linked list
    call self%local_mesh_list%insert_item( local_mesh_to_add )

    local_mesh_id = local_mesh_to_add%get_id()

  end function add_new_local_mesh


  !===========================================================================
  !> @brief Returns mesh tag names of mesh objects in the collection.
  !>
  !> @return mesh_names  String array <<allocatable>> of mesh names in
  !>                     collection.
  !>
  function get_mesh_names( self ) result( mesh_names )

    implicit none

    class(local_mesh_collection_type) :: self
    character(str_def), allocatable :: mesh_names(:)

    integer(i_def) :: n_meshes, i

    type(local_mesh_type), pointer :: local_mesh

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    n_meshes = self%local_mesh_list%get_length()

    if (n_meshes > 0) then

      allocate(mesh_names(n_meshes))
      local_mesh => null()

      ! Start at the head of the collection's
      ! linked-list
      loop => self%local_mesh_list%get_head()

      do i=1, n_meshes
        ! If list is empty or we're at the end of list
        ! and we didn't find a mesh, return an
        ! unallocated array.
        if ( .not. associated(loop) ) then
          nullify(local_mesh)
          exit
        end if

        ! 'cast' to local_mesh_type
        select type(m => loop%payload)
          type is (local_mesh_type)
            local_mesh => m
            mesh_names(i) = local_mesh%get_mesh_name()
        end select

        loop => loop%next
      end do

      nullify(loop)
      nullify(local_mesh)

    end if

    return
  end function get_mesh_names


  !===========================================================================
  !> @brief   Requests a local mesh object with the specified name
  !>          from the collection.
  !> @details The name of the local mesh is determined by the name given
  !>          to the mesh topology in the global mesh input file.
  !>
  !> @param[in] local_mesh_name Name tag of local mesh object to retrieve.
  !>
  !> @return    local_mesh      Pointer to the local mesh object with
  !>                            requested name if present in collection.
  !>                            A null pointer is returned if there is no
  !>                            mesh with the requested name.
  !>
  function get_mesh_by_name( self, local_mesh_name ) result( local_mesh )

    implicit none

    class(local_mesh_collection_type) :: self
    character(*), intent(in)          :: local_mesh_name

    type(local_mesh_type), pointer    :: local_mesh


    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type),pointer :: loop => null()

    ! start at the head of the mesh collection linked list
    loop => self%local_mesh_list%get_head()

    local_mesh => null()
    do
      ! If list is empty or we're at the end of list and we didn't find the
      ! mesh_id, return a null pointer
      if ( .not. associated(loop) ) then
        nullify(local_mesh)
        exit
      end if

      ! Otherwise search list for the name we want
      select type(list_item => loop%payload)
      type is (local_mesh_type)
        if ( trim(local_mesh_name) == trim(list_item%get_mesh_name()) ) then
          local_mesh => list_item
          exit
        end if
      end select

      loop => loop%next
    end do

    nullify(loop)

  end function get_mesh_by_name


  !===========================================================================
  !> @brief   Requests a local mesh object with specified ID from
  !>          the collection.
  !> @details The mesh ID is an internal integer ID assigned when the
  !>          mesh is instantiated. It is dependent on the order in which
  !>          the local mesh objects were created (not to be confused with
  !>          the order in which they were added to the collection).
  !>
  !> @param[in] local_mesh_id   ID of mesh to retrieve.
  !>
  !> @return    local_mesh      Pointer to local mesh object with
  !>                            the requested ID if present in collection.
  !>                            A null pointer is returned if there is no
  !>                            mesh with the requested ID.
  !>
  function get_mesh_by_id( self, local_mesh_id ) result( local_mesh )

    implicit none

    class(local_mesh_collection_type) :: self
    integer(i_def), intent(in)        :: local_mesh_id

    type(local_mesh_type), pointer    :: local_mesh


    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type),pointer :: loop => null()

    ! start at the head of the mesh collection linked list
    loop => self%local_mesh_list%get_head()

    local_mesh => null()
    do
      ! If list is empty or we're at the end of list and we didn't find the
      ! mesh_id, return a null pointer
      if ( .not. associated(loop) ) then
        nullify(local_mesh)
        exit
      end if

      ! Otherwise search list for the id we want
      if ( local_mesh_id == loop%payload%get_id() ) then
        ! 'cast' to the local_mesh_type
        select type(m => loop%payload)
          type is (local_mesh_type)
            local_mesh => m
        end select
        exit
      end if
      loop => loop%next
    end do

    nullify(loop)

  end function get_mesh_by_id


  !===========================================================================
  !> @brief Check whether a particular named mesh is present in the
  !>        mesh collection.
  !>
  !> @param[in] mesh_name  Mesh name of object to check for.
  !> @return    answer     .true. if global mesh object present
  !>                       in collection.
  !>
  function check_for(self, mesh_name) result(answer)

    implicit none

    class(local_mesh_collection_type), intent(in) :: self
    character(str_def),                intent(in) :: mesh_name

    type(local_mesh_type), pointer :: mesh => null()

    logical :: answer

    answer = .false.
    mesh => self%get_mesh_by_name(mesh_name)
    if ( associated(mesh) ) answer = .true.
    nullify(mesh)

    return
  end function check_for


  !===========================================================================
  !> @brief Forced clear of all the local mesh objects in the collection.
  !>
  !> This routine should not need to be called manually except (possibly) in
  !> pfunit tests
  !>
  subroutine clear(self)

    ! Clear all items from the linked list in the collection
    implicit none

    class(local_mesh_collection_type), intent(inout) :: self

    call self%local_mesh_list%clear()

    return
  end subroutine clear

end module local_mesh_collection_mod
