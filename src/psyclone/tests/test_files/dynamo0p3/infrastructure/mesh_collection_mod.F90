!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds and manages the multiple meshes used in a model run.
!>
!> @details A container which holds a collection of meshes
!>          It will handle the creation and storing of requested meshes.
!
module mesh_collection_mod

  use constants_mod,             only: r_def, i_def, l_def, str_def, imdi
  use mesh_mod,                  only: mesh_type
  use local_mesh_collection_mod, only: local_mesh_collection
  use local_mesh_mod,            only: local_mesh_type
  use log_mod,                   only: log_event, log_scratch_space, &
                                       LOG_LEVEL_WARNING, LOG_LEVEL_TRACE
  use linked_list_mod,           only: linked_list_type, &
                                       linked_list_item_type

  implicit none

  private

  type, public :: mesh_collection_type

    private

    type(linked_list_type) :: mesh_list

  contains
    private
    procedure, public :: add_new_mesh

    procedure, public :: n_meshes
    procedure, public :: get_mesh_names
    procedure, public :: get_mesh_id

    procedure, public :: get_mesh_by_name
    procedure, public :: get_mesh_by_id
    procedure, public :: get_mesh_variant
    generic,   public :: get_mesh => get_mesh_by_id,   &
                                     get_mesh_by_name, &
                                     get_mesh_variant

    procedure, public :: check_for
    procedure, public :: clear

    final             :: mesh_collection_destructor

  end type mesh_collection_type

  interface mesh_collection_type
    module procedure mesh_collection_constructor
  end interface

  ! Module variable allows access to the single mesh collection
  type(mesh_collection_type), public, allocatable :: mesh_collection

contains

!===========================================================================
!> Constructs the mesh collection object
!> @return self The constructed mesh collection object
function mesh_collection_constructor() result(self)

  implicit none

  type(mesh_collection_type) :: self

  self%mesh_list = linked_list_type()

end function mesh_collection_constructor


!===========================================================================
!> @brief Adds a mesh to the mesh collection
!> @param [in] mesh   Mesh object which will be copied into the collection
!> @return A unique identifier for the created mesh
function add_new_mesh( self, mesh ) result( mesh_id )

  implicit none

  class(mesh_collection_type),  intent(inout) :: self
  type(mesh_type),              intent(in)    :: mesh

  integer(i_def) :: mesh_id

  character(str_def) :: mesh_name

  mesh_id  = imdi

  mesh_name = mesh%get_mesh_name()

  ! Check to see if mesh is already in collection
  if (self%check_for(mesh_name)) then
    write(log_scratch_space,'(A)')  &
        'Mesh '//trim(mesh_name)//  &
        ' already present in collection.'
    call log_event(log_scratch_space, LOG_LEVEL_WARNING)
    return
  end if

  mesh_id = mesh%get_id()

  call self%mesh_list%insert_item( mesh )

  return
end function add_new_mesh


!===========================================================================
!> @brief Requests a mesh object with specified mesh name from
!>        the collection.
!>
!> @param[in] mesh_name Name tag of mesh object to retrieve.
!>
!> @return Mesh object with requested mesh name if present in
!>         collection, a null pointer is returned if there is no
!>         mesh with the requested name.
!>
function get_mesh_by_name( self, mesh_name ) result( mesh )

  implicit none

  class(mesh_collection_type), intent(in) :: self
  character(*),                intent(in) :: mesh_name

  type(mesh_type), pointer :: mesh

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type),pointer :: loop => null()

  ! start at the head of the mesh collection linked list
  loop => self%mesh_list%get_head()

  mesh => null()

  do
    ! If list is empty or we're at the end of list and
    ! we didn't find the correct mesh, return a null
    ! pointer.
    if ( .not. associated(loop) ) then
      nullify(mesh)
      exit
    end if

    ! 'cast' to mesh_type
    select type(m => loop%payload)
      type is (mesh_type)
        mesh => m
        if ( mesh_name == mesh%get_mesh_name() ) exit
    end select

    loop => loop%next

  end do

  nullify(loop)

  return

end function get_mesh_by_name


!===========================================================================
!> @brief Requests a mesh object with specified mesh id from
!>        the collection.
!>
!> @param[in] mesh_id ID of mesh object to retrieve.
!>
!> @return Mesh object with requested mesh id if present in
!>         collection, a null pointer is returned if there is no
!>         mesh with the requested id.
!>
function get_mesh_by_id( self, mesh_id ) result( mesh )

  implicit none

  class(mesh_collection_type), intent(in) :: self
  integer(i_def),              intent(in) :: mesh_id

  type(mesh_type), pointer :: mesh

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type),pointer :: loop => null()

  ! start at the head of the mesh collection linked list
  loop => self%mesh_list%get_head()

  mesh => null()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! mesh_id, return a null pointer
    if ( .not. associated(loop) ) then
      nullify(mesh)
      exit
    end if

    ! Otherwise search list for the id we want
    if ( mesh_id == loop%payload%get_id() ) then
      ! 'cast' to the mesh_type
      select type(m => loop%payload)
        type is (mesh_type)
          mesh => m
      end select
      exit
    end if
    loop => loop%next
  end do

  nullify(loop)

end function get_mesh_by_id

!===========================================================================
!> @brief Queries the collection as to the presence of a
!>        mesh object with the specified mesh name.
!>
!> @param[in] mesh_name  Mesh name of object to check for.
!>
!> @return    logical    .true. if global mesh object present
!>                       in collection.
!>
function check_for(self, mesh_name) result(answer)

  implicit none

  class(mesh_collection_type), intent(in) :: self
  character(*),                intent(in) :: mesh_name

  type(mesh_type), pointer :: mesh => null()

  logical :: answer

  answer = .false.
  mesh => self%get_mesh_by_name(mesh_name)
  if ( associated(mesh) ) answer = .true.
  nullify(mesh)

  return
end function check_for


!===========================================================================
!> @brief Returns the number of meshes in the collection.
!>
!> @detail This function returns the number of unique mesh
!>         tag names in this collection.
!>
!> @return Number of mesh tag names availble to query.
!>
function n_meshes(self) result(number_of_meshes)

  implicit none

  class(mesh_collection_type), intent(in) :: self

  integer(i_def) :: number_of_meshes

  number_of_meshes = self%mesh_list%get_length()

  return
end function n_meshes


!===========================================================================
!> @brief Returns mesh tag names of mesh objects in the collection.
!>
!> @return mesh_names  String array <<allocatable>> of mesh names in
!>                     collection.
!>
function get_mesh_names(self) result(mesh_names)

  implicit none

  class(mesh_collection_type), intent(in) :: self

  character(str_def), allocatable :: mesh_names(:)

  integer(i_def) :: n_meshes, i

  type(mesh_type), pointer :: mesh

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type),pointer :: loop => null()

  n_meshes = self%mesh_list%get_length()

  if (n_meshes > 0) then

    allocate(mesh_names(n_meshes))
    mesh => null()

    ! Start at the head of the collection's
    ! linked-list
    loop => self%mesh_list%get_head()

    do i=1, n_meshes
      ! If list is empty or we're at the end of list
      ! and we didn't find a mesh, return a null pointer
      if ( .not. associated(loop) ) then
        nullify(mesh)
        exit
      end if

      ! 'cast' to mesh_type
      select type(m => loop%payload)
        type is (mesh_type)
          mesh => m
          mesh_names(i) = mesh%get_mesh_name()
      end select

      loop => loop%next
    end do

    nullify(loop)
    nullify(mesh)

  end if

  return
end function get_mesh_names


!===========================================================================
!> @brief Returns mesh ID of specified mesh in the collection.
!>
!> @param[in] mesh_name  Mesh name of object to return ID.
!>
!> @return    mesh_id    Integer ID of global mesh object if present
!>                       in collection.
!>
function get_mesh_id( self, mesh_name ) result( mesh_id )

  implicit none

  class(mesh_collection_type), intent(in) :: self
  character(*),                intent(in) :: mesh_name

  integer(i_def) :: mesh_id
  integer(i_def) :: n_meshes
  integer(i_def) :: i
  type(mesh_type), pointer :: mesh

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  mesh_id  = imdi
  n_meshes = self%mesh_list%get_length()

  if (n_meshes > 0) then

    mesh => null()

    ! Start at the head of the collection's
    ! linked-list
    loop => self%mesh_list%get_head()

    do i=1, n_meshes
      ! If list is empty or we're at the end of list
      ! and we didn't find a mesh, return a null pointer
      if ( .not. associated(loop) ) then
        nullify(mesh)
        exit
      end if

      ! 'cast' to global_mesh_type
      select type(m => loop%payload)
        type is (mesh_type)
          mesh => m
          if (mesh_name == mesh%get_mesh_name()) then
            mesh_id = loop%payload%get_id()
            exit
          end if
      end select

      loop => loop%next
    end do

    nullify(loop)
    nullify(mesh)

  end if

  return
end function get_mesh_id


!===========================================================================
!> @brief Returns variant of given mesh with common local mesh.
!>
!> @param[in] mesh            Given mesh with desired common local mesh.
!> @param[in] extrusion_id    Enumerated extrusion id of desired mesh.
!>
!> @return    new_mesh       The desired mesh.
!>
function get_mesh_variant( self, mesh, extrusion_id )  result( variant_mesh )

  implicit none

  class(mesh_collection_type), intent(in) :: self
  type(mesh_type),             intent(in) :: mesh
  integer(i_def),              intent(in) :: extrusion_id

  integer(i_def)                 :: variant_local_mesh_id, local_mesh_id
  integer(i_def)                 :: n_meshes, i
  type(mesh_type),       pointer :: variant_mesh
  type(local_mesh_type), pointer :: variant_local_mesh, local_mesh

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  n_meshes = self%mesh_list%get_length()

  local_mesh => mesh%get_local_mesh()
  local_mesh_id = local_mesh%get_id()

  if (n_meshes > 0) then

    variant_mesh => null()

    ! Start at the head of the collection's linked-list
    loop => self%mesh_list%get_head()

    do i = 1, n_meshes
      ! If list is empty or we're at the end of list
      ! and we didn't find a mesh, return a null pointer
      if ( .not. associated(loop) ) then
        nullify(variant_mesh)
        exit
      end if

      select type(m => loop%payload)
        type is (mesh_type)
          variant_mesh => m
          variant_local_mesh => variant_mesh%get_local_mesh()
          variant_local_mesh_id = variant_local_mesh%get_id()

          if (local_mesh_id == variant_local_mesh_id .and. &
              extrusion_id == variant_mesh%get_extrusion_id()) then
            exit
          else
            nullify(variant_mesh)
          end if
      end select

      loop => loop%next
    end do

    nullify(loop)
    nullify(local_mesh)
    nullify(variant_local_mesh)

  end if

  return
end function get_mesh_variant


!===========================================================================
!> Clear all items from the mesh collection linked list
subroutine clear(self)

  implicit none

  class(mesh_collection_type), intent(inout) :: self

  call self%mesh_list%clear()

end subroutine clear


!===========================================================================
! Mesh collection destructor
subroutine mesh_collection_destructor(self)

  implicit none

  type (mesh_collection_type), intent(inout) :: self

  call self%clear()

end subroutine mesh_collection_destructor


end module mesh_collection_mod
