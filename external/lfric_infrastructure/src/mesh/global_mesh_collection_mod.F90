!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief   Holds and manages the multiple global meshes used to setup a model
!>          run.
!>
!> @details A container which holds a collection of global meshes.
!>          It will handle the creation and storing of requested global meshes.
!
module global_mesh_collection_mod

  use constants_mod,         only : r_def, i_def, IMDI,               &
                                    str_max_filename, str_def
  use global_mesh_mod,       only : global_mesh_type
  use linked_list_mod,       only : linked_list_type, linked_list_item_type
  use log_mod,               only : log_event, log_scratch_space,     &
                                    LOG_LEVEL_ERROR, LOG_LEVEL_TRACE, &
                                    LOG_LEVEL_WARNING

  implicit none

  private

  type, public :: global_mesh_collection_type

    private

    ! List of the global_mesh_type objects in this collection.
    type(linked_list_type) :: global_mesh_list

    ! Number of panels in the mesh layout. npanels is set to be the same as
    ! the 1st global mesh loaded into the collection. All subsequent global
    ! meshes added should have been specified with the same nume of panels.
    !> @deprecated  Once multiple global meshes and associated mappings
    !>              are available in ugrid files.

    ! At present, global_mesh_type objects which are described from ugrid
    ! files which contain details for only one global mesh and thus contain no
    ! information about mapping between global meshes at different resolutions.
    ! As a consequence of this, mappings are calculated by the
    ! global_mesh_collection as between subsequent global meshes as they are
    ! added to the global mesh_collection. This calculation requires that all
    ! meshes have the same number of panels in the mesh. npanels is set to
    ! be the same as the 1st global mesh loaded into the collection.
    integer(i_def)         :: npanels = IMDI

    ! Pointer to global_mesh_type object in linked list. This global mesh
    ! object will be use as the source mesh when for global mesh map creation
    ! when the next global mesh is added to the collection.
    ! THIS IS TEMPORARY AND SHOULD BE REMOVED WHEN GLOBAL MESH MAPS ARE
    ! READ DIRECTLY FROM THE UGRID MESH FILE
    type(global_mesh_type),  pointer :: source_global_mesh => null()

  contains
    procedure, public  :: add_new_global_mesh
    procedure, public  :: add_unit_test_global_mesh

    procedure, public  :: n_meshes
    procedure, public  :: get_mesh_names
    procedure, public  :: get_mesh_id

    procedure, public  :: get_mesh_by_name
    procedure, public  :: get_mesh_by_id
    generic,   public  :: get_global_mesh => get_mesh_by_id, &
                                             get_mesh_by_name
    procedure, public  :: check_for

    procedure, public  :: clear
    final              :: global_mesh_collection_destructor

  end type global_mesh_collection_type

  interface global_mesh_collection_type
    module procedure global_mesh_collection_constructor
  end interface

  !> @brief Singleton instance of a global_mesh_collection_type object.
  !>
  type(global_mesh_collection_type), public, allocatable :: &
      global_mesh_collection

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Constructs an empty mesh collection object.
  !>
  !> @return The constructed mesh collection object.
  !>
  function global_mesh_collection_constructor() result(self)

    implicit none

    type(global_mesh_collection_type) :: self

    self%global_mesh_list = linked_list_type()

  end function global_mesh_collection_constructor


  !===========================================================================
  !> @brief Destructor tears down object prior to being freed.
  !>
  subroutine global_mesh_collection_destructor(self)

    ! Object finalizer
    implicit none

    type (global_mesh_collection_type), intent(inout) :: self

    call self%clear()

    return
  end subroutine global_mesh_collection_destructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Adds a global mesh object to the collection
  !>
  !> Maps between each global mesh are created on the fly between subsequent
  !> global meshes in the collection in the order that they are added.
  !>
  !> At present, there is no way to uniquely identify each global mesh that
  !> is read in. Global meshes are currently identified by an integer ID
  !> which is assigned on creation of the global mesh object.
  !>
  !> When multiple global meshes per file are possible there will be a need
  !> to use a hash function to identify each global mesh object read in.
  !>
  !> @param[in] global_mesh_to_add The global mesh object to be added to the
  !>                               collection
  !>
  subroutine add_new_global_mesh( self, global_mesh_to_add )
    implicit none

    class(global_mesh_collection_type), intent(inout) :: self
    type (global_mesh_type),            intent(in)    :: global_mesh_to_add

    character(str_def)      :: global_mesh_name

    global_mesh_name = global_mesh_to_add%get_mesh_name()

    ! Read in the requested mesh topology from file.
    !=================================================================
    ! Check list of tag names to see if mesh is already in collection
    ! As these are assumed to be read in from the same mesh input
    ! file specifed for a given run, if the name is already in the
    ! collection it will be the same mesh.
    if (self%check_for(global_mesh_name)) then
      write(log_scratch_space,'(A)')        &
          'Mesh '//trim(global_mesh_name)// &
          ' already present in collection.'
      call log_event(log_scratch_space, LOG_LEVEL_WARNING)
      return
    end if

    ! Now add the requested mesh to the collection
    call self%global_mesh_list%insert_item( global_mesh_to_add )

    return
  end subroutine add_new_global_mesh


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Returns the number of meshes in the collection.
  !>
  !> @detail This function returns the number of unique mesh
  !>         tag names in this collection.
  !>
  !> @return Number of mesh tag names avaible to query.
  !>
  function n_meshes(self) result(number_of_meshes)

    implicit none

    class(global_mesh_collection_type), intent(in) :: self

    integer(i_def) :: number_of_meshes

    number_of_meshes = self%global_mesh_list%get_length()

    return
  end function n_meshes


  !===========================================================================
  !> @brief Adds hardwired global mesh object to the collection (for unit
  !>        testing).
  !>
  !> @return ID of the global mesh added to collection.
  !>
  function add_unit_test_global_mesh(self) result(global_mesh_id)

    implicit none

    class(global_mesh_collection_type), intent(inout) :: self
    integer(i_def) :: global_mesh_id

    type (global_mesh_type) :: global_mesh

    integer(i_def)     :: n_global_meshes
    character(str_def) :: global_mesh_name

    n_global_meshes = self%global_mesh_list%get_length()

    if (n_global_meshes < 1) then
      self%npanels   = 1
    else
      if (self%npanels /= 1) then
        write(log_scratch_space,'(A,I0,A)')        &
            'This global mesh collection is '//    &
            'for global meshes of comprising of ', &
            self%npanels, ' panels.'

        call log_event(log_scratch_space,LOG_LEVEL_ERROR)
      end if
    end if

    global_mesh      = global_mesh_type()
    global_mesh_name = global_mesh%get_mesh_name()
    global_mesh_id   = global_mesh%get_id()
    call self%global_mesh_list%insert_item( global_mesh )

    return
  end function add_unit_test_global_mesh


  !===========================================================================
  !> @brief   Requests a global mesh object with the specified mesh name
  !>          from the collection.
  !> @details The name of the global mesh is determined by the name given
  !>          to the mesh topology in the mesh input file.
  !>
  !> @param[in] global_mesh_name Name tag of global mesh object to retrieve.
  !>
  !> @return    global_mesh      Pointer to global mesh object with
  !>                             requested name if present in collection.
  !>                             A null pointer is returned if there is no
  !>                             mesh with the requested name.
  !>
  function get_mesh_by_name( self, global_mesh_name ) result( global_mesh )

    implicit none

    class(global_mesh_collection_type), intent(in) :: self
    character(str_def),                 intent(in) :: global_mesh_name

    type(global_mesh_type), pointer :: global_mesh

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type),pointer :: loop => null()

    ! Start at the head of the mesh collection linked list
    loop => self%global_mesh_list%get_head()

    global_mesh => null()

    do
      ! If list is empty or we're at the end of list and
      ! we didn't find the correct mesh, return a null
      ! pointer.
      if ( .not. associated(loop) ) then
        nullify(global_mesh)
        exit
      end if

      ! 'cast' to the global_mesh_type
      select type(m => loop%payload)
        type is (global_mesh_type)
          global_mesh => m
          if ( global_mesh_name == global_mesh%get_mesh_name() ) exit
      end select

      loop => loop%next

    end do

    nullify(loop)

    return
  end function get_mesh_by_name


  !===========================================================================
  !> @brief   Requests a global mesh object with specified mesh ID from
  !>          the collection.
  !> @details The mesh ID is an internal integer ID assigned when the
  !>          mesh is instantiated. It is dependent on the order in which
  !>          the global mesh objects were created (not to be confused with
  !>          the order in which they were added to the collection).
  !>
  !> @param[in] global_mesh_id   ID of mesh to retrieve.
  !>
  !> @return    global_mesh      Pointer to global mesh object with
  !>                             the requested ID if present in collection.
  !>                             A null pointer is returned if there is no
  !>                             mesh with the requested ID.
  !>
  function get_mesh_by_id( self, global_mesh_id ) result( global_mesh )

    implicit none

    class(global_mesh_collection_type) :: self
    integer(i_def), intent(in)         :: global_mesh_id

    type(global_mesh_type), pointer    :: global_mesh


    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    ! start at the head of the mesh collection linked list
    loop => self%global_mesh_list%get_head()

    global_mesh => null()
    do
      ! If list is empty or we're at the end of list and we didn't find the
      ! mesh_id, return a null pointer
      if ( .not. associated(loop) ) then
        nullify(global_mesh)
        exit
      end if

      ! Otherwise search list for the id we want
      if ( global_mesh_id == loop%payload%get_id() ) then
        ! 'cast' to the global_mesh_type
        select type(m => loop%payload)
          type is (global_mesh_type)
            global_mesh => m
        end select
        exit
      end if
      loop => loop%next
    end do

    nullify(loop)

  end function get_mesh_by_id


  !===========================================================================
  !> @brief Returns mesh tag names of mesh objects in the collection.
  !>
  !> @return mesh_names  String array <<allocatable>> of mesh names in
  !>                     collection.
  !>
  function get_mesh_names( self ) result( mesh_names )

    implicit none

    class(global_mesh_collection_type) :: self
    character(str_def), allocatable :: mesh_names(:)

    integer(i_def) :: n_meshes, i

    type(global_mesh_type), pointer :: global_mesh

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    n_meshes = self%global_mesh_list%get_length()

    if (n_meshes > 0) then

      allocate(mesh_names(n_meshes))
      global_mesh => null()

      ! Start at the head of the collection's
      ! linked-list
      loop => self%global_mesh_list%get_head()

      do i=1, n_meshes
        ! If list is empty or we're at the end of list
        ! and we didn't find a mesh, return a null pointer
        if ( .not. associated(loop) ) then
          nullify(global_mesh)
          exit
        end if

        ! 'cast' to global_mesh_type
        select type(m => loop%payload)
          type is (global_mesh_type)
            global_mesh => m
            mesh_names(i) = global_mesh%get_mesh_name()
        end select

        loop => loop%next
      end do

      nullify(loop)
      nullify(global_mesh)

    end if

    return
  end function  get_mesh_names


  !===========================================================================
  !> @brief Returns mesh ID specified mesh in the collection.
  !>
  !> @param[in] mesh_name  Mesh name of object to return ID.
  !>
  !> @return    mesh_id    Integer ID of global mesh object if present
  !>                       in collection.
  !>
  function get_mesh_id( self, mesh_name ) result( mesh_id )

    implicit none

    class(global_mesh_collection_type) :: self
    character(str_def), intent(in) :: mesh_name

    integer(i_def) :: mesh_id

    integer(i_def) :: n_meshes, i
    type(global_mesh_type), pointer :: global_mesh

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    mesh_id  = imdi
    n_meshes = self%global_mesh_list%get_length()

    if (n_meshes > 0) then

      global_mesh => null()

      ! Start at the head of the collection's
      ! linked-list
      loop => self%global_mesh_list%get_head()

      do i=1, n_meshes
        ! If list is empty or we're at the end of list
        ! and we didn't find a mesh, return a null pointer
        if ( .not. associated(loop) ) then
          nullify(global_mesh)
          exit
        end if

        ! 'cast' to global_mesh_type
        select type(m => loop%payload)
          type is (global_mesh_type)
            global_mesh => m
            if (mesh_name == global_mesh%get_mesh_name()) then
              mesh_id = loop%payload%get_id()
              exit
            end if
        end select

        loop => loop%next
      end do

      nullify(loop)
      nullify(global_mesh)

    end if

    return
  end function get_mesh_id


  !===========================================================================
  !> @brief Queries the collection as to the presence of a
  !>        mesh object with the specified mesh name in the collection
  !>
  !> @param[in] global_mesh_name  Mesh name of object to check for.
  !>
  !> @return    logical          .true. if global mesh object present
  !>                             in collection.
  !>
  function check_for(self, global_mesh_name) result(answer)

    implicit none

    class(global_mesh_collection_type), intent(in) :: self
    character(str_def),                 intent(in) :: global_mesh_name

    type(global_mesh_type), pointer :: global_mesh => null()

    logical :: answer

    answer = .false.
    global_mesh => self%get_mesh_by_name(global_mesh_name)
    if ( associated(global_mesh) ) answer = .true.
    nullify(global_mesh)

    return
  end function check_for


  !===========================================================================
  !> @brief Forced clear of all the global mesh objects in the collection.
  !>
  !> This routine should not need to be called manually - except:
  !> (possibly) in pfunit tests, or
  !> (temporarily) to make sure global_meshes aren't used in model code
  !> after local_meshes have been created from them. (This can be removed
  !> when local_meshes are read directly into the model and global_meshes
  !> have been removed completely from model code)
  !>
  subroutine clear(self)

    ! Clear all items from the linked list in the collection
    implicit none

    class(global_mesh_collection_type), intent(inout) :: self

    call self%global_mesh_list%clear()

    nullify(self%source_global_mesh)

    return
  end subroutine clear

end module global_mesh_collection_mod
