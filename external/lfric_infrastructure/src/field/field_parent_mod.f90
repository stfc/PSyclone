!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module containing the abstract type that is the parent to all
!>        child field types.
!>
!> @details All concrete field types are part of the same family of fields. This
!>          module contains the abstract type that will be the parent to all
!>          of them

module field_parent_mod

  use constants_mod,               only: i_def, l_def, str_def, imdi, cmdi, &
                                         default_halo_depth
  use function_space_mod,          only: function_space_type
  use halo_routing_collection_mod, only: halo_routing_collection
  use halo_comms_mod,              only: halo_routing_type
  use lfric_mpi_mod,               only: global_mpi, lfric_mpi_type
  use mesh_mod,                    only: mesh_type
  use pure_abstract_field_mod,     only: pure_abstract_field_type

  implicit none

  private

  character(10), parameter :: name_none = 'none'  ! reserved for undefined field names

  !> Abstract field type that is the parent of any field type in the field
  !> object hierarchy
  type, extends(pure_abstract_field_type), public, abstract :: field_parent_type
    private
    !> A pointer to the function space on which the field lives
    type( function_space_type ), pointer :: vspace => null( )
    !> Holds information about how to halo exchange a field
    type(halo_routing_type), pointer :: halo_routing => null()
    !> Flag that holds whether each depth of halo is clean or dirty (dirty=1)
    integer(kind=i_def),allocatable :: halo_dirty(:)
    !> Name of the field. Note the name is immutable once defined via
    !! the initialiser.
    character(str_def) :: name = cmdi
    !> Coupling id for each multidata-level
    integer(kind=i_def), allocatable :: cpl_id( : )
    !> Depth of halo for this field
    integer(kind=i_def) :: field_halo_depth
    !> marker for if a field has been initialised
    logical(kind=l_def) :: initialised = .false.
  contains
    !> Initialiser for a field parent object
    !> @param [in] vector_space The function space that the field lives on
    !> @param [in] name The name of the field. 'none' is a reserved name
    procedure, public :: field_parent_initialiser
    !> Deallocate memory associated with a scalar field_parent_type instance.
    procedure, public :: field_parent_final
    !> Initialise public pointers that belong to the field_parent_type.
    !> @param [inout] field_proxy The field proxy object being initialised.
    procedure, public :: field_parent_proxy_initialiser
    !> Copy the contents of one field_parent_type to another
    !> @param [inout] dest A field_parent object to copy into
    procedure, public :: copy_field_parent
    !> Returns the enumerated integer for the functions_space on which
    !! the field lives
    !> @return fs The enumerated integer for the functions_space
    procedure, public :: which_function_space
    !> Returns a pointer to the function space on which the field lives
    !> @return vspace A pointer to the function space on which the field lives.
    procedure, public :: get_function_space
    !> Returns the mesh used by this field
    !> @return mesh The mesh object associated with the field
    procedure, public :: get_mesh
    !> Return the id of the mesh used by this field
    !> @return mesh_id The id of the mesh object associated with the field
    procedure, public :: get_mesh_id
    !> Returns the horizontal order of the FEM elements
    !> @return elem_h Element order of this field
    procedure, public :: get_element_order_h
    !> Returns the vertical order of the FEM elements
    !> @return elem_v Element order of this field
    procedure, public :: get_element_order_v
    !> Returns the name of the field
    !> @return field name
    procedure, public :: get_name
    !> routine to get coupling id
    procedure         :: get_cpl_id
    !> routine to set coupling id
    procedure         :: set_cpl_id
    !> Return halo depth on this field
    !> @return The halo depth on this field
    procedure, public :: get_field_halo_depth
    !> Returns whether this field has been initialised
    procedure, public :: is_initialised
  end type field_parent_type

  !> Abstract field proxy type that is the patrent of any field proxy
  type, public, abstract :: field_parent_proxy_type
    private
    !> An unused integer that can be used when self needs to be accessed within
    !> a function to avoid compilers complaining about unused variables
    integer(kind=i_def) :: dummy_for_self
    !> A pointer to the function space on which the field lives
    type( function_space_type ), pointer, public :: vspace => null()
    !> Holds metadata information about a field - like dofmap or routing tables
    type(halo_routing_type), public, pointer :: halo_routing => null()
    !> A pointer to the array that holds halo dirtiness
    integer(kind=i_def), public, pointer :: halo_dirty(:) => null()
    !> Depth of halo for this field
    integer(kind=i_def), pointer :: field_halo_depth
  contains
    !> Return the halo depth on this field
    !> @return The halo depth on this field
    procedure, public :: get_field_proxy_halo_depth
    !> Returns the halo routing information
    !> @return The halo routing object to be used in halo exchanges
    procedure, public :: get_halo_routing
    !> Returns whether the halos at the given depth are dirty or clean
    !! @param[in] depth The depth at which to check the halos
    !! @return True if the halos are dirty or false if they are clean
    procedure, public :: is_dirty
    !> Flags all halos as being dirty
    procedure, public :: set_dirty
    !> Flags all the halos up the given depth as clean
    !! @param[in] depth The depth up to which to set the halo to clean
    procedure, public :: set_clean
    !> Perform a blocking halo exchange operation on the field
    procedure(halo_exchange_interface), deferred :: halo_exchange
    !> Returns the mpi object used for this field
    !> @return mpi The MPI object
    procedure, public :: get_mpi
  end type field_parent_proxy_type

!______end of type declarations_______________________________________________

  ! Define the IO interfaces

  abstract interface

    subroutine write_interface(field_name, field_proxy)
      import field_parent_proxy_type
      character(len=*), optional,      intent(in) :: field_name
      class(field_parent_proxy_type ), intent(in) :: field_proxy
    end subroutine write_interface

    subroutine read_interface(field_name, field_proxy)
      import field_parent_proxy_type
      character(len=*),                intent(in)    :: field_name
      class(field_parent_proxy_type ), intent(inout) :: field_proxy
    end subroutine read_interface

    subroutine checkpoint_write_interface(field_name, file_name, field_proxy)
      import field_parent_proxy_type
      character(len=*),                intent(in) :: field_name
      character(len=*),                intent(in) :: file_name
      class(field_parent_proxy_type ), intent(in) :: field_proxy
    end subroutine checkpoint_write_interface

    subroutine checkpoint_read_interface(field_name, file_name, field_proxy)
      import field_parent_proxy_type
      character(len=*),                intent(in)    :: field_name
      character(len=*),                intent(in)    :: file_name
      class(field_parent_proxy_type ), intent(inout) :: field_proxy
    end subroutine checkpoint_read_interface

  end interface

  abstract interface

    subroutine halo_exchange_interface( self, depth )
      use constants_mod, only: i_def
      import field_parent_proxy_type
      implicit none

      class( field_parent_proxy_type), target, intent(inout) :: self
      integer(i_def), intent(in) :: depth
    end subroutine halo_exchange_interface

  end interface

 public :: write_interface
 public :: read_interface
 public :: checkpoint_write_interface
 public :: checkpoint_read_interface
 public :: name_none

contains

  !> Initialise a <code>field_parent_type</code> object.
  !>
  !> @param [inout] self the field_parent object that will be initialised
  !> @param [in] vector_space the function space that the field lives on
  !> @param [in] name The name of the field. 'none' is a reserved name
  !> @param [in] fortran_type The Fortran type of the field data
  !> @param [in] fortran_kind The Fortran kind of the field data
  !> @param [in] field_halo_depth Depth of halo the field will be created with
  !>
  subroutine field_parent_initialiser( self, &
                                       vector_space, &
                                       fortran_type, &
                                       fortran_kind, &
                                       name,         &
                                       halo_depth)
    implicit none

    class(field_parent_type), intent(inout)       :: self
    type(function_space_type), target, intent(in) :: vector_space
    !> The type of data in the field to be halo swapped
    integer(i_def), intent(in)                    :: fortran_type
    !> The kind of data in the field to be halo swapped
    integer(i_def), intent(in)                    :: fortran_kind
    character(*), optional, intent(in)            :: name
    !> Depth of halo this field will be created with
    integer(i_def), optional, intent(in)          :: halo_depth

    type (mesh_type), pointer :: mesh => null()

    self%vspace => vector_space
    mesh => self%vspace%get_mesh()

    ! Set the depth of the halos for this field. If the optional parameter
    ! is not given, use the maximum halo depth offerred by the mesh
    if ( present(halo_depth) ) then
      self%field_halo_depth = halo_depth
    else
      self%field_halo_depth = default_halo_depth
    end if


    ! Fields on a read-only function space can never halo exchanged
    ! - so only need a routing table for writable function spaces
    if ( vector_space%is_writable() ) then
      self%halo_routing => &
        halo_routing_collection%get_halo_routing(                                &
                                             mesh,                               &
                                             vector_space%get_element_order_h(), &
                                             vector_space%get_element_order_v(), &
                                             vector_space%which(),               &
                                             vector_space%get_ndata(),           &
                                             fortran_type,                       &
                                             fortran_kind,                       &
                                             self%field_halo_depth )
    end if

    ! Set the name of the field if given, otherwise default to 'none'
    if (present(name)) then
      self%name = name
    else
      self%name = name_none
    end if

    ! Allow unphysical zero depth halos to support optimised halo exchanges
    allocate(self%halo_dirty(0:self%field_halo_depth))
    self%halo_dirty(0) = 0
    self%halo_dirty(1:) = 1

    nullify(mesh)

    self%initialised = .true.

  end subroutine field_parent_initialiser

  ! Deallocate memory associated with a scalar field_parent_type instance.
  subroutine field_parent_final(self)

    implicit none

    class(field_parent_type), intent(inout)    :: self

    nullify( self%vspace )
    if ( allocated(self%halo_dirty) ) deallocate(self%halo_dirty)

    if(allocated(self%cpl_id)) then
      deallocate(self%cpl_id)
    end if

    self%initialised = .false.

  end subroutine field_parent_final

  ! Initialise public pointers that belong to the field_parent_type.
  subroutine field_parent_proxy_initialiser(self, field_proxy)

   implicit none

   class(field_parent_type), target, intent(in)  :: self
   class(field_parent_proxy_type), intent(inout) :: field_proxy

   field_proxy%vspace           => self%vspace
   field_proxy%halo_routing     => self%halo_routing
   field_proxy%halo_dirty       => self%halo_dirty
   field_proxy%field_halo_depth => self%field_halo_depth

  end subroutine field_parent_proxy_initialiser

  ! Checks if the field parent (so, hence, the field) has been initialised
  function is_initialised(self) result(initialised)
    implicit none
    class(field_parent_type), intent(in) :: self
    logical(l_def) :: initialised

    initialised = self%initialised
  end function is_initialised

  ! Copy the contents of one field_parent_type to another
  subroutine copy_field_parent(self, dest)

    implicit none
    class(field_parent_type), target, intent(in)    :: self
    class(field_parent_type), target, intent(inout) :: dest

    dest%field_halo_depth = self%field_halo_depth
    dest%halo_dirty(:)=self%halo_dirty(:)
  end subroutine copy_field_parent

  ! Function to return the integer id of the function space from the field
  function which_function_space(self) result(fs)
    implicit none
    class(field_parent_type), intent(in) :: self
    integer(i_def) :: fs

    fs = self%vspace%which()
    return
  end function which_function_space

  ! Function to get pointer to function space from the field.
  function get_function_space(self) result(vspace)
    implicit none

    class (field_parent_type), target :: self
    type(function_space_type), pointer :: vspace

    vspace => self%vspace

    return
  end function get_function_space

  ! Function to get mesh information from the field.
  function get_mesh(self) result(mesh)

    implicit none

    class(field_parent_type), intent(in) :: self
    type(mesh_type), pointer :: mesh

    mesh => self%vspace%get_mesh()

  end function get_mesh

  ! Function to get mesh id from the field.
  function get_mesh_id(self) result(mesh_id)
    implicit none

    class (field_parent_type) :: self

    type(mesh_type), pointer :: mesh => null()
    integer(i_def) :: mesh_id

    mesh => self%vspace%get_mesh()
    mesh_id = mesh%get_id()

    mesh => null()

    return
  end function get_mesh_id

  ! Function to get horizontal element order from the field.
  function get_element_order_h(self) result(elem_h)
    implicit none

    class (field_parent_type) :: self
    integer(i_def) :: elem_h

    elem_h = self%vspace%get_element_order_h()

    return
  end function get_element_order_h

  ! Function to get vertical element order from the field.
  function get_element_order_v(self) result(elem_v)
    implicit none

    class (field_parent_type) :: self
    integer(i_def) :: elem_v

    elem_v = self%vspace%get_element_order_v()

    return
  end function get_element_order_v

  !> Returns the name of the field
  function get_name(self) result(name)

    implicit none

    class(field_parent_type), intent(in) :: self
    character(str_def) :: name

    name = self%name

  end function get_name

  ! Returns the halo depth of the field.
  function get_field_halo_depth(self) result(field_halo_depth)

    implicit none

    class(field_parent_type), intent(in) :: self
    integer(i_def) :: field_halo_depth

    field_halo_depth = self%field_halo_depth

  end function get_field_halo_depth

  ! Returns the halo routing information
  function get_halo_routing(self) result(halo_routing)

    implicit none

    class(field_parent_proxy_type), intent(in) :: self
    type(halo_routing_type), pointer :: halo_routing

    halo_routing => self%halo_routing

  end function get_halo_routing

  ! Returns true if a halo depth is dirty
  function is_dirty(self, depth) result(dirtiness)

    use log_mod,         only : log_event, &
                                LOG_LEVEL_ERROR
    implicit none

    class(field_parent_proxy_type), intent(in) :: self
    integer(i_def), intent(in) :: depth
    logical(l_def) :: dirtiness

    if( depth > self%field_halo_depth ) &
      call log_event( 'Error in field: '// &
                      'call to is_dirty() with depth out of range.', &
                      LOG_LEVEL_ERROR )

    dirtiness = .false.
    if(self%halo_dirty(depth) == 1)dirtiness = .true.

  end function is_dirty

  ! Sets a halo depth to be flagged as dirty
  subroutine set_dirty( self )

    implicit none

    class(field_parent_proxy_type), intent(inout) :: self

    ! Leave zero depth clean
    self%halo_dirty(1:) = 1

  end subroutine set_dirty

  ! Sets the halos up to depth to be flagged as clean
  subroutine set_clean(self, depth)

    use log_mod,         only : log_event, &
                                LOG_LEVEL_ERROR
    implicit none

    class(field_parent_proxy_type), intent(inout) :: self
    integer(i_def), intent(in) :: depth

    if( depth > self%field_halo_depth ) &
      call log_event( 'Error in field: '// &
                      'call to set_clean() with depth out of range.', &
                      LOG_LEVEL_ERROR )

    self%halo_dirty(1:depth) = 0
  end subroutine set_clean

  !> Returns the mpi object this field is built on
  function get_mpi(self) result(mpi)

    implicit none

    class(field_parent_proxy_type), intent(in) :: self
    type(lfric_mpi_type) :: mpi
    integer              :: value_tmp

    ! This is a placeholder for when fields can be built on different mpi
    ! objects. To support future use, it currently just returns the global
    ! mpi object.

    ! In the future, "self" will be needed, but not at the moment. To stop
    ! compilers complaining about unused variables, make some trivial use
    value_tmp = self%dummy_for_self

    mpi = global_mpi

  end function get_mpi

  !> Function to get coupling id from the field.
  !>
  !> @param[in] i_multidata_lev multidata-level for which coupling id is requested
  !> @return field coupling id
  function get_cpl_id(self, i_multidata_lev) result(dcpl_id)
    use log_mod, only : log_event, log_scratch_space, LOG_LEVEL_ERROR
    implicit none

    class (field_parent_type), intent(in) :: self
    integer(i_def), intent(in)     :: i_multidata_lev

    integer(i_def) :: dcpl_id
    type(function_space_type), pointer :: function_space => null()

    function_space => self%get_function_space()

    if(i_multidata_lev > function_space%get_ndata()) then
       write(log_scratch_space,'(A,I3,A,I3,A,A)')&
       'get_cpl_id: multidata-level requested ', i_multidata_lev, &
       ' larger than number of multidata-levels available (', &
       function_space%get_ndata(), &
       ') for', self%name
       call log_event(log_scratch_space,LOG_LEVEL_ERROR )
       dcpl_id = imdi
    elseif(allocated(self%cpl_id)) then
       dcpl_id = self%cpl_id(i_multidata_lev)
    else
       dcpl_id = imdi
    endif

    return
  end function get_cpl_id

  !> subroutine to set coupling id of the field.
  !>
  !> @param[in] dcpl_id oasis id of the field
  !> @param[in] i_multidata_lev multidata-level for which coupling id is set
  subroutine set_cpl_id(self, dcpl_id, i_multidata_lev)
    use log_mod, only : log_event, log_scratch_space, LOG_LEVEL_ERROR
    implicit none

    class (field_parent_type), intent(inout) :: self
    integer(i_def),     intent(in   ) :: dcpl_id, i_multidata_lev

    type(function_space_type), pointer :: function_space => null()

    function_space => self%get_function_space()

    if(i_multidata_lev > function_space%get_ndata()) then
      write(log_scratch_space,'(A,I3,A,I3,A,A)')&
      'set_cpl_id: multidata-level requested (', i_multidata_lev, &
      ') larger than number of multidata-levels available (', &
      function_space%get_ndata(), &
      ') for ', self%name
      call log_event(log_scratch_space,LOG_LEVEL_ERROR )
    endif

    if(allocated(self%cpl_id)) then
       self%cpl_id(i_multidata_lev) = dcpl_id
    else
       allocate(self%cpl_id(function_space%get_ndata()))
       self%cpl_id(:) = imdi
       self%cpl_id(i_multidata_lev) = dcpl_id
    endif

  end subroutine set_cpl_id

  ! Returns the halo depth of the field.
  function get_field_proxy_halo_depth(self) result(field_halo_depth)

    implicit none

    class(field_parent_proxy_type), intent(in) :: self
    integer(i_def) :: field_halo_depth

    field_halo_depth = self%field_halo_depth

  end function get_field_proxy_halo_depth

end module field_parent_mod
