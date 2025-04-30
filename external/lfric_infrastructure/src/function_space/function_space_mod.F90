!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds information about the function space.
!>
!> @details A container which holds type definition of the function space and
!>          holds a number of static copies of the function spaces required
!>          by the model. It provides accessor functions (getters) to various
!>          information held in the type.
!
module function_space_mod

  use constants_mod,        only : i_def, i_halo_index, l_def, r_def
  use mesh_mod,             only : mesh_type
  use master_dofmap_mod,    only : master_dofmap_type
  use stencil_dofmap_helper_functions_mod, &
                            only : generate_stencil_dofmap_id
  use log_mod,              only : log_event, log_scratch_space,               &
                                   LOG_LEVEL_DEBUG, LOG_LEVEL_ERROR,           &
                                   LOG_LEVEL_INFO
  use fs_continuity_mod,    only : W0, W1, W2, W3, Wtheta, W2broken, W2trace,  &
                                   W2Htrace, W2Vtrace, W2V, W2H, Wchi,         &
                                   W2Hbroken
  use function_space_constructor_helper_functions_mod, &
                            only : ndof_setup, basis_setup, dofmap_setup,      &
                                   levels_setup, generate_fs_id
  use linked_list_data_mod, only : linked_list_data_type
  use linked_list_mod,      only : linked_list_type, linked_list_item_type
  use mesh_collection_mod,  only : mesh_collection

  implicit none

  private

  integer(i_def), public, parameter :: BASIS = 100
  integer(i_def), public, parameter :: DIFF_BASIS = 101

  !-----------------------------------------------------------------------------
  ! Public types
  !-----------------------------------------------------------------------------

  type, extends(linked_list_data_type), public :: function_space_type

    private

    !> Number of degrees of freedom associated with each cell
    integer(i_def) :: ndof_cell

    !> Number of unique degrees of freedom located on
    !> the 3D mesh associated with this function space.
    integer(i_def) :: ndof_glob

    !> Number of degrees of freedom associated with each cell
    integer(i_def) :: ndof_interior

    !> Number of degrees of freedom associated with each cell
    integer(i_def) :: ndof_exterior

    !> Number of degrees of freedom located on cell vertex entities.
    integer(i_def) :: ndof_vert

    !> Number of degrees of freedom located on horizontal cell edge entities
    !> (edges which lie in a plane of constant z).
    integer(i_def) :: ndof_edge_h

    !> Number of degrees of freedom located on vertical cell edge entities
    !> (edges which lie in a plane of contant x or y).
    integer(i_def) :: ndof_edge_v

    !> Number of degrees of freedom located on horizontal cell face entities
    !> (faces whose normal vectors have 0 z-component).
    integer(i_def) :: ndof_face_h

    !> Number of degrees of freedom located on vertical cell face entities
    !> (faces whose normal vectors have 0 x and y components).
    integer(i_def) :: ndof_face_v

    !> Number of degrees of freedom located on cell volume entities.
    integer(i_def) :: ndof_vol

    !> Integer value for Gungho functions spaces, e.g. W0 would be 1
    integer(i_def) :: fs

    !> Element base-order of Gungho function space in horizontal direction
    integer(i_def) :: element_order_h

    !> Element base-order of Gungho function space in vertical direction
    integer(i_def) :: element_order_v

    ! Function space polynomial order? dynamics is still to provide us
    ! with a name for this, same as element order except for W0
    ! where is it equal to element_order_h/v+1, in either horizontal or vertical
    integer(i_def) :: fs_order_h
    integer(i_def) :: fs_order_v

    !> The number of data values to be held at each dof location
    integer(i_def) :: ndata

    !> Flag describes order of data. False=layer first, true=multi-data first
    logical(l_def) :: ndata_first

    !> Number of dimensions in this function space
    integer(i_def) :: dim_space

    !> Number of dimensions in this function space when differentiated
    integer(i_def) :: dim_space_diff

    !> A two dimensional, allocatable array which holds the indirection map or
    !> dofmap for the whole function space over the bottom level of the domain.
    type(master_dofmap_type) :: master_dofmap

    !> Mesh object used to create this function space. This is a
    !> pointer to a mesh in a linked list of mesh objects
    type(mesh_type), pointer :: mesh => null()

    !> A two dimensional, allocatable array of reals which holds the coordinates
    !> of the function_space degrees of freedom
    real(r_def), allocatable :: nodal_coords(:,:)

    !> A two dimensional, allocatable, integer array which specifies which
    !> dofs are on vertex boundarys
    integer(i_def), allocatable :: dof_on_vert_boundary(:,:)

    !> An allocatable array of labels (integers) which maps degree of freedom
    !> index to the geometric entity
    !> (V - Volume, W - West face, T - Top face, etc.)
    integer(i_def), allocatable :: entity_dofs(:)

    !> An array to hold an ordered, unique list of levels for output
    !> of fields on this function space
    real(r_def), allocatable :: fractional_levels(:)

    !> @}
    !> @name Arrays needed for on the fly basis evaluations
    integer(i_def), allocatable :: basis_order(:,:)
    integer(i_def), allocatable :: basis_index(:,:)
    real(r_def),    allocatable :: basis_vector(:,:)
    real(r_def),    allocatable :: basis_x(:,:,:)
    real(r_def),    allocatable :: basis_z(:,:)
    !> @}

    !> A one dimensional, allocatable array which holds a unique global index
    !> for every dof in the local domain
    integer(i_halo_index), allocatable :: global_dof_id(:)

    !> A one dimensional, allocatable array which holds a unique global index
    !> for cell dofs in the 2D horizontal portion of the local domain
    integer(i_def), allocatable :: global_cell_dof_id_2d(:)

    !> A one dimensional, allocatable array which holds a unique global index
    !> for edge dofs in the 2D horizontal portion of the local domain
    integer(i_def), allocatable :: global_edge_dof_id_2d(:)

    !> A one dimensional, allocatable array which holds a unique global index
    !> for vertex dofs in the 2D horizontal portion of the local domain
    integer(i_def), allocatable :: global_vert_dof_id_2d(:)

    !> The index within the dofmap of the last "owned" dof
    integer(i_def) :: last_dof_owned

    !> The index within the dofmap of the last "annexed" dof
    !> ("Annexed" dofs that those that are not owned, but are on owned cells)
    integer(i_def) :: last_dof_annexed

    !> A one dimensional, allocatable array which holds the index in the dofmap
    !> of the last of the halo dofs (from the various depths of halo)
    integer(i_def), allocatable :: last_dof_halo(:)

    !> A linked list of stencil dofmaps
    type(linked_list_type) :: dofmap_list

    !> Flag holds whether fields on this function space will be readonly
    logical(l_def) :: readonly

  contains

    !> @brief Gets the total number of unique degrees of freedom for this space,
    !> @return Integer Total number of unique degrees of freedom
    procedure, public :: get_undf

    !> @brief Gets the total number of unique degrees of freedom located on
    !>        the 3D mesh associated with this function space.
    !> @return Integer Total number of unique degrees of freedom
    procedure, public :: get_ndof_glob

    !> @brief Returns the number of cells in a horizontal 2D layer
    !>        in the function space
    !> @return Integer, Number of cells in 2D layer
    procedure, public :: get_ncell

    !> @brief Returns the number of layers in the function space
    !> @return Integer, Number of layers
    procedure, public :: get_nlayers

    !> @brief Returns a pointer to the dofmap for the cell
    !> @param[in] cell Which cell
    !> @return The pointer which points to a slice of the dofmap
    procedure, public :: get_cell_dofmap

    !> @brief Returns a pointer to the dofmap for all cells
    !> @return The pointer which points to the cell-ordered dofmap
    procedure, public :: get_whole_dofmap

    !> @brief Returns a pointer to the fractional levels in a column
    !>        for the function space
    !> @return The pointer which points to the fractional levels array
    procedure, public :: get_levels

    !> @brief Obtains the number of dofs per cell
    !> @return Integer, the number of dofs per cell
    procedure, public :: get_ndf

    !> @brief Obtains the number of interior dofs
    !> @return Integer, the number of dofs associated with the interior of
    !>         each cell
    procedure, public :: get_ndof_interior

    !> @brief Obtains the number of face dofs on each horizontal face
    !> @return Integer, the number of dofs associated with the faces of
    !>         each cell
    procedure, public :: get_ndof_face_h

    !> @brief Obtains the number of face dofs on each vertical face
    !> @return Integer, the number of dofs associated with the faces of
    !>         each cell
    procedure, public :: get_ndof_face_v

    !> Gets the coordinates of the function space
    !> @return A pointer to the two dimensional array of nodal_coords, (xyz,ndf)
    procedure, public :: get_nodes

    !> @brief Returns the enumerated integer for the functions_space which
    !! is this function_space
    !> @return Integer, The enumerated integer for the functions space
    procedure, public :: which

    !> @brief Gets the flag (0) for dofs on bottom and top faces of element
    !> @return A pointer to boundary_dofs(ndf,2) the flag for bottom (:,1)
    !>         and top (:,2) boundaries
    procedure, public :: get_boundary_dofs

    !> @brief Calls an available function at a point
    !> @param[in] function_to_call The function to call
    !> @param[in] df The dof to compute the basis function of
    !> @param[in] xi The (x,y,z) coodinates to evaluate the basis function
    procedure, public :: call_function

    !> @brief Evaluates the basis function at a point
    !> @param[in] df The dof to compute the basis function of
    !> @param[in] xi The (x,y,z) coodinates to evaluate the basis function
    procedure, private :: evaluate_basis

    !> @brief Evaluates the differential of a basis function
    !> @param[in] df The dof to compute the basis function of
    !> @param[in] xi The (x,y,z) coodinates to evaluate the basis function
    procedure, private :: evaluate_diff_basis

    !> @brief Evaluates the basis function for a given quadrature.
    !>@deprecated has been moved to the new evaluater_type and once that is
    !supported, this can be removed
    !> @param[in] ndf integer number of dofs
    !> @param[in] qp_h integer number of quadrature points in the horizontal
    !> @param[in] qp_v integer number of quadrature points in the vertical
    !> @param[in] x_qp real two dimensional array holding the x's horizontal
    !> @param[in] z_qp real two dimensional array holding the x's vertical
    !> @param[out] basis real 3 dimensional array holding the evaluated basis
    !! functions
    procedure, public :: compute_basis_function

    !> @brief Evaluates the differential basis function for a given quadrature
    !>@deprecated has been moved to the new evaluater_type and once that is
    !supported, this can be removed
    !> @param[in] ndf integer number of dofs
    !> @param[in] qp_h integer number of quadrature points in the horizontal
    !> @param[in] qp_v integer number of quadrature points in the vertical
    !> @param[in] x_qp real two dimensional array holding the x's horizontal
    !> @param[in] z_qp real two dimensional array holding the x's vertical
    !> @param[out] dbasis real 3 dimensional array holding the evaluated basis
    !> functions
    procedure, public :: compute_diff_basis_function

    !> @brief Gets the size of the space
    !!(1 is scalar 3 is vector). Returns dim
    !> @return dim The size of the space
    procedure, public :: get_dim_space

    !> @brief Gets the size of the differential space
    !! (1 is scalar 3 is vector). Returns dim
    !> @return dim The size of the differential space
    procedure, public :: get_dim_space_diff

    !> @brief Access the mesh object used to create this function space
    !> @return mesh Mesh object
    procedure, public :: get_mesh

    !> @brief Gets the id of the mesh object for this space
    !> @return mesh_id ID of the mesh object
    procedure, public :: get_mesh_id

    !> @brief Returns the horizontal element order of a function space
    procedure, public :: get_element_order_h

    !> @brief Returns the vertical element order of a function space
    procedure, public :: get_element_order_v

    !> @brief Returns the horizontal order of a function space
    procedure, public :: get_fs_order_h

    !> @brief Returns the vertical order of a function space
    procedure, public :: get_fs_order_v

    !> @brief Returns the number of data values held at each dof
    procedure, public :: get_ndata

    !> Returns if the ordering of data is multi-data quickest
    !> @return True if the data is ordered multi-data quickest
    procedure, public :: is_ndata_first

    !> @brief Gets mapping from degree of freedom to reference element entity.
    !> @return Integer array mapping degree of freedom index to geometric entity
    !> on the reference element.
    procedure, public :: get_entity_dofs

    !> Gets the array that holds the global indices of all dofs
    procedure get_global_dof_id

    !> Gets the array that holds the global indices of all cell dofs
    !> in 2D horizontal domain
    procedure get_global_cell_dof_id_2d

    !> Gets the array that holds the global indices of all edge dofs
    !> in 2D horizontal domain
    procedure get_global_edge_dof_id_2d

    !> Gets the array that holds the global indices of all vertex dofs
    !> in 2D horizontal domain
    procedure get_global_vert_dof_id_2d

    !> Gets the index within the dofmap of the last "owned" dof
    procedure get_last_dof_owned

    !> Gets the index within the dofmap of the last "annexed" dof
    procedure get_last_dof_annexed

    !> Gets the index in the dofmap of the last dof in any depth of halo
    procedure get_last_dof_halo_any

    !> Gets the index in the dofmap of the last dof in the deepest depth of halo
    procedure get_last_dof_halo_deepest

    generic :: get_last_dof_halo => get_last_dof_halo_any, &
                                    get_last_dof_halo_deepest

    !> Returns whether fields on this function space are readonly
    procedure, public :: is_readonly

    !> Returns whether fields on this function space can be written to
    procedure, public :: is_writable

    !> Get the instance of a stencil dofmap for a given id
    procedure, public :: get_stencil_dofmap

    !> Get the instance of a 2D stencil dofmap for a given id
    procedure, public :: get_stencil_2D_dofmap

    ! Mesh colouring wrapper methods
    !> @brief Populates args with colouring info from member mesh.
    !>
    !> @param[out] ncolours  Number of colours used to colour member mesh.
    !> @param[out] ncells_per_colour  Count of cells in each colour.
    !> @param[out] colour_map  Indices of cells in each colour.
    procedure, public :: get_colours

    !> @brief   Returns count of colours used in colouring member mesh.
    !> @return  Number of colours used to colour this mesh.
    procedure, public :: get_ncolours

    !> @brief   Returns the halo depth of the function space
    !> @return  Halo depth
    procedure, public :: get_halo_depth

    procedure, public :: clear

    !> Routine to destroy function_space_type
    final :: function_space_destructor

  end type function_space_type

  interface function_space_type
    module procedure fs_constructor
  end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

  !-----------------------------------------------------------------------------
  ! Returns a pointer to a function space object
  !-----------------------------------------------------------------------------
  !> @brief Stucture-Constructor for function_space_type object.
  !> @details This constructor function returns a pointer to an instantiated
  !>          function space. The pointer is to a function space singleton,
  !>          i.e. the function space is only created on the initial call,
  !>          all other calls just return a pointer to the function space.
  !> @param[in] mesh             The mesh upon which to base this function space
  !> @param[in] element_order_h  The element order for this function space in
  !>                             the horizontal direction, 0 being the lowest
  !>                             element order for function spaces defined for
  !>                             Gungho.
  !>                             @b Note: This is not necessarily the same as
  !>                             the order of the function space
  !> @param[in] element_order_v  The element order for this function space in
  !>                             the vertical direction, 0 being the lowest
  !>                             element order for function spaces defined for
  !>                             Gungho.
  !>                             @b Note: This is not necessarily the same as the
  !>                             order of the function space
  !> @param[in] lfric_fs         The integer number indicating which of the
  !>                             function spaces predefined for lfric to base the
  !>                             instantiated function space on. Recognised
  !>                             integers are assigned to the function spaces
  !>                             "handles" in the fs_handles_mod module.
  !> @param[in] ndata            The number of data values to be held at each dof
  !>                             location
  !> @param[in] ndata_first      Flag to set data to be layer first (false) or
  !!                             ndata first (true)
  !> @return    A pointer to the function space held in this module
  function fs_constructor( mesh_id,                                            &
                           element_order_h,                                    &
                           element_order_v,                                    &
                           lfric_fs,                                           &
                           ndata,                                              &
                           ndata_first )  result(instance)

    implicit none

    integer(i_def),           intent(in) :: mesh_id
    integer(i_def),           intent(in) :: element_order_h
    integer(i_def),           intent(in) :: element_order_v
    integer(i_def),           intent(in) :: lfric_fs
    integer(i_def), optional, intent(in) :: ndata
    logical(l_def), optional, intent(in) :: ndata_first

    type(function_space_type) :: instance

    integer(i_def) :: id

    if ( present(ndata_first) ) then
      instance%ndata_first = ndata_first
    else
      instance%ndata_first = .false.
    end if

    if (present(ndata)) then
      instance%ndata = ndata
    else
      instance%ndata = 1
    end if

    instance%mesh => mesh_collection%get_mesh(mesh_id)
    instance%fs = lfric_fs
    instance%element_order_h = element_order_h
    instance%element_order_v = element_order_v

    id = generate_fs_id(lfric_fs, element_order_h, element_order_v, mesh_id, &
                        instance%ndata, instance%ndata_first)
    call instance%set_id(id)

    if (lfric_fs == W0) then
      instance%fs_order_h = element_order_h + 1
      instance%fs_order_v = element_order_v + 1
    else
      instance%fs_order_h = element_order_h
      instance%fs_order_v = element_order_v
    end if
    call init_function_space(instance)

  end function fs_constructor


  subroutine init_function_space(self)

    implicit none

    type(function_space_type), intent(inout) :: self

    integer(i_def) :: ncells_2d
    integer(i_def) :: ncells_2d_with_ghost

    integer(i_def), allocatable :: dofmap(:,:)

    ncells_2d = self%mesh % get_ncells_2d()
    ncells_2d_with_ghost = self%mesh % get_ncells_2d_with_ghost()

    select case (self%fs)
    case (W0, WTHETA, WCHI)
      self%dim_space = 1  ! Scalar field
      self%dim_space_diff = 3  ! Vector field

    case (W1)
      self%dim_space = 3  ! Vector field
      self%dim_space_diff = 3  ! Vector field

    case (W2, W2broken, W2V, W2H, W2Hbroken)
      self%dim_space = 3  ! Vector field
      self%dim_space_diff = 1  ! Scalar field

    case (W2trace, W2Vtrace, W2Htrace, W3)
      self%dim_space = 1  ! Scalar field
      self%dim_space_diff = 3  ! Vector field

    case default
      call log_event(&
      'Attempt to initialise unknown function space', &
      LOG_LEVEL_ERROR)

    end select

    call ndof_setup ( self%mesh,                                               &
                      self%element_order_h, self%element_order_v,              &
                      self%fs,                                                 &
                      self%ndof_vert,                                          &
                      self%ndof_edge_h, self%ndof_edge_v,                      &
                      self%ndof_face_h, self%ndof_face_v,                      &
                      self%ndof_vol,                                           &
                      self%ndof_cell,                                          &
                      self%ndof_glob,                                          &
                      self%ndof_interior, self%ndof_exterior )

    if (allocated(self%basis_index))    deallocate(self%basis_index)
    if (allocated(self%basis_order))    deallocate(self%basis_order)
    if (allocated(self%basis_vector))   deallocate(self%basis_vector)
    if (allocated(self%basis_x))        deallocate(self%basis_x)
    if (allocated(self%basis_z))        deallocate(self%basis_z)
    if (allocated(self%nodal_coords))   deallocate(self%nodal_coords)
    if (allocated(self%dof_on_vert_boundary)) &
                                        deallocate(self%dof_on_vert_boundary)
    if (allocated(self%entity_dofs))    deallocate(self%entity_dofs)

    allocate(self%basis_index( 3, self%ndof_cell ))
    allocate(self%basis_order( 3, self%ndof_cell ))
    allocate(self%basis_vector( self%dim_space, self%ndof_cell ))

    allocate(self%basis_x( self%element_order_h + 2, 2, self%ndof_cell ))
    allocate(self%basis_z( self%element_order_v + 2, self%ndof_cell ))
    allocate(self%nodal_coords( 3, self%ndof_cell ))
    allocate(self%dof_on_vert_boundary ( self%ndof_cell, 2 ))
    allocate(self%entity_dofs(self%ndof_cell))

    call basis_setup( self%element_order_h,                 &
                      self%element_order_v,                 &
                      self%fs,                              &
                      self%ndof_vert, self%ndof_cell,       &
                      self%mesh%get_reference_element(),    &
                      self%basis_index, self%basis_order,   &
                      self%basis_vector, self%basis_x,      &
                      self%basis_z,                         &
                      self%nodal_coords,                    &
                      self%dof_on_vert_boundary,            &
                      self%entity_dofs)

    ncells_2d_with_ghost = self%mesh % get_ncells_2d_with_ghost()

    allocate(dofmap( self%ndof_cell, 0:ncells_2d_with_ghost ))

    allocate(self%global_dof_id (self%ndof_glob * self%ndata))
    allocate(&
    self%global_cell_dof_id_2d(self%mesh%get_last_edge_cell() * self%ndata))

    allocate(&
    self%global_edge_dof_id_2d(self%mesh%get_num_edges_owned_2d() * self%ndata))

    allocate(&
    self%global_vert_dof_id_2d(self%mesh%get_num_verts_owned_2d() * self%ndata))

    allocate(self%last_dof_halo (0 : self%mesh % get_halo_depth()))

    call dofmap_setup ( self%mesh,                                             &
                        self%fs,                                               &
                        self%element_order_h, self%element_order_v,            &
                        self%ndata,                                            &
                        self%ndata_first,                                      &
                        ncells_2d_with_ghost,                                  &
                        self%ndof_vert,                                        &
                        self%ndof_edge_h, self%ndof_edge_v,                    &
                        self%ndof_face_h, self%ndof_face_v,                    &
                        self%ndof_vol,                                         &
                        self%ndof_cell,                                        &
                        self%last_dof_owned,                                   &
                        self%last_dof_annexed,                                 &
                        self%last_dof_halo,                                    &
                        dofmap,                                                &
                        self%global_dof_id,                                    &
                        self%global_cell_dof_id_2d,                            &
                        self%global_edge_dof_id_2d,                            &
                        self%global_vert_dof_id_2d )

    self%master_dofmap = master_dofmap_type(dofmap)


    ! create the linked list
    self%dofmap_list = linked_list_type()

    ! Set the readonly flag for WCHI. This means routing tables don't need to be
    ! set up for this function space
    if(self%fs == WCHI) then
      self%readonly = .true.
    else
      self%readonly = .false.
    end if

    ! Set up the fractional levels (for diagnostic output) for this fs

    call levels_setup( self%mesh,          &
                       self%get_nlayers(), &
                       self%ndof_cell,     &
                       self%nodal_coords,  &
                       self%fractional_levels )

    if (allocated(dofmap)) deallocate (dofmap)

  end subroutine init_function_space

  !-----------------------------------------------------------------------------
  ! Gets total local unique dofs for this space
  !-----------------------------------------------------------------------------
  integer function get_undf(self)

    implicit none

    class(function_space_type), intent(in) :: self

    get_undf = self%last_dof_halo(ubound(self%last_dof_halo,1))

  end function get_undf

  !-----------------------------------------------------------------------------
  ! Gets the total number of unique degrees of freedom located on
  ! the 3D mesh associated with this function space.
  !-----------------------------------------------------------------------------
  integer function get_ndof_glob(self)

    implicit none

    class(function_space_type), intent(in) :: self

    get_ndof_glob = self%ndof_glob

  end function get_ndof_glob

  !-----------------------------------------------------------------------------
  ! Gets the number of cells for this function space
  !-----------------------------------------------------------------------------
  function get_ncell(self) result(ncells_2d)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ncells_2d

    ncells_2d = self%mesh%get_ncells_2d()

  end function get_ncell

  !-----------------------------------------------------------------------------
  ! Gets the number of layers for this functions space
  !-----------------------------------------------------------------------------
  function get_nlayers(self) result(nlayers)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: nlayers

    nlayers = self%mesh%get_nlayers()

  end function get_nlayers

  !-----------------------------------------------------------------------------
  ! Gets the number of dofs for a single cell
  !-----------------------------------------------------------------------------
  function get_ndf(self) result(ndof_cell)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ndof_cell

    ndof_cell = self%ndof_cell

  end function get_ndf
  !-----------------------------------------------------------------------------
  ! Gets the number of interior dofs for a single cell
  !-----------------------------------------------------------------------------
  function get_ndof_interior(self) result(ndof_interior)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ndof_interior

    ndof_interior = self%ndof_interior

  end function get_ndof_interior

  !-----------------------------------------------------------------------------
  ! Gets the number of horizontal face dofs for a single cell
  !-----------------------------------------------------------------------------
  function get_ndof_face_h(self) result(ndof_face_h)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ndof_face_h

    ndof_face_h = self%ndof_face_h

  end function get_ndof_face_h

  !-----------------------------------------------------------------------------
  ! Gets the number of vertical face dofs for a single cell
  !-----------------------------------------------------------------------------
  function get_ndof_face_v(self) result(ndof_face_v)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ndof_face_v

    ndof_face_v = self%ndof_face_v

  end function get_ndof_face_v

  !-----------------------------------------------------------------------------
  ! Gets the dofmap for a single cell
  !-----------------------------------------------------------------------------
  function get_cell_dofmap(self, cell_lid) result(map)

    implicit none

    class(function_space_type), target, intent(in) :: self
    integer(i_def), intent(in) :: cell_lid
    integer(i_def), pointer :: map(:)

    map => self%master_dofmap%get_master_dofmap(cell_lid)

  end function get_cell_dofmap

  !-----------------------------------------------------------------------------
  ! Gets the dofmap for the whole domain
  !-----------------------------------------------------------------------------
  function get_whole_dofmap(self) result(map)

    implicit none

    class(function_space_type), target, intent(in) :: self
    integer(i_def), pointer :: map(:,:)

    map => self%master_dofmap%get_whole_master_dofmap()

  end function get_whole_dofmap

  !-----------------------------------------------------------------------------
  ! Gets the fractional levels for a column in this function space
  !-----------------------------------------------------------------------------
  function get_levels(self) result(levels)

    implicit none

    class(function_space_type), target, intent(in) :: self
    real(r_def), pointer :: levels(:)

    levels => self%fractional_levels

  end function get_levels

  !-----------------------------------------------------------------------------
  ! Gets the nodal coordinates of the function_space
  !-----------------------------------------------------------------------------
  function get_nodes(self) result(nodal_coords)

    implicit none

    class(function_space_type), target, intent(in) :: self

    real(r_def), pointer :: nodal_coords(:,:)

    nodal_coords => self%nodal_coords

  end function get_nodes

  !-----------------------------------------------------------------------------
  ! Gets a flag for dofs on vertical boundaries
  !-----------------------------------------------------------------------------
  function get_boundary_dofs(self) result(boundary_dofs)

    implicit none

    class(function_space_type), target, intent(in) :: self

    integer(i_def), pointer :: boundary_dofs(:,:)

    boundary_dofs => self%dof_on_vert_boundary(:,:)

  end function get_boundary_dofs

  !-----------------------------------------------------------------------------
  ! Gets enumerated integer for the function space
  !-----------------------------------------------------------------------------
  function which(self) result(fs)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: fs

    fs = self%fs

  end function which

  !-----------------------------------------------------------------------------
  ! Gets the size of the function space
  !-----------------------------------------------------------------------------
  function get_dim_space(self) result(dim)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: dim

    dim = self%dim_space

  end function get_dim_space

  !-----------------------------------------------------------------------------
  ! Gets the size of the diferential function space
  !-----------------------------------------------------------------------------
  function get_dim_space_diff(self) result(dim)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: dim

    dim = self%dim_space_diff

  end function get_dim_space_diff

  !-----------------------------------------------------------------------------
  ! Evaluates one of the listed (function_to_call) functions
  !-----------------------------------------------------------------------------
  function call_function(self, function_to_call, df, xi) result(evaluate)

    implicit none

    class(function_space_type) :: self
    integer(i_def), intent(in) :: function_to_call
    integer(i_def), intent(in) :: df
    real(r_def), intent(in) :: xi(3)
    real(r_def), allocatable :: evaluate(:)

    select case (function_to_call)
    case(BASIS)
      allocate(evaluate(self%dim_space))
      evaluate = evaluate_basis(self, df, xi)

    case(DIFF_BASIS)
      allocate(evaluate(self%dim_space_diff))
      evaluate = evaluate_diff_basis(self, df, xi)

    case default
      call log_event(&
      'function_to_call does not match the available enumerators', &
      LOG_LEVEL_ERROR)

    end select

  end function call_function

  !-----------------------------------------------------------------------------
  ! Evaluates a basis function at a point
  !-----------------------------------------------------------------------------
  function evaluate_basis(self, df, xi) result(p)

    use polynomial_mod, only : poly1d

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(in) :: df
    real(r_def), intent(in) :: xi(3)
    real(r_def) :: p(self%dim_space)

    p(:) = poly1d(self%basis_order(1, df), xi(1), self%basis_x(:, 1, df), self%basis_index(1, df)) &
         * poly1d(self%basis_order(2, df), xi(2), self%basis_x(:, 2, df), self%basis_index(2, df)) &
         * poly1d(self%basis_order(3, df), xi(3), self%basis_z(:, df),    self%basis_index(3, df)) &
         * self%basis_vector(:, df)

  end function evaluate_basis

  !-----------------------------------------------------------------------------
  ! Evaluates the differential of a basis function at a point
  !-----------------------------------------------------------------------------
  pure function evaluate_diff_basis(self, df, xi) result(evaluate)

    use polynomial_mod, only : poly1d, poly1d_deriv

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(in)  :: df
    real(r_def),    intent(in)  :: xi(3)
    real(r_def)                 :: evaluate(self%dim_space_diff)
    real(r_def)                 :: dpdx(3)

    dpdx(1) = poly1d_deriv(self%basis_order(1, df), xi(1), self%basis_x(:, 1, df), self%basis_index(1, df)) &
            * poly1d      (self%basis_order(2, df), xi(2), self%basis_x(:, 2, df), self%basis_index(2, df)) &
            * poly1d      (self%basis_order(3, df), xi(3), self%basis_z(:, df), self%basis_index(3, df))

    dpdx(2) = poly1d      (self%basis_order(1, df), xi(1), self%basis_x(:, 1, df), self%basis_index(1, df)) &
            * poly1d_deriv(self%basis_order(2, df), xi(2), self%basis_x(:, 2, df), self%basis_index(2, df)) &
            * poly1d      (self%basis_order(3, df), xi(3), self%basis_z(:, df), self%basis_index(3, df))

    dpdx(3) = poly1d      (self%basis_order(1, df), xi(1), self%basis_x(:, 1, df), self%basis_index(1, df)) &
            * poly1d      (self%basis_order(2, df), xi(2), self%basis_x(:, 2, df), self%basis_index(2, df)) &
            * poly1d_deriv(self%basis_order(3, df), xi(3), self%basis_z(:, df), self%basis_index(3, df))

    if (self%dim_space == 1 .and. self%dim_space_diff == 3) then
      ! grad(p)
      evaluate(1) = dpdx(1)
      evaluate(2) = dpdx(2)
      evaluate(3) = dpdx(3)
    else if (self%dim_space == 3 .and. self%dim_space_diff == 3) then
      ! curl(p)
      evaluate(1) = dpdx(2) * self%basis_vector(3, df) &
                  - dpdx(3) * self%basis_vector(2, df)

      evaluate(2) = dpdx(3) * self%basis_vector(1, df) &
                  - dpdx(1) * self%basis_vector(3, df)

      evaluate(3) = dpdx(1) * self%basis_vector(2, df) &
                  - dpdx(2) * self%basis_vector(1, df)

    else if (self%dim_space == 3 .and. self%dim_space_diff == 1) then
      ! div(p)
      evaluate(1) = dpdx(1) * self%basis_vector(1, df)  &
                  + dpdx(2) * self%basis_vector(2, df)  &
                  + dpdx(3) * self%basis_vector(3, df)

    else if (self%dim_space == 1 .and. self%dim_space_diff == 1) then
      ! dp/dz
      evaluate(1) = dpdx(3)
    else
      evaluate(:) = 0.0_r_def
    end if

  end function evaluate_diff_basis

  !-----------------------------------------------------------------------------
  ! Evaluates the basis function for a given quadrature
  !-----------------------------------------------------------------------------
  subroutine compute_basis_function(self, basis, ndf, qp_h, qp_v, x_qp, z_qp)

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(in) :: ndf
    integer(i_def), intent(in) :: qp_h
    integer(i_def), intent(in) :: qp_v

    real(r_def), intent(in) :: x_qp (qp_h, 2)
    real(r_def), intent(in) :: z_qp (qp_v)

    real(r_def), intent(out) :: basis( self%dim_space, ndf, qp_h, qp_v )

    ! Local variables - loop counters
    integer(i_def) :: df
    integer(i_def) :: qp1
    integer(i_def) :: qp2
    real(r_def) :: xyz(3)

    do qp2 = 1, qp_v
      xyz(3) = z_qp(qp2)
      do qp1 = 1, qp_h
        xyz(1) = x_qp( qp1, 1 )
        xyz(2) = x_qp( qp1, 2 )
        do df = 1, ndf
          basis(:, df, qp1, qp2) = self%evaluate_basis( df, xyz )
        end do
      end do
    end do

  end subroutine compute_basis_function

  !-----------------------------------------------------------------------------
  ! Evaluates the differential basis function for a given quadrature
  !-----------------------------------------------------------------------------
  subroutine compute_diff_basis_function( self,   &
                                          dbasis, &
                                          ndf,    &
                                          qp_h,   &
                                          qp_v,   &
                                          x_qp,   &
                                          z_qp )

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(in) :: ndf
    integer(i_def), intent(in) :: qp_h
    integer(i_def), intent(in) :: qp_v

    real(r_def), intent(in) :: x_qp(qp_h, 2)
    real(r_def), intent(in) :: z_qp(qp_v)

    real(r_def), intent(out) :: dbasis( self%dim_space_diff, ndf, qp_h, qp_v )

    ! local variables - loop counters
    integer(i_def) :: df
    integer(i_def) :: qp1
    integer(i_def) :: qp2
    real(r_def) :: xyz(3)

    do qp2 = 1, qp_v
      xyz(3) = z_qp(qp2)
      do qp1 = 1, qp_h
        xyz(1) = x_qp( qp1, 1 )
        xyz(2) = x_qp( qp1, 2 )
        do df = 1, ndf
          dbasis( :, df, qp1, qp2 ) = self%evaluate_diff_basis( df, xyz )
        end do
      end do
    end do

  end subroutine compute_diff_basis_function

  !-----------------------------------------------------------------------------
  ! Gets order for this space in the horizontal direction
  !-----------------------------------------------------------------------------
  !> @brief Gets the polynomial order for this space, returns an integer
  !> @param[in] self the calling function space
  !-----------------------------------------------------------------------------
  function get_element_order_h(self) result (element_order_h)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: element_order_h

    element_order_h = self%element_order_h

  end function get_element_order_h

  !-----------------------------------------------------------------------------
  ! Gets order for this space in the vertical direction
  !-----------------------------------------------------------------------------
  !> @brief Gets the polynomial order for this space, returns an integer
  !> @param[in] self the calling function space
  !-----------------------------------------------------------------------------
  function get_element_order_v(self) result (element_order_v)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: element_order_v

    element_order_v = self%element_order_v

  end function get_element_order_v

  !-----------------------------------------------------------------------------
  !> @details Gets the order for this function space in the horizontal direction
  !> @return  The order of the function space
  function get_fs_order_h(self) result (fs_order_h)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: fs_order_h

    fs_order_h = self%fs_order_h

  end function get_fs_order_h

  !-----------------------------------------------------------------------------
  !> @details Gets the order for this function space in the vertical direction
  !> @return  The order of the function space
  function get_fs_order_v(self) result (fs_order_v)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: fs_order_v

    fs_order_v = self%fs_order_v

  end function get_fs_order_v

  !-----------------------------------------------------------------------------
  !> @details Gets the number of data values held at each dof
  !> @return  The number of data values held at each dof
  function get_ndata(self) result (ndata)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ndata

    ndata = self%ndata

  end function get_ndata

  !> Returns whether the field data is ordered multi-data first
  !>
  !> @return Flag for if field data order is multi-data first
  function is_ndata_first(self) result(flag)

    implicit none

    class(function_space_type), intent(in) :: self
    logical(l_def) :: flag

    flag = self%ndata_first

  end function is_ndata_first

  !-----------------------------------------------------------------------------
  ! Gets the mapping from degrees of freedom to reference
  ! element entity.
  !-----------------------------------------------------------------------------
  function get_entity_dofs(self) result (entity_dofs)

    implicit none

    class(function_space_type), target, intent(in) :: self
    integer(i_def), pointer :: entity_dofs(:)

    entity_dofs => self%entity_dofs(:)

  end function get_entity_dofs

  !-----------------------------------------------------------------------------
  ! Gets mesh object for this space
  !-----------------------------------------------------------------------------
  !> @brief Gets the mesh object for this space
  !> @param[in] self the calling function space
  !> @return mesh Mesh Object
  !-----------------------------------------------------------------------------
  function get_mesh(self) result (mesh)

    implicit none

    class(function_space_type), intent(in) :: self
    type(mesh_type), pointer :: mesh

    if ( associated (self%mesh) ) then
      mesh => self%mesh
    else
      call log_event('Function space has null pointer to mesh!!!', log_level_error)
    end if

  end function get_mesh

  !-----------------------------------------------------------------------------
  ! Get id of mesh object for this space
  !-----------------------------------------------------------------------------
  !> @brief Gets the id of the mesh object for this space
  !> @param[in] self the calling function space
  !> @return mesh_id
  !-----------------------------------------------------------------------------
  function get_mesh_id(self) result (mesh_id)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: mesh_id

    mesh_id = self%mesh%get_id()

  end function get_mesh_id

  !-----------------------------------------------------------------------------
  ! Gets the array that holds the global indices of all dofs
  !-----------------------------------------------------------------------------
  function get_global_dof_id(self) result(global_dof_id)

    implicit none

    class(function_space_type), target, intent(in) :: self

    integer(i_halo_index), pointer :: global_dof_id(:)

    global_dof_id => self%global_dof_id(:)

  end function get_global_dof_id

  !-----------------------------------------------------------------------------
  ! Gets the array that holds the global indices of cell dofs in 2D
  ! Horizontal domain
  !-----------------------------------------------------------------------------
  subroutine get_global_cell_dof_id_2d(self, global_cell_dof_id_2d)

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(out) :: global_cell_dof_id_2d(:)

    global_cell_dof_id_2d(:) = self%global_cell_dof_id_2d(:)

  end subroutine get_global_cell_dof_id_2d

  !-----------------------------------------------------------------------------
  ! Gets the array that holds the global indices of edge dofs in 2D
  ! Horizontal domain
  !-----------------------------------------------------------------------------
  subroutine get_global_edge_dof_id_2d(self, global_edge_dof_id_2d)

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(out) :: global_edge_dof_id_2d(:)

    global_edge_dof_id_2d(:) = self%global_edge_dof_id_2d(:)

  end subroutine get_global_edge_dof_id_2d

  !-----------------------------------------------------------------------------
  ! Gets the array that holds the global indices of vertex dofs in 2D
  ! Horizontal domain
  !-----------------------------------------------------------------------------
  subroutine get_global_vert_dof_id_2d(self, global_vert_dof_id_2d)

    implicit none

    class(function_space_type), intent(in) :: self

    integer(i_def), intent(out) :: global_vert_dof_id_2d(:)

    global_vert_dof_id_2d(:) = self%global_vert_dof_id_2d(:)

  end subroutine get_global_vert_dof_id_2d

  !-----------------------------------------------------------------------------
  ! Gets the index within the dofmap of the last "owned" dof
  !-----------------------------------------------------------------------------
  function get_last_dof_owned(self) result (last_dof_owned)

    implicit none

    class(function_space_type) :: self

    integer(i_def) :: last_dof_owned

    last_dof_owned = self%last_dof_owned

  end function get_last_dof_owned

  !-----------------------------------------------------------------------------
  ! Gets the index within the dofmap of the last "annexed" dof
  !-----------------------------------------------------------------------------
  function get_last_dof_annexed(self) result (last_dof_annexed)

    implicit none

    class(function_space_type) :: self

    integer(i_def) :: last_dof_annexed

    last_dof_annexed = self%last_dof_annexed

  end function get_last_dof_annexed

  !-----------------------------------------------------------------------------
  ! Gets the index within the dofmap of the last dof in the specified halo
  !-----------------------------------------------------------------------------
  function get_last_dof_halo_any(self, depth) result (last_dof_halo)

    implicit none

    class(function_space_type) :: self
    integer(i_def), intent(in) :: depth

    integer(i_def) :: last_dof_halo

    last_dof_halo = self%last_dof_halo(depth)

  end function get_last_dof_halo_any

  !-----------------------------------------------------------------------------
  ! Gets the index within the dofmap of the last dof in the deepest halo
  !-----------------------------------------------------------------------------
  function get_last_dof_halo_deepest(self) result (last_dof_halo)

    implicit none

    class(function_space_type) :: self

    integer(i_def) :: last_dof_halo

    last_dof_halo = self%last_dof_halo(ubound(self%last_dof_halo,1))

  end function get_last_dof_halo_deepest


  !> @brief Returns whether fields on this function space are readonly
  !> @return return_readonly Flag describes if fields on this function space
  !>                         will be readonly
  function is_readonly(self) result(return_readonly)

    implicit none

    class(function_space_type), intent(in) :: self
    logical(l_def) :: return_readonly

    return_readonly = self%readonly

  end function is_readonly


  !> @brief Returns whether fields on this function space can be written to
  !> @return return_writable Flag describes if fields on this function space
  !>                         can be written to
  function is_writable(self) result(return_writable)

    implicit none

    class(function_space_type), intent(in) :: self
    logical(l_def) :: return_writable

    return_writable = .not.self%readonly

  end function is_writable

  !> @brief Get the instance of a stencil dofmap for a given shape and size
  !> @param[in] stencil_shape The shape identifier for the stencil dofmap to create
  !> @param[in] stencil_extent The extent of the stencil excluding the centre cell
  !> @return map the stencil_dofmap object to return
  function get_stencil_dofmap(self, stencil_shape, stencil_extent) result(map)

    use stencil_dofmap_mod, only : stencil_dofmap_type

    implicit none

    class(function_space_type), intent(inout) :: self
    integer(i_def), intent(in) :: stencil_shape
    integer(i_def), intent(in) :: stencil_extent

    type(stencil_dofmap_type), pointer :: map ! return value
    type(linked_list_item_type), pointer :: loop => null()

    integer(i_def) :: id

    map => null()

    ! Calculate id of the stencil_dofmap we want
    id = generate_stencil_dofmap_id(stencil_shape, stencil_extent)


    ! point at the head of the stencil_dofmap linked list
    loop => self%dofmap_list%get_head()

    ! loop through list
    do
      if (.not. associated(loop)) then
        ! At the end of list and we didn't find it
        ! create stencil dofmap and add it

        call self%dofmap_list%insert_item(stencil_dofmap_type(stencil_shape,   &
                                                            stencil_extent,    &
                                                            self%ndof_cell,    &
                                                            self%mesh,         &
                                                            self%master_dofmap))


        ! At this point the desired stencil dofmap is the tail of the list
        ! so just retrieve it and exit loop

        loop => self%dofmap_list%get_tail()

        ! 'cast' to the stencil_dofmap_type
        select type(v => loop%payload)
        type is (stencil_dofmap_type)
          map => v
        end select
        exit

      end if
      ! otherwise search list for the id we want
      if (id == loop%payload%get_id()) then
        ! 'cast' to the stencil_dofmap_type
        select type(v => loop%payload)
        type is (stencil_dofmap_type)
          map => v
        end select
        exit
      end if
      loop => loop%next
    end do

  end function get_stencil_dofmap

  !> @brief Get the instance of a 2D stencil dofmap for a given shape and size
  !> @param[in] stencil_shape The shape identifier for the stencil dofmap to create
  !> @param[in] stencil_extent The extent of the stencil excluding the centre cell
  !> @return map The stencil_dofmap object to return
  function get_stencil_2D_dofmap(self, stencil_shape, stencil_extent) result(map)

    use stencil_2D_dofmap_mod, only : stencil_2D_dofmap_type

    implicit none

    class(function_space_type), intent(inout) :: self
    integer(i_def), intent(in) :: stencil_shape
    integer(i_def), intent(in) :: stencil_extent
    type(stencil_2D_dofmap_type), pointer :: map ! return value

    type(linked_list_item_type), pointer :: loop => null()

    integer(i_def) :: id

    map => null()

    ! Calculate id of the stencil_dofmap we want
    id = generate_stencil_dofmap_id(stencil_shape, stencil_extent)

    ! point at the head of the stencil_dofmap linked list
    loop => self%dofmap_list%get_head()

    ! loop through list
    do
      if (.not. associated(loop)) then
        ! At the end of list and we didn't find it
        ! create stencil dofmap and add it

        call self%dofmap_list%insert_item(stencil_2D_dofmap_type(             &
                                                            stencil_shape,    &
                                                            stencil_extent,   &
                                                            self%ndof_cell,   &
                                                            self%mesh,        &
                                                            self%master_dofmap))


        ! At this point the desired stencil dofmap is the tail of the list
        ! so just retrieve it and exit loop

        loop => self%dofmap_list%get_tail()

        ! 'cast' to the stencil_dofmap_type
        select type(v => loop%payload)
        type is (stencil_2D_dofmap_type)
          map => v
        end select
        exit

      end if
      ! otherwise search list for the id we want
      if (id == loop%payload%get_id()) then
        ! 'cast' to the stencil_dofmap_type
        select type(v => loop%payload)
        type is (stencil_2D_dofmap_type)
          map => v
        end select
        exit
      end if
      loop => loop%next
    end do

  end function get_stencil_2D_dofmap

  !-----------------------------------------------------------------------------
  !> @brief   Returns count of colours used in colouring member mesh.
  !>
  !> @return  Number of colours used to colour this mesh.
  !-----------------------------------------------------------------------------
  function get_ncolours(self) result(ncolours)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: ncolours

    ncolours = self%mesh%get_ncolours()

  end function get_ncolours

  !=============================================================================
  !> @brief Populates args with colouring info from member mesh.
  !>
  !> @param[out] ncolours  Number of colours used to colour member mesh.
  !> @param[out] ncells_per_colour  Count of cells in each colour.
  !> @param[out] colour_map  Indices of cells in each colour.
  !=============================================================================
  subroutine get_colours(self, ncolours, ncells_per_colour, colour_map)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def), intent(out) :: ncolours
    integer(i_def), pointer, intent(out) :: ncells_per_colour(:)
    integer(i_def), pointer, intent(out) :: colour_map(:,:)

    call self%mesh%get_colours(ncolours, ncells_per_colour, colour_map)

  end subroutine get_colours

  !-----------------------------------------------------------------------------
  !> @brief   Returns the halo depth of the function space
  !>
  !> @return  Depth of the halo
  !-----------------------------------------------------------------------------
  function get_halo_depth(self) result(halo_depth)

    implicit none

    class(function_space_type), intent(in) :: self
    integer(i_def) :: halo_depth

    halo_depth = self%mesh%get_halo_depth()

  end function get_halo_depth

  !-----------------------------------------------------------------------------
  !  Function to clear up objects - called by destructor
  !-----------------------------------------------------------------------------
  !> @details Explcitly deallocates any allocatable arrays in the function space
  !>          to avoid memory leaks
  !> @return  Error status variable
  subroutine clear(self)

    implicit none

    class (function_space_type), intent(inout) :: self

    if (allocated(self%entity_dofs))      deallocate(self%entity_dofs)
    if (allocated(self%nodal_coords))     deallocate(self%nodal_coords)
    if (allocated(self%basis_order))      deallocate(self%basis_order)
    if (allocated(self%basis_index))      deallocate(self%basis_index)
    if (allocated(self%basis_vector))     deallocate(self%basis_vector)
    if (allocated(self%basis_x))          deallocate(self%basis_x)
    if (allocated(self%basis_z))          deallocate(self%basis_z)
    if (allocated(self%global_dof_id))    deallocate(self%global_dof_id)
    if (allocated(self%global_cell_dof_id_2d))  &
                                          deallocate(self%global_cell_dof_id_2d)
    if (allocated(self%global_edge_dof_id_2d))  &
                                          deallocate(self%global_edge_dof_id_2d)
    if (allocated(self%global_vert_dof_id_2d))  &
                                          deallocate(self%global_vert_dof_id_2d)
    if (allocated(self%last_dof_halo))    deallocate(self%last_dof_halo)
    if (allocated(self%fractional_levels))deallocate(self%fractional_levels)
    if (allocated(self%dof_on_vert_boundary))   &
                                          deallocate(self%dof_on_vert_boundary)

    call self%master_dofmap%clear()
    call self%dofmap_list%clear()

    nullify(self%mesh)

  end subroutine clear

  !-----------------------------------------------------------------------------
  ! Function space destructor
  !-----------------------------------------------------------------------------

  subroutine function_space_destructor(self)

    implicit none

    type (function_space_type), intent(inout) :: self

    call self%clear()

  end subroutine function_space_destructor

end module function_space_mod
