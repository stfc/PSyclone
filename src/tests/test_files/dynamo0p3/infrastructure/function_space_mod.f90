!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!>
!> @brief Holds information about the function space.
!>
!> @details A container which holds type definition of the function space and
!>          has holds a number of static copies of the function spaces require
!>          by the model. It provides accessor functions (getters) to various
!>          information held in the type.
!
module function_space_mod


use constants_mod,         only: r_def, i_def
use mesh_mod,              only: mesh_type
!use master_dofmap_mod,     only: master_dofmap_type
use stencil_dofmap_mod,    only: stencil_dofmap_type, STENCIL_POINT
!!$use ESMF,                  only: ESMF_RouteHandle, ESMF_DistGrid, ESMF_Array   &
!!$                               , ESMF_Success, ESMF_DistGridCreate             &
!!$                               , ESMF_ArrayCreate, ESMF_ArrayHaloStore         &
!!$                               , ESMF_ArrayDestroy, ESMF_TYPEKIND_R8
!!$use log_mod,               only: log_event, log_scratch_space                  &
!!$                               , LOG_LEVEL_DEBUG, LOG_LEVEL_ERROR              &
!!$                               , LOG_LEVEL_INFO
!!$use reference_element_mod, only: tangent_to_edge, normal_to_face               &
!!$                               , nfaces_h, nedges_h, nverts_h                  &
!!$                               , nfaces, nverts, nedges, x_vert                &
!!$                               , edge_on_face, select_entity_type              &
!!$                               , select_entity_all, select_entity_theta        &
!!$                               , select_entity_w2h, select_entity_w2v
!!$
!!$use fs_continuity_mod,     only: W0, W1, W2, W3, Wtheta, W2V, W2H, Wchi
!!$use function_space_constructor_helper_functions_mod, &
!!$                           only: ndof_setup, basis_setup, dofmap_setup
!!$
!!$use evaluate_function_mod, only : evaluate_function_type, BASIS, DIFF_BASIS
!!$use linked_list_mod,       only : linked_list_type, &
!!$                                  linked_list_item_type
!!$use mesh_collection_mod,   only : mesh_collection

implicit none

private
!public :: W0, W1, W2, W3, Wtheta, W2V, W2H, Wchi

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

!type, extends(evaluate_function_type), public :: function_space_type
type, public :: function_space_type

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

  !> Number of degrees of freedom located on cell edge entities.
  integer(i_def) :: ndof_edge

  !> Number of degrees of freedom located on cell face entities.
  integer(i_def) :: ndof_face

  !> Number of degrees of freedom located on cell volume entities.
  integer(i_def) :: ndof_vol

  !> Integer value for dynamo functions spaces, e.g. W0 would be 1
  integer(i_def) :: fs

  !> Element base-order of dynamo function space
  integer(i_def) :: element_order

  ! Function space polynomial order? dynamics is still to provide us
  ! with a name for this, same as element order except for W0 
  ! where is it equal to element_order+1
  integer(i_def) :: fs_order

  !> Number of dimensions in this function space
  integer(i_def) :: dim_space

  !> Number of dimensions in this function space when differentiated
  integer(i_def) :: dim_space_diff

  !> A two dimensional, allocatable array which holds the indirection map
  !> or dofmap for the whole function space over the bottom level of the domain.
  !type(master_dofmap_type) :: master_dofmap

  !> Mesh object used to create this function space. This is a
  !> pointer to a mesh in a linked list of mesh objects
  type(mesh_type), pointer :: mesh => null()

  !> A two dimensional, allocatable array of reals which holds the coordinates
  !> of the function_space degrees of freedom
  real(r_def),    allocatable :: nodal_coords(:,:)

  !> A two dimensional, allocatable, integer array which specifies which 
  !> dofs are on vertex boundarys
  integer(i_def), allocatable :: dof_on_vert_boundary(:,:)

  !> @}
  !> @name Arrays needed for on the fly basis evaluations
  integer(i_def), allocatable :: basis_order(:,:)
  integer(i_def), allocatable :: basis_index(:,:)
  real(r_def),    allocatable :: basis_vector(:,:)
  real(r_def),    allocatable :: basis_x(:,:,:)
  !> @}

  !> A one dimensional, allocatable array which holds a unique global index for
  !> every dof in the local domain
  integer(i_def), allocatable :: global_dof_id(:)

  !> The index within the dofmap of the last "owned" dof
  integer(i_def) :: last_dof_owned

  !> The index within the dofmap of the last "annexed" dof
  !> ("Annexed" dofs that those that are not owned, but are on owned cells)
  integer(i_def) :: last_dof_annexed

  !> A one dimensional, allocatable array which holds the index in the dofmap
  !> of the last of the halo dofs (from the various depths of halo)
  integer(i_def), allocatable :: last_dof_halo(:)

  !> A linked list of stencil dofmaps
  !type(linked_list_type)      :: dofmap_list

  !> A list of the routing tables needed to perform halo swaps to various depths
  !! of halo
  !type(ESMF_RouteHandle), allocatable :: haloHandle(:)

  !> An ESMF distributed grid description
  !type(ESMF_DistGrid) :: distgrid

contains

  !> @brief Gets the total number of unique degrees of freedom for this space,
  !> @return Integer Total number of unique degrees of freedom
  procedure, public :: get_undf

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

  !> @brief Obtains the number of dofs per cell
  !> @return Integer, the number of dofs per cell
  procedure, public :: get_ndf

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
  !> @param[in] func_to_call The function to call
  !> @param[in] df The dof to compute the basis function of
  !> @param[in] xi The (x,y,z) coodinates to evaluate the basis function
  procedure, public :: evaluate_function

  !> @brief Evaluates the basis function at a point
  !> @param[in] df The dof to compute the basis function of
  !> @param[in] xi The (x,y,z) coodinates to evaluate the basis function
  !> @TODO once the new evaluator is implemented via evaluate_function then
  !>       this function could be made private as its accessed from
  !>       evaluate_function
  procedure, public :: evaluate_basis

  !> @brief Evaluates the differential of a basis function
  !> @param[in] df The dof to compute the basis function of
  !> @param[in] xi The (x,y,z) coodinates to evaluate the basis function
  !> @TODO once the new evaluator is implemented via evaluate_function then
  !>       this function could be made private as its accessed from
  !>       evaluate_function
  procedure, public :: evaluate_diff_basis

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

  !> Subroutine to evaluate the basis function at a set of nodes
  !> @param[out] basis real 3 dimensional array holding the evaluated basis 
  !! functions
  !> @param[in] ndf integer number of dofs
  !> @param[in] n_node integer number of nodal points
  !> @param[in] x_node real three dimensional array holding the nodal
  !>            coordinates
  !>@deprecated has been moved to the new evaluater_type and once that is
  !supported, this can be removed (ticket #723)
  procedure, public :: compute_nodal_basis_function

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

  !> Subroutine to evaluate the differential basis function at a set of nodes
  !> @param[out] dbasis real 3 dimensional array holding the evaluated
  !!             differential basis functions
  !> @param[in] ndf integer number of dofs
  !> @param[in] n_node integer number of nodal points
  !> @param[in] x_node real three dimensional array holding the nodal
  !>            coordinates
  !>@deprecated has been moved to the new evaluater_type and once that is
  !supported, this can be removed (#723)
  procedure, public :: compute_nodal_diff_basis_function

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
  procedure, public :: get_mesh_id

  !> @brief Returns the element order of a function space
  procedure, public :: get_element_order

  !> @brief Returns the order of a function space
  procedure, public :: get_fs_order

  !> @brief Gets a routing table for halo swapping
  !> @param[in] depth The depth of halo swap to perform  
  !> @return haloHandle The routing table for swapping halos to the depth required
  !procedure get_haloHandle

  !> @brief Gets the ESMF distributed grid description for the function space
  !> @return distgrid The ESMF distributed grid description
!  procedure get_distgrid

  !> Gets the array that holds the global indices of all dofs
!  procedure get_global_dof_id

  !> Gets the index within the dofmap of the last "owned" dof
  procedure get_last_dof_owned

  !> Gets the index in the dofmap of the last dof in the deepest depth of halo
  procedure get_last_dof_halo

  !> Get the instance of a stencil dofmap with for a given id
  procedure, public   :: get_stencil_dofmap

  ! Mesh colouring wrapper methods
  !> @brief Populates args with colouring info from member mesh.
  !>
  !> @param[out] ncolours  Number of colours used to colour member mesh. 
  !> @param[out] ncells_per_colour  Count of cells in each colour.
  !> @param[out] colour_map  Indices of cells in each colour.
  procedure, public  :: get_colours

  !> @brief   Returns count of colours used in colouring member mesh.
  !> @return  Number of colours used to colour this mesh. 
  procedure, public  :: get_ncolours

  !> @brief  Invoke calculation of colouring for the member mesh.
  procedure, public  :: set_colours

!  procedure, public  :: clear

  !> Routine to destroy function_space_type
  !final              :: function_space_destructor

end type function_space_type
!!$
!!$interface function_space_type
!!$  module procedure fs_constructor
!!$end interface


!-------------------------------------------------------------------------------
! Contained functions/subroutines 
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
! Returns a pointer to a function space object
!-------------------------------------------------------------------------------
!> @brief Stucture-Constructor for function_space_type object.
!> @details This constructor function returns a pointer to an instantiated
!>          function space. The pointer is to a function space singleton,
!>          i.e. the function space is only created on the initial call,
!>          all other calls just return a pointer to the function space.
!> @param[in] mesh           The mesh upon which to base this function space
!> @param[in] element_order  The element order for this function space, 0 being 
!>                           the lowest element order for function spaces defined
!>                           for dynamo.
!>                           @b Note: This is not necessarily the same as the
!>                           order of the function space
!> @param[in] dynamo_fs      The integer number indicating which of the function
!>                           spaces predefined for dynamo to base the
!>                           instantiated function space on. Recognised integers
!>                           are assigned to the function spaces "handles" in the
!>                           fs_handles_mod module.
!> @return    A pointer to the function space held in this module
!!$function fs_constructor(mesh_id, element_order, dynamo_fs) result(instance)
!!$
!!$  implicit none
!!$
!!$  integer(i_def), intent(in) :: mesh_id
!!$  integer(i_def), intent(in) :: element_order
!!$  integer(i_def), intent(in) :: dynamo_fs
!!$
!!$  type(function_space_type), pointer :: instance
!!$
!!$  allocate(instance)
!!$
!!$  instance%mesh => mesh_collection%get_mesh( mesh_id )
!!$
!!$  ! Set the id in the base class
!!$  call instance%set_id(1000000*mesh_id + (1000*element_order) + dynamo_fs)
!!$
!!$  instance%fs = dynamo_fs
!!$
!!$  instance%element_order =  element_order
!!$
!!$  if (dynamo_fs == W0) then
!!$    instance%fs_order = element_order + 1
!!$  else
!!$    instance%fs_order = element_order
!!$  end if
!!$  call init_function_space( instance )
!!$
!!$  return
!!$end function fs_constructor


!!$subroutine init_function_space( self )
!!$
!!$  implicit none
!!$
!!$  class(function_space_type) :: self
!!$
!!$  integer(i_def) :: ncells_2d
!!$  integer(i_def) :: ncells_2d_with_ghost
!!$
!!$
!!$  integer(i_def) :: st_shape   ! Stencil Shape
!!$  integer(i_def) :: st_size    ! Stencil Extent
!!$
!!$  integer(i_def) :: rc
!!$  integer(i_def) :: idepth
!!$  integer(i_def) :: halo_start, halo_finish
!!$
!!$  integer(i_def), allocatable :: dofmap(:,:)
!!$
!!$  type(ESMF_Array) :: temporary_esmf_array
!!$
!!$  ncells_2d            = self%mesh % get_ncells_2d()
!!$  ncells_2d_with_ghost = self%mesh % get_ncells_2d_with_ghost()
!!$
!!$  st_size  = 1
!!$  st_shape = STENCIL_POINT
!!$
!!$
!!$  select case (self%fs)
!!$  case (W0,WTHETA,WCHI)
!!$    self%dim_space      = 1  ! Scalar field
!!$    self%dim_space_diff = 3  ! Vector field
!!$
!!$  case (W1)
!!$    self%dim_space      = 3  ! Vector field
!!$    self%dim_space_diff = 3  ! Vector field
!!$
!!$  case (W2,W2V,W2H)
!!$    self%dim_space      = 3  ! Vector field
!!$    self%dim_space_diff = 1  ! Scalar field
!!$
!!$  case (W3)
!!$    self%dim_space      = 1  ! Scalar field
!!$    self%dim_space_diff = 3  ! Vector field
!!$
!!$  end select
!!$
!!$  call ndof_setup ( self%mesh, self%element_order, self%fs   &
!!$                  , self%ndof_vert, self%ndof_edge, self%ndof_face               &
!!$                  , self%ndof_vol,  self%ndof_cell, self%ndof_glob               &
!!$                  , self%ndof_interior, self%ndof_exterior )
!!$
!!$
!!$  allocate( self%basis_index  (                     3, self%ndof_cell) )
!!$  allocate( self%basis_order  (                     3, self%ndof_cell) )
!!$  allocate( self%basis_vector (self%dim_space,         self%ndof_cell) )
!!$  allocate( self%basis_x      (self%element_order+2,3, self%ndof_cell) )
!!$  allocate( self%nodal_coords (                     3, self%ndof_cell) )
!!$  allocate( self%dof_on_vert_boundary (self%ndof_cell,2) )
!!$
!!$  call basis_setup ( self%element_order, self%fs, self%ndof_vert  &
!!$                   , self%ndof_cell, self%basis_index,  self%basis_order              &
!!$                   , self%basis_vector, self%basis_x                                  &
!!$                   , self%nodal_coords, self%dof_on_vert_boundary )
!!$
!!$  ncells_2d_with_ghost = self%mesh % get_ncells_2d_with_ghost()
!!$
!!$  allocate( dofmap             ( self%ndof_cell                                &
!!$                               , 0:ncells_2d_with_ghost ) )
!!$  allocate( self%global_dof_id ( self%ndof_glob) )
!!$
!!$  allocate( self%last_dof_halo ( self%mesh % get_halo_depth()) )
!!$
!!$  call dofmap_setup ( self%mesh, self%fs, ncells_2d_with_ghost                 &
!!$                    , self%ndof_vert, self%ndof_edge, self%ndof_face           &
!!$                    , self%ndof_vol,  self%ndof_cell, self%last_dof_owned      &
!!$                    , self%last_dof_annexed, self%last_dof_halo, dofmap        &
!!$                    , self%global_dof_id )
!!$
!!$
!!$  self%master_dofmap = master_dofmap_type( dofmap )
!!$
!!$
!!$  ! create the linked list
!!$  self%dofmap_list = linked_list_type()
!!$
!!$  ! Create the stencil_dofmap object and add to linked list
!!$
!!$  call self%dofmap_list%insert_item( stencil_dofmap_type(st_shape,           &
!!$                                                         st_size,            &
!!$                                                         self%ndof_cell,     &
!!$                                                         self%mesh,          &
!!$                                                         self%master_dofmap) )
!!$
!!$
!!$  !Set up routing tables for halo exchanges
!!$  rc = ESMF_SUCCESS
!!$  ! Create an ESMF DistGrid, which describes which partition owns which cells
!!$  self%distgrid = ESMF_DistGridCreate( arbSeqIndexList= &
!!$                                  self%global_dof_id(1:self%last_dof_owned), &
!!$                                       rc=rc )
!!$
!!$  allocate(self%haloHandle(size(self%last_dof_halo)))
!!$
!!$  do idepth = 1, size(self%last_dof_halo)
!!$
!!$    halo_start  = self%last_dof_owned+1
!!$    halo_finish = self%last_dof_halo(idepth)
!!$    !If this is a serial run (no halos), halo_start is out of bounds - so fix it
!!$    if(halo_start > self%last_dof_halo(idepth))then
!!$      halo_start  = self%last_dof_halo(idepth)
!!$      halo_finish = self%last_dof_halo(idepth) - 1
!!$    end if
!!$    ! Can only halo-swap an ESMF array so set up a temporary one that's big
!!$    ! enough to hold all the owned cells and all the halos
!!$    if (rc == ESMF_SUCCESS) &
!!$      temporary_esmf_array = &
!!$        ESMF_ArrayCreate( distgrid=self%distgrid, &
!!$                          typekind=ESMF_TYPEKIND_R8, &
!!$                          haloSeqIndexList= &
!!$                                 self%global_dof_id( halo_start:halo_finish ), &
!!$                          rc=rc )
!!$
!!$    ! Calculate the routing table required to perform the halo-swap, so the
!!$    ! code knows where to find the values it needs to fill in the halos
!!$    if (rc == ESMF_SUCCESS) &
!!$      call ESMF_ArrayHaloStore( array=temporary_esmf_array, &
!!$                                routehandle=self%haloHandle(idepth), &
!!$                                rc=rc )
!!$    ! Clean up the temporary array used to generate the routing table 
!!$    if (rc == ESMF_SUCCESS) &
!!$      call ESMF_ArrayDestroy(array=temporary_esmf_array, &
!!$                             noGarbage=.TRUE. , &
!!$                             rc=rc)
!!$
!!$  end do
!!$
!!$  if (rc /= ESMF_SUCCESS) call log_event( &
!!$    'ESMF failed to generate the halo routing table', &
!!$    LOG_LEVEL_ERROR )
!!$
!!$  deallocate (dofmap)
!!$
!!$  return
!!$end subroutine init_function_space

!-----------------------------------------------------------------------------
! Gets total local unique dofs for this space
!-----------------------------------------------------------------------------
integer function get_undf(self)
  implicit none

  class(function_space_type), intent(in) :: self

  get_undf = self%last_dof_halo(size(self%last_dof_halo))

  return
end function get_undf

!-----------------------------------------------------------------------------
! Gets the number of cells for this function space
!-----------------------------------------------------------------------------
function get_ncell(self) result(ncells_2d)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: ncells_2d

  ncells_2d = self%mesh%get_ncells_2d()

  return
end function get_ncell

!-----------------------------------------------------------------------------
! Gets the number of layers for this functions space
!-----------------------------------------------------------------------------
function get_nlayers(self) result(nlayers)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: nlayers

  nlayers = self%mesh%get_nlayers()

  return
end function get_nlayers

!-----------------------------------------------------------------------------
! Gets the number of dofs for a single cell
!-----------------------------------------------------------------------------
function get_ndf(self) result(ndof_cell)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: ndof_cell

  ndof_cell= self%ndof_cell

  return
end function get_ndf

!-----------------------------------------------------------------------------
! Gets the dofmap for a single cell
!-----------------------------------------------------------------------------
function get_cell_dofmap(self,cell_lid) result(map)

  implicit none
  class(function_space_type), target, intent(in) :: self
  integer(i_def),                     intent(in) :: cell_lid
  integer(i_def), pointer                        :: map(:)

  !map => self%master_dofmap%get_master_dofmap(cell_lid)
  return
end function get_cell_dofmap

!-----------------------------------------------------------------------------
! Gets the dofmap for the whole domain
!-----------------------------------------------------------------------------
function get_whole_dofmap(self) result(map)

  implicit none
  class(function_space_type), target, intent(in) :: self
  integer(i_def), pointer                        :: map(:,:)

  !map => self%master_dofmap%get_whole_master_dofmap()
  return
end function get_whole_dofmap

!-----------------------------------------------------------------------------
! Gets the nodal coordinates of the function_space
!-----------------------------------------------------------------------------
function get_nodes(self) result(nodal_coords)

  implicit none
  class(function_space_type), target, intent(in)  :: self

  real(r_def), pointer :: nodal_coords(:,:)

  nodal_coords => self%nodal_coords

  return
end function get_nodes

!-----------------------------------------------------------------------------
! Gets a flag for dofs on vertical boundaries
!-----------------------------------------------------------------------------
function get_boundary_dofs(self) result(boundary_dofs)

  implicit none
  class(function_space_type), target, intent(in) :: self

  integer(i_def), pointer :: boundary_dofs(:,:)

  boundary_dofs => self%dof_on_vert_boundary(:,:)

  return
end function get_boundary_dofs

!-----------------------------------------------------------------------------
! Gets enumerated integer for the function space
!-----------------------------------------------------------------------------
function which(self) result(fs)

  implicit none
  class(function_space_type),  intent(in) :: self
  integer(i_def) :: fs

  fs = self%fs

  return
end function which

!-----------------------------------------------------------------------------
! Gets the size of the function space
!-----------------------------------------------------------------------------
function get_dim_space(self) result(dim)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: dim

  dim = self%dim_space

  return
end function get_dim_space

!-----------------------------------------------------------------------------
! Gets the size of the diferential function space
!-----------------------------------------------------------------------------
function get_dim_space_diff(self) result(dim)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: dim

  dim = self%dim_space_diff

  return
end function get_dim_space_diff

!-----------------------------------------------------------------------------
! Evaluates one of the listed (func_to_call) functions
!-----------------------------------------------------------------------------
function evaluate_function(self, func_to_call, df, xi) result(evaluate)

  class(function_space_type)  :: self
  integer(i_def), intent(in)  :: func_to_call
  integer(i_def), intent(in)  :: df
  real(r_def),    intent(in)  :: xi(3)
  real(r_def),   allocatable  :: evaluate(:)

!!$  select case ( func_to_call )
!!$
!!$    case( BASIS )
!!$      allocate( evaluate(self%dim_space) )
!!$      evaluate = evaluate_basis(self, df, xi)
!!$
!!$    case( DIFF_BASIS )
!!$      allocate( evaluate(self%dim_space_diff) )
!!$      evaluate = evaluate_diff_basis(self, df, xi)
!!$
!!$    case default
!!$      call log_event( &
!!$      'func_to_call does not match the available enumerators', &
!!$      LOG_LEVEL_ERROR )
!!$  
!!$  end select
  evaluate(:) = 1.0_r_def
end function evaluate_function

!-----------------------------------------------------------------------------
! Evaluates a basis function at a point
!-----------------------------------------------------------------------------
function evaluate_basis(self, df, xi) result(p)

  !use polynomial_mod, only: poly1d

  class(function_space_type), intent(in)  :: self

  integer(i_def), intent(in)  :: df
  real(r_def),    intent(in)  :: xi(3)
  real(r_def)                 :: p(self%dim_space)

  p(:) = 1.0_r_def
!!$  p(:) = poly1d( self%basis_order(1,df), xi(1), self%basis_x(:,1,df), self%basis_index(1,df)) &
!!$       * poly1d( self%basis_order(2,df), xi(2), self%basis_x(:,2,df), self%basis_index(2,df)) &
!!$       * poly1d( self%basis_order(3,df), xi(3), self%basis_x(:,3,df), self%basis_index(3,df)) &
!!$       * self%basis_vector(:,df)

end function evaluate_basis

!-----------------------------------------------------------------------------
! Evaluates the differential of a basis function at a point
!-----------------------------------------------------------------------------
pure function evaluate_diff_basis(self, df, xi) result(evaluate)

  !use polynomial_mod, only: poly1d, poly1d_deriv

  class(function_space_type), intent(in)  :: self

  integer(i_def), intent(in)  :: df
  real(r_def),    intent(in)  :: xi(3)
  real(r_def)                 :: evaluate(self%dim_space_diff)
  real(r_def)                 :: dpdx(3)

!!$  dpdx(1) = poly1d_deriv( self%basis_order(1,df), xi(1)                 &
!!$                        , self%basis_x(:,1,df), self%basis_index(1,df)) &
!!$          * poly1d      ( self%basis_order(2,df), xi(2)                 &
!!$                        , self%basis_x(:,2,df), self%basis_index(2,df)) &
!!$          * poly1d      ( self%basis_order(3,df), xi(3)                 &
!!$                        , self%basis_x(:,3,df), self%basis_index(3,df))
!!$
!!$  dpdx(2) = poly1d      ( self%basis_order(1,df), xi(1)                 &
!!$                        , self%basis_x(:,1,df), self%basis_index(1,df)) &
!!$          * poly1d_deriv( self%basis_order(2,df), xi(2)                 &
!!$                        , self%basis_x(:,2,df), self%basis_index(2,df)) &
!!$          * poly1d      ( self%basis_order(3,df), xi(3)                 &
!!$                        , self%basis_x(:,3,df), self%basis_index(3,df))
!!$
!!$  dpdx(3) = poly1d      ( self%basis_order(1,df), xi(1)                 &
!!$                        , self%basis_x(:,1,df), self%basis_index(1,df)) &
!!$          * poly1d      ( self%basis_order(2,df), xi(2)                 &
!!$                        , self%basis_x(:,2,df), self%basis_index(2,df)) &
!!$          * poly1d_deriv( self%basis_order(3,df), xi(3)                 &
!!$                        , self%basis_x(:,3,df), self%basis_index(3,df))
!!$
!!$
!!$  if ( self%dim_space == 1 .and. self%dim_space_diff == 3 ) then
!!$    ! grad(p)
!!$    evaluate(1) = dpdx(1)
!!$    evaluate(2) = dpdx(2)
!!$    evaluate(3) = dpdx(3)
!!$  else if ( self%dim_space == 3 .and. self%dim_space_diff == 3 ) then
!!$    ! curl(p)
!!$    evaluate(1) = dpdx(2)*self%basis_vector(3,df) - dpdx(3)*self%basis_vector(2,df)
!!$    evaluate(2) = dpdx(3)*self%basis_vector(1,df) - dpdx(1)*self%basis_vector(3,df)
!!$    evaluate(3) = dpdx(1)*self%basis_vector(2,df) - dpdx(2)*self%basis_vector(1,df)
!!$  else if ( self%dim_space == 3 .and. self%dim_space_diff == 1 ) then
!!$    ! div(p)
!!$    evaluate(1) = dpdx(1)*self%basis_vector(1,df) + dpdx(2)*self%basis_vector(2,df)  &
!!$          + dpdx(3)*self%basis_vector(3,df)
!!$  else if ( self%dim_space == 1 .and. self%dim_space_diff == 1 ) then
!!$    ! dp/dz
!!$    evaluate(1) = dpdx(3)
!!$  else
!!$    evaluate(:) = 0.0_r_def
!!$  end if

  evaluate(:) = 1.0_r_def

end function evaluate_diff_basis

!-----------------------------------------------------------------------------
! Evaluates the basis function for a given quadrature
!-----------------------------------------------------------------------------
subroutine compute_basis_function(self, basis, ndf, qp_h, qp_v, x_qp, z_qp)

  implicit none

  class(function_space_type), intent(in) :: self

  integer(i_def), intent(in)  :: ndf
  integer(i_def), intent(in)  :: qp_h
  integer(i_def), intent(in)  :: qp_v

  real(r_def), intent(in)  :: x_qp (qp_h,2)
  real(r_def), intent(in)  :: z_qp (qp_v)

  real(r_def), intent(out) :: basis(self%dim_space,ndf,qp_h,qp_v)

  ! Local variables - loop counters
  integer(i_def) :: df
  integer(i_def) :: qp1
  integer(i_def) :: qp2
  real(r_def)    :: xyz(3)

  do qp2=1, qp_v
     xyz(3) = z_qp(qp2)
     do qp1=1, qp_h
        xyz(1) = x_qp(qp1,1)
        xyz(2) = x_qp(qp1,2)
        do df=1, ndf
           basis(:,df,qp1,qp2) = self%evaluate_basis(df,xyz)
        end do
     end do
  end do

end subroutine compute_basis_function

!-----------------------------------------------------------------------------
! Evaluates the basis function for a given set of nodal points
!-----------------------------------------------------------------------------
subroutine compute_nodal_basis_function(self, basis, ndf, n_node, x_node)

  implicit none
  class(function_space_type), intent(in)  :: self

  integer(i_def), intent(in) :: ndf
  integer(i_def), intent(in) :: n_node

  real(r_def), dimension(3,n_node),                  intent(in)  :: x_node
  real(r_def), dimension(self%dim_space,ndf,n_node), intent(out) :: basis

  ! local variables - loop counters
  integer(i_def) :: df
  integer(i_def) :: qp

  do qp = 1, n_node
    do df = 1, ndf
      basis(:,df,qp) = self%evaluate_basis(df,x_node(:,qp))
    end do
  end do

end subroutine compute_nodal_basis_function

!-----------------------------------------------------------------------------
! Evaluates the differential basis function for a given quadrature
!-----------------------------------------------------------------------------
subroutine compute_diff_basis_function(self,                                & 
                                       dbasis,                              &
                                       ndf,                                 &
                                       qp_h,                                &
                                       qp_v,                                &
                                       x_qp,                                &
                                       z_qp)


  implicit none

  class(function_space_type), intent(in) :: self

  integer(i_def), intent(in) :: ndf
  integer(i_def), intent(in) :: qp_h
  integer(i_def), intent(in) :: qp_v

  real(r_def), intent(in)  :: x_qp(qp_h,2)
  real(r_def), intent(in)  :: z_qp(qp_v)

  real(r_def), intent(out) :: dbasis(self%dim_space_diff, ndf, qp_h, qp_v)

  ! local variables - loop counters
  integer(i_def) :: df
  integer(i_def) :: qp1
  integer(i_def) :: qp2
  real(r_def)    :: xyz(3)

  do qp2=1, qp_v
     xyz(3) = z_qp(qp2)
     do qp1=1, qp_h
        xyz(1) = x_qp(qp1,1)
        xyz(2) = x_qp(qp1,2)
        do df=1, ndf
           dbasis(:,df,qp1,qp2) = self%evaluate_diff_basis(df,xyz)
        end do
     end do
  end do

end subroutine compute_diff_basis_function

!-----------------------------------------------------------------------------
! Evaluates the diff basis function for a given set of nodal points
!-----------------------------------------------------------------------------
subroutine compute_nodal_diff_basis_function(self, &
     dbasis, ndf, n_node, x_node)
  implicit none
  class(function_space_type), intent(in)  :: self
  integer,                                                intent(in)  :: ndf
  integer,                                                intent(in)  :: n_node
  real(kind=r_def), dimension(3,n_node),                  intent(in)  :: x_node
  real(kind=r_def), dimension(self%dim_space_diff,ndf,n_node), intent(out) :: dbasis

  ! local variables - loop counters
  integer :: df
  integer :: qp

  do qp = 1, n_node
    do df = 1, ndf
      dbasis(:,df,qp) = self%evaluate_diff_basis(df,x_node(:,qp))
    end do
  end do
  
end subroutine compute_nodal_diff_basis_function

!-----------------------------------------------------------------------------
! Gets order for this space
!-----------------------------------------------------------------------------
!> @brief Gets the polynomial order for this space, returns an integer
!> @param[in] self the calling function space
!-----------------------------------------------------------------------------
function get_element_order(self) result (element_order)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: element_order

  element_order = self%element_order

  return
end function get_element_order

!-----------------------------------------------------------------------------
!> @details Gets the order for this function space
!> @return  The order of the function space
function get_fs_order(self) result (fs_order)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: fs_order

  fs_order = self%fs_order

  return
end function get_fs_order

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

  mesh => self%mesh

  return
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

  return
end function get_mesh_id

!-----------------------------------------------------------------------------
! Gets a routing table for halo swapping
!-----------------------------------------------------------------------------
!!$function get_haloHandle(self, depth) result (haloHandle)
!!$
!!$  implicit none
!!$  class(function_space_type) :: self
!!$  integer(i_def) , intent(in) :: depth
!!$
!!$  type(ESMF_RouteHandle) :: haloHandle
!!$
!!$  haloHandle = self%haloHandle(depth)
!!$
!!$  return
!!$end function get_haloHandle

!-----------------------------------------------------------------------------
! Gets the ESMF distributed grid description for the function space
!-----------------------------------------------------------------------------
!!$function get_distgrid(self) result (distgrid)
!!$
!!$  implicit none
!!$  class(function_space_type) :: self
!!$
!!$  type(ESMF_DistGrid) :: distgrid
!!$
!!$  distgrid = self%distgrid
!!$
!!$  return
!!$end function get_distgrid

!-----------------------------------------------------------------------------
! Gets the array that holds the global indices of all dofs
!-----------------------------------------------------------------------------
!!$subroutine get_global_dof_id(self, global_dof_id)
!!$
!!$  implicit none
!!$  class(function_space_type) :: self
!!$
!!$  integer(i_def) :: global_dof_id(:)
!!$
!!$  global_dof_id(:) = self%global_dof_id(:)
!!$
!!$  return
!!$end subroutine get_global_dof_id

!-----------------------------------------------------------------------------
! Gets the index within the dofmap of the last "owned" dof
!-----------------------------------------------------------------------------
function get_last_dof_owned(self) result (last_dof_owned)

  implicit none
  class(function_space_type) :: self

  integer(i_def) :: last_dof_owned

  last_dof_owned = self%last_dof_owned

  return
end function get_last_dof_owned

!-----------------------------------------------------------------------------
! Gets the index within the dofmap of the last dof in the deepest halo
!-----------------------------------------------------------------------------
function get_last_dof_halo(self) result (last_dof_halo)

  implicit none
  class(function_space_type) :: self

  integer(i_def) :: last_dof_halo

  last_dof_halo = self%last_dof_halo(size(self%last_dof_halo))

  return
end function get_last_dof_halo


!> Get the instance of a stencil dofmap with for a given shape and size
!> @param[in] stencil_shape The shape identifier for the stencil dofmap to
!create
!> @param[in] stencil_size The number of cells in the stencil
!> @return map the stencil_dofmap object to return
function get_stencil_dofmap(self, stencil_shape, stencil_size) result(map)

  implicit none

  class(function_space_type), intent(inout) :: self
  integer(i_def),             intent(in) :: stencil_shape
  integer(i_def),             intent(in) :: stencil_size

  type(stencil_dofmap_type), pointer  :: map ! pointer to return instance
!  type(linked_list_item_type),pointer :: loop ! temp pointer for looping


  integer(i_def) :: id

  map => null()
  
  ! Calculate id of the stencil_dofmap we want
!!$  id = stencil_shape*100 + stencil_size
!!$
!!$
!!$  ! point at the head of the stencil_dofmap linked list
!!$  loop => self%dofmap_list%get_head()
!!$
!!$  ! loop through list
!!$  do
!!$    if ( .not. associated(loop) ) then
!!$      ! At the end of list and we didn't find it
!!$      ! create stencil dofmap and add it 
!!$
!!$      call self%dofmap_list%insert_item(stencil_dofmap_type(stencil_shape,    &
!!$                                                            stencil_size,     &
!!$                                                            self%ndof_cell,   &
!!$                                                            self%mesh,        &
!!$                                                            self%master_dofmap))
!!$
!!$
!!$      ! At this point the desired stencil dofmap is the tail of the list
!!$      ! so just retrieve it and exit loop
!!$
!!$      loop => self%dofmap_list%get_tail()
!!$
!!$      ! 'cast' to the stencil_dofmap_type
!!$      select type(v => loop%payload)
!!$        type is (stencil_dofmap_type)
!!$          map => v
!!$      end select
!!$      exit
!!$
!!$    end if
!!$    ! otherwise search list for the id we want
!!$    if ( id == loop%payload%get_id() ) then
!!$      ! 'cast' to the stencil_dofmap_type
!!$      select type(v => loop%payload)
!!$        type is (stencil_dofmap_type)
!!$          map => v
!!$      end select
!!$      exit
!!$    end if
!!$    loop => loop%next
!!$  end do
end function get_stencil_dofmap


!============================================================================
!> @brief  Invoke calculation of colouring for the member mesh.
!============================================================================
subroutine set_colours(self)
  !use mesh_colouring_mod, only : colour_mod_set_colours => set_colours
  implicit none
  class(function_space_type), intent(inout) :: self

  !call self%mesh%set_colours()
  return
end subroutine set_colours

!----------------------------------------------------------------------------
!> @brief   Returns count of colours used in colouring member mesh.
!>
!> @return  Number of colours used to colour this mesh.
!----------------------------------------------------------------------------
function get_ncolours(self) result(ncolours)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def)                         :: ncolours

  ncolours = self%mesh%get_ncolours()

end function get_ncolours

!============================================================================
!> @brief Populates args with colouring info from member mesh.
!>
!> @param[out] ncolours  Number of colours used to colour member mesh.
!> @param[out] ncells_per_colour  Count of cells in each colour.
!> @param[out] colour_map  Indices of cells in each colour.
!============================================================================
subroutine get_colours(self, ncolours, ncells_per_colour, colour_map)
  implicit none
  class(function_space_type), intent(in)    :: self
  integer(i_def), intent(out)               :: ncolours
  integer(i_def), pointer, intent(out)  :: ncells_per_colour(:)
  integer(i_def), pointer, intent(out)  :: colour_map(:,:)


  call self%mesh%get_colours(ncolours, ncells_per_colour, colour_map)

end subroutine get_colours

!-----------------------------------------------------------------------------
!  Function to clear up objects - called by destructor
!-----------------------------------------------------------------------------
!> @details Explcitly deallocates any allocatable arrays in the function space
!>          to avoid memory leaks
!> @return  Error status variable
!!$subroutine clear(self)
!!$
!!$  implicit none
!!$
!!$  class (function_space_type), intent(inout) :: self
!!$  integer (i_def) :: err
!!$
!!$  if (allocated(self%nodal_coords))   deallocate( self%nodal_coords )
!!$  if (allocated(self%basis_order))    deallocate( self%basis_order )
!!$  if (allocated(self%basis_index))    deallocate( self%basis_index )
!!$  if (allocated(self%basis_vector))   deallocate( self%basis_vector )
!!$  if (allocated(self%basis_x))        deallocate( self%basis_x )
!!$  if (allocated(self%global_dof_id))  deallocate( self%global_dof_id )
!!$  if (allocated(self%last_dof_halo))  deallocate( self%last_dof_halo )
!!$  if (allocated(self%haloHandle))     deallocate( self%haloHandle )
!!$
!!$  if (allocated(self%dof_on_vert_boundary)) &
!!$                                      deallocate( self%dof_on_vert_boundary )
!!$
!!$  err = self%master_dofmap%clear()
!!$  call self%dofmap_list%clear()
!!$
!!$end subroutine clear

!-----------------------------------------------------------------------------
! Function space destructor
!-----------------------------------------------------------------------------

!!$subroutine function_space_destructor(self)
!!$
!!$  implicit none
!!$
!!$  type (function_space_type), intent(inout) :: self
!!$
!!$  call self%clear()
!!$
!!$end subroutine function_space_destructor



end module function_space_mod
