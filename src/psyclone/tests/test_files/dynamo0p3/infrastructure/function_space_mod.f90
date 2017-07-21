! Modifications copyright (c) 2017, Science and Technology Facilities Council
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
use stencil_dofmap_mod,    only: stencil_dofmap_type, STENCIL_POINT

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

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

end type function_space_type

!-------------------------------------------------------------------------------
! Contained functions/subroutines 
!-------------------------------------------------------------------------------
contains

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

end module function_space_mod
