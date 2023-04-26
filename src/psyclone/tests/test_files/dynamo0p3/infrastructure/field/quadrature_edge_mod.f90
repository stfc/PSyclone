

!-----------------------------------------------------------------------------
! (c) Crown copyright 2018-2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

!> @brief Edge Quadrature object for computing quadrature on the edges of a
!>        cell.

!> @details Compute a quadrature object whose points are located on the edeges
!> og a cell. this type contains points and weights stored in 3D (x-y-z), on one of a
!> number of edges. A proxy
!> is used to access the data. A type bound procedure 'compute_function'
!> is also available. This method uses the call_function defined in
!> function_space_type. The function is evaluated for the xyz arrangement


module quadrature_edge_mod

use constants_mod,           only: r_def, i_def, PI, EPS
use log_mod,                 only: LOG_LEVEL_ERROR, log_event, log_scratch_space
use quadrature_rule_mod,     only: quadrature_rule_type
use quadrature_mod,          only: quadrature_type
use function_space_mod,      only: function_space_type
use reference_element_mod,   only: reference_element_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! edge quadrature type
!-------------------------------------------------------------------------------

type, public, extends(quadrature_type) :: quadrature_edge_type

  private

  !> Allocatable arrays which holds the quadrature weights
  real(kind=r_def), allocatable :: weights_xyz(:,:)

  !> Allocatable arrays which holds the points
  real(kind=r_def), allocatable :: points_xyz(:,:,:)

  !> Total number of points
  integer(kind=i_def) :: np_xyz

  !> Total number of edges
  integer(kind=i_def) :: nedges

  !> Edges in the horizontal and vertical
  integer(kind=i_def) :: nedges_horizontal, nedges_vertical

contains

  ! Get a proxy with public pointers to the data in a quadrature_edge type.
  procedure, public :: get_quadrature_proxy

  ! Evaluates the a function for given set of 3d points
  procedure, public :: compute_function

  ! Destroy the quadrature object
  procedure, public :: quadrature_final

  ! Object finalizer
  final :: quadrature_destructor

end type quadrature_edge_type

!> Psy layer representation of a quadrature_edge type
!>
!> This is an accessor class that allows access to quadrature_edge_type
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_edge_proxy_type

  private
  !> Allocatable arrays which hold the values of the gaussian quadrature
  real(kind=r_def), pointer, public :: weights_xyz(:,:)  => null()
  real(kind=r_def), pointer, public :: points_xyz(:,:,:) => null()

  !> Number of points
  integer(kind=i_def), public       :: np_xyz

  !> Number of edges (vertical, horizontal, and total)
  integer(kind=i_def), public       :: nedges
  integer(kind=i_def), public       :: nedges_horizontal, nedges_vertical

contains

end type quadrature_edge_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
interface quadrature_edge_type

  module procedure init_quadrature

end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!> @brief Initialises the edge quadrature type.
!> @param[in] np_1 integer, The number of points in the 1st direction
!> @param[in] horizontal_edges Compute quadrature on horizontal edges
!> @param[in] vertical_edges Compute quadrature on vertical edges
!> @param[in] reference_element Element to compute quadrature on
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!>
!> @return An object of type quadrature_edge_type.
function init_quadrature(np_1, horizontal_edges, vertical_edges, &
                         reference_element, rule) result (self)

  implicit none

  type(quadrature_edge_type)                :: self
  integer(kind=i_def),           intent(in) :: np_1
  logical,                       intent(in) :: horizontal_edges, vertical_edges
  class(reference_element_type), intent(in) :: reference_element
  class(quadrature_rule_type),   intent(in) :: rule

  real(kind=r_def), allocatable           :: points_weights_1(:,:)


  if ( .not. (horizontal_edges .or. vertical_edges) ) then
      write( log_scratch_space, '(A)' )  'Invalid edge choice for quadrature'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  if ( horizontal_edges ) then
    self%nedges_horizontal = reference_element%get_number_horizontal_edges()
  else
    self%nedges_horizontal = 0
  end if

  if ( vertical_edges ) then
    self%nedges_vertical   = reference_element%get_number_vertical_edges()
  else
    self%nedges_vertical   = 0
  end if

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_1( np_1, 2) )

  ! Get a copy of the 1D points and weights
  points_weights_1 = rule % quadrature_rule( np_1 )

  ! Initialise object data
  self%np_xyz = np_1
  self%nedges = self%nedges_horizontal + self%nedges_vertical
  call create_quadrature( self, points_weights_1, reference_element, &
                          horizontal_edges, vertical_edges)

  ! Tidy memory
  deallocate( points_weights_1 )

end function init_quadrature
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!> @brief Distribute quadrature points and weights.
!> @param[in] self, The calling quadrature_type
!> @param[in] points_weights_1 real, 1D points and weights in 1st direction
!> @param[in] points_weights_2 real, 1D points and weights in 2nd direction
!> @param[in] horizontal_edges Compute quadrature on horiztonal edges
!> @param[in] vertical_edges Compute quadrature on vertical edges
!> @param[in] reference_element Element to compute quadrature on
!> @todo This code is correct for quads but will need modification for
!>       hexes/triangles)
subroutine create_quadrature(self, points_weights_1, reference_element, &
                             horizontal_edges, vertical_edges)

  implicit none

  class(quadrature_edge_type)               :: self
  real(kind=r_def),              intent(in) :: points_weights_1(:,:)
  logical,                       intent(in) :: horizontal_edges, vertical_edges
  class(reference_element_type), intent(in) :: reference_element
  integer(kind=i_def)                       :: i, edge, vertex ,nvertex, nedge
  real(kind=r_def), dimension(3)            :: vert, tangent

  real(kind=r_def), allocatable, dimension(:)   :: vert_x, vert_y, edge_x, variable_x
  real(kind=r_def), allocatable, dimension(:,:) :: edge_coords

  integer(kind=i_def) :: offset, vert_offset, horiz_extent

  ! Allocate space for the points of points weights in the quad type
  allocate( self%points_xyz(3, self%np_xyz, self%nedges) )
  allocate( self%weights_xyz(self%np_xyz, self%nedges) )

  ! Initialise all to zero
  self%points_xyz(:,:,:) = 0.0_r_def
  self%weights_xyz(:,:)  = 0.0_r_def

  ! Arrays for assigning coordinates to each edge, assumed quad cell with edge
  ! layout:
  ! z=0, horizontal edges
  ! y
  ! |  |--4--|
  ! |  1     3
  ! |  |--2--|
  ! |
  ! --------x
  !
  ! z=1, horizontal edges
  ! y
  ! |  |--8--|
  ! |  5     7
  ! |  |--6--|
  ! |
  ! --------x
  !
  ! Vertical edges
  ! y
  ! |  4-----3
  ! |  |     |
  ! |  1-----2
  ! |
  ! --------x

  ! We do this for every edge but only need the first four
  nedge = reference_element%get_number_edges()
  allocate( edge_x(nedge), variable_x(nedge) )
  call reference_element%get_edge_centre_coordinates(edge_coords)
  do edge = 1, nedge
    call reference_element%get_tangent_to_edge(edge,tangent)
    ! Flag if: (1) x-coordinate varies along this edge or
    !          (0) y-coordinate varies along this edge
    variable_x(edge) = abs(tangent(1))
    ! Constant x or y value on each horizontal edge
    edge_x(edge) = (1.0_r_def-abs(tangent(1)))*edge_coords(edge,1) &
                 + (1.0_r_def-abs(tangent(2)))*edge_coords(edge,2)
  end do

  ! Coordinate of each vertex
  nvertex = reference_element%get_number_vertices()
  allocate( vert_x(nvertex), vert_y(nvertex) )
  do vertex = 1, nvertex
    vert = reference_element%get_vertex(vertex)
    vert_x(vertex) = vert(1)
    vert_y(vertex) = vert(2)
  end do

  ! We fill arrays for horizontal edges in the same loop, so we only
  ! need to loop over the 4 (cube) or 3 (tri. prisms) edges on a
  ! horizontal face (top/bottom) of the reference element.
  horiz_extent = self%nedges_horizontal/2

  ! Determine offset based on whether all or a subset of edges are needed
  if ( horizontal_edges .and. vertical_edges ) then
    ! Based on the numbering of edges in the reference element, the
    ! bottom face edges (WB, SB, EB, and NB) are labeled 1 through 4.
    ! The top edges (WT, ST, ET, NT) are 9 through 12. The middle
    ! edges 5 through 8 are vertically aligned.
    ! These offsets ensure that the quadrature arrays match the correct
    ! edge index.
    offset   = horiz_extent
    vert_offset = self%nedges_horizontal
  else
    ! Otherwise, no offset since this is only a strictly vertical or horizontal
    ! quadrature rule.
    offset   = 0
    vert_offset = horiz_extent
  end if

  ! Distribute the 1D points and weights
  ! First we set the points and weights for the horizontally
  ! aligned edges (if any)
  if ( horizontal_edges ) then
    ! Horizontal edges (X or Y quadrature + fixed Y or X and fixed Z)
    do edge = 1, horiz_extent
      do i = 1, size(points_weights_1, 1)
        self%points_xyz(1, i, edge) = points_weights_1(i, 1)*variable_x(edge)               &
                                    + (1.0_r_def - variable_x(edge))*edge_x(edge)
        self%points_xyz(2, i, edge) = points_weights_1(i, 1)*(1.0_r_def - variable_x(edge)) &
                                    + variable_x(edge)*edge_x(edge)
        self%points_xyz(3, i, edge) = 0.0_r_def
        self%weights_xyz(i,   edge) = points_weights_1(i, 2)

        self%points_xyz(1, i, edge + vert_offset) = self%points_xyz(1, i, edge)
        self%points_xyz(2, i, edge + vert_offset) = self%points_xyz(2, i, edge)
        self%points_xyz(3, i, edge + vert_offset) = 1.0_r_def
        self%weights_xyz(i,   edge + vert_offset) = self%weights_xyz(i, edge)
      end do
    end do
  end if
  ! Now the vertical edges (if any)
  if ( vertical_edges ) then
    do edge = 1, self%nedges_vertical
      ! Vertical edges (Z quadrature, and fixed X and Y)
      do i = 1, size(points_weights_1,1)
        self%points_xyz(1, i, edge + offset) = vert_x(edge)
        self%points_xyz(2, i, edge + offset) = vert_y(edge)
        self%points_xyz(3, i, edge + offset) = points_weights_1(i, 1)
        self%weights_xyz(i,   edge + offset) = points_weights_1(i, 2)
      end do
    end do
  end if

  deallocate( vert_x, vert_y, edge_x, variable_x, edge_coords )

  return

end subroutine create_quadrature
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!>        quadrature_edge_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_edge_type.
type(quadrature_edge_proxy_type) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_edge_type), target, intent(in) :: self

  get_quadrature_proxy % points_xyz  => self % points_xyz
  get_quadrature_proxy % weights_xyz => self % weights_xyz
  get_quadrature_proxy % np_xyz      = self % np_xyz
  get_quadrature_proxy % nedges      = self % nedges

end function get_quadrature_proxy
!-------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!> @brief Evaluates the a given function for on a set of 3d points.
!> @param[in] self, The calling quadrature_type
!> @param[in] function_to_call integer, Enumerator defining the function to call
!> @param[in] function_space function_space_type, Function space containing the
!> function to evaluate
!> @param[in] fspace_dim integer, Size of the function to be evaluated
!> @param[in] ndf integer, Number of dofs
!> @param[out] basis real, 3 dimensional array holding the evaluated function
subroutine compute_function(self, function_to_call, function_space, &
                            fspace_dim, ndf, basis)
  implicit none

  class(quadrature_edge_type), intent(in)  :: self
  type(function_space_type),  intent(in)  :: function_space
  integer(kind=i_def),        intent(in)  :: function_to_call
  integer(kind=i_def),        intent(in)  :: fspace_dim
  integer(kind=i_def),        intent(in)  :: ndf
  real(kind=r_def),           intent(out) :: basis(fspace_dim,ndf,self%np_xyz,self%nedges)

  ! Local variables - loop counters
  integer(kind=i_def) :: df
  integer(kind=i_def) :: qp1
  integer(kind=i_def) :: f

  do f = 1, self%nedges
    do qp1 = 1, self%np_xyz
      do df = 1, ndf
        basis(:,df,qp1,f) = function_space%call_function(function_to_call,df,&
                                                         self%points_xyz(:,qp1,f))
      end do
    end do
  end do

end subroutine compute_function

!-------------------------------------------------------------------------------
!> @brief Routine to destroy quadrature.
!> @param[in] self, The calling quadrature_type
subroutine quadrature_final(self)

  implicit none

  class(quadrature_edge_type) :: self

  if (allocated(self%points_xyz))  deallocate(self%points_xyz)
  if (allocated(self%weights_xyz)) deallocate(self%weights_xyz)

end subroutine quadrature_final

!-------------------------------------------------------------------------------
!> @brief Finalizer routine which should automatically call quadrature_final
!>        when object is out of scope.
!> @param[in] self, The calling quadrature_type
!-------------------------------------------------------------------------------
subroutine quadrature_destructor(self)

  implicit none

  type(quadrature_edge_type) :: self

  call self%quadrature_final()

end subroutine quadrature_destructor
!-------------------------------------------------------------------------------

end module quadrature_edge_mod
