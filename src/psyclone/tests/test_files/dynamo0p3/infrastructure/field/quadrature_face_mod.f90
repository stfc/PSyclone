

!-----------------------------------------------------------------------------
! (c) Crown copyright 2018-2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

!> @brief Quadrature object for computing quadrature on faces of a
!>        cell.
!> @details Face quadrature object that contains quadrature points and weights
!> stored in 3D (x-y-z),
!> on one of a number of faces of a cell. A proxy
!> is used to access the data. A type bound procedure 'compute_function'
!> is also available. This method uses the call_function defined in
!> function_space_type. The function is evaluated for the xyz arrangement


module quadrature_face_mod

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
! face quadrature type
!-------------------------------------------------------------------------------

type, public, extends(quadrature_type) :: quadrature_face_type

  private

  !> Allocatable arrays which holds the quadrature weights
  real(kind=r_def), allocatable :: weights_xyz(:,:)

  !> Allocatable arrays which holds the points
  real(kind=r_def), allocatable :: points_xyz(:,:,:)

  !> Total number of points
  integer(kind=i_def) :: np_xyz

  !> Number of faces (vertical, horizontal, and total)
  integer(kind=i_def) :: nfaces
  integer(kind=i_def) :: nfaces_horizontal, nfaces_vertical

contains

  ! Get a proxy with public pointers to the data in a quadrature_face type.
  procedure, public :: get_quadrature_proxy

  ! Evaluates the a function for given set of 3d points
  procedure, public :: compute_function

  ! Destroy the quadrature object
  procedure, public :: quadrature_final

  ! Object finalizer
  final :: quadrature_destructor

end type quadrature_face_type

!> PSy layer representation of a quadrature_face type
!>
!> This is an accessor class that allows access to quadrature_face_type
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_face_proxy_type

  private
  !> Allocatable arrays which hold the values of the gaussian quadrature
  real(kind=r_def), pointer, public :: weights_xyz(:,:)  => null()
  real(kind=r_def), pointer, public :: points_xyz(:,:,:) => null()

  !> Number of points
  integer(kind=i_def), public       :: np_xyz

  !> Number of faces (vertical, horizontal, and total)
  integer(kind=i_def), public       :: nfaces
  integer(kind=i_def), public       :: nfaces_horizontal, nfaces_vertical

contains

end type quadrature_face_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
interface quadrature_face_type

  module procedure init_quadrature_variable
  module procedure init_quadrature_symmetrical

end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!> @brief Initialises the face quadrature type using a different number of
!>        points in both directions.
!> @param[in] np_1 integer, The number of points in the 1st direction
!> @param[in] np_2 integer, The number of points in the 2nd direction
!> @param[in] horizontal_faces, Compute quadrature on horizontal faces
!> @param[in] vertical_faces, Compute quadrature on vertical faces
!> @param[in] reference_element Element to compute quadrature on
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!> @return An object of type quadrature_face_type.
function init_quadrature_variable(np_1, np_2, horizontal_faces, vertical_faces, &
                                  reference_element, rule) result (self)

  implicit none

  type(quadrature_face_type)                :: self
  integer(kind=i_def),           intent(in) :: np_1, np_2
  logical,                       intent(in) :: horizontal_faces, vertical_faces
  class(reference_element_type), intent(in) :: reference_element
  class(quadrature_rule_type),   intent(in) :: rule

  real(kind=r_def), allocatable           :: points_weights_1(:,:)
  real(kind=r_def), allocatable           :: points_weights_2(:,:)

  if ( .not. (horizontal_faces .or. vertical_faces) ) then
      write( log_scratch_space, '(A)' )  'Invalid face choice for quadrature'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  if ( horizontal_faces ) then
    self%nfaces_horizontal = reference_element%get_number_horizontal_faces()
  else
    self%nfaces_horizontal = 0
  end if

  if ( vertical_faces ) then
    self%nfaces_vertical   = reference_element%get_number_vertical_faces()
  else
    self%nfaces_vertical   = 0
  end if

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_1( np_1, 2 ) )
  allocate( points_weights_2( np_2, 2) )

  ! Get a copy of the 1D points and weights
  points_weights_1 = rule % quadrature_rule( np_1 )
  points_weights_2 = rule % quadrature_rule( np_2 )

  ! Initialise object data
  self%np_xyz = np_1*np_2
  self%nfaces = self%nfaces_horizontal + self%nfaces_vertical
  call create_quadrature( self, points_weights_1, points_weights_2, &
                          reference_element, horizontal_faces, vertical_faces)

  ! Tidy memory
  deallocate( points_weights_1 )
  deallocate( points_weights_2 )

end function init_quadrature_variable
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Initialises the face quadrature type using the same number of points
!>        in both directions.
!> @param[in] np integer, The number of points
!> @param[in] horizontal_faces, Compute quadrature on horizontal faces
!> @param[in] vertical_faces, Compute quadrature on vertical faces
!> @param[in] reference_element Element to compute quadrature on
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!>
!> @return An object of type quadrature_face_type.
function init_quadrature_symmetrical(np, horizontal_faces, vertical_faces, &
                                     reference_element, rule) result (self)

  implicit none

  type(quadrature_face_type)                :: self
  integer(kind=i_def),           intent(in) :: np
  class(reference_element_type), intent(in) :: reference_element
  class(quadrature_rule_type),   intent(in) :: rule
  logical,                       intent(in) :: horizontal_faces, vertical_faces

  real(kind=r_def), allocatable, target   :: points_weights_1(:,:)
  real(kind=r_def), pointer               :: points_weights_2(:,:) => null()

  if ( horizontal_faces .and. vertical_faces ) then
    self%nfaces_horizontal = reference_element%get_number_horizontal_faces()
    self%nfaces_vertical   = reference_element%get_number_vertical_faces()
  else if ( horizontal_faces ) then
    self%nfaces_horizontal = reference_element%get_number_horizontal_faces()
    self%nfaces_vertical   = 0
  else if ( vertical_faces ) then
    self%nfaces_horizontal = 0
    self%nfaces_vertical   = reference_element%get_number_vertical_faces()
  end if

  if ( .not. (horizontal_faces .or. vertical_faces) ) then
      write( log_scratch_space, '(A)' )  'Invalid face choice for quadrature'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_1( np, 2 ) )

  ! Get a copy of the 1D points and weights
  points_weights_1 = rule % quadrature_rule( np )
  points_weights_2 => points_weights_1

  ! Initialise object data
  self%np_xyz = np*np
  self%nfaces = self%nfaces_horizontal + self%nfaces_vertical
  call create_quadrature( self, points_weights_1, points_weights_2, &
                          reference_element, horizontal_faces, vertical_faces)

  ! Tidy memory
  deallocate( points_weights_1 )
  nullify( points_weights_2)

end function init_quadrature_symmetrical

!-------------------------------------------------------------------------------
!> @brief Distribute quadrature points and weights.
!> @param[in] self, The calling quadrature_type
!> @param[in] points_weights_1 real, 1D points and weights in 1st direction
!> @param[in] points_weights_2 real, 1D points and weights in 2nd direction
!> @param[in] reference_element Element to compute quadrature on
!> @param[in] horizontal_faces, Compute quadrature on horizontal faces
!> @param[in] vertical_faces, Compute quadrature on vertical faces
!> @todo This code is correct for quads but will need modification for
!>       hexes/triangles)
subroutine create_quadrature(self, points_weights_1, points_weights_2, &
                             reference_element,                        &
                             horizontal_faces, vertical_faces)

  implicit none

  class(quadrature_face_type)               :: self
  real(kind=r_def),              intent(in) :: points_weights_1(:,:)
  real(kind=r_def),              intent(in) :: points_weights_2(:,:)
  logical,                       intent(in) :: horizontal_faces, vertical_faces
  class(reference_element_type), intent(in) :: reference_element
  integer(kind=i_def)             :: i, j, face, ij, edge, nedge
  real(kind=r_def), dimension(3)  :: tangent
  integer(kind=i_def)             :: offset
  real(kind=r_def), allocatable, dimension(:)   :: face_x, variable_x
  real(kind=r_def), allocatable, dimension(:,:) :: edge_coords

  ! Allocate space for the points of points weights in the quad type
  allocate( self%points_xyz(3, self%np_xyz, self%nfaces) )
  allocate( self%weights_xyz(self%np_xyz, self%nfaces) )

  ! Initialise all to zero
  self%points_xyz(:,:,:) = 0.0_r_def
  self%weights_xyz(:,:)  = 0.0_r_def

  ! Arrays for assigning coordinates to each face, assumed quad cell with face
  ! layout:
  ! y
  ! |  |--4--|
  ! |  1     3
  ! |  |--2--|
  ! |
  ! --------x

  ! We do this for every edge
  nedge = reference_element%get_number_edges()
  allocate( face_x(nedge), variable_x(nedge) )
  call reference_element%get_edge_centre_coordinates(edge_coords)
  do edge = 1, nedge
    call reference_element%get_tangent_to_edge(edge,tangent)
    ! Flag if: (1) x-coordinate varies along this face or
    !          (0) y-coordinate varies along this face
    variable_x(edge) = abs(tangent(1))
    ! Constant x or y value on each horizontal edge
    face_x(edge) = (1.0_r_def-abs(tangent(1)))*edge_coords(edge,1) &
                 + (1.0_r_def-abs(tangent(2)))*edge_coords(edge,2)
  end do

  ! Determine offset based on whether all or a subset of faces are needed
  if ( horizontal_faces .and. vertical_faces ) then
    ! If all faces are needed: horizontal faces (ex. cubes) are labeled: 1, 2, 3, and 4.
    ! Vertical faces (bottom and top resp.) are labeled: 5 and 6.
    ! Offset in the quadrature arrays depends on the number of horizontal
    ! faces that are present in the quadrature rule.
    offset = self%nfaces_horizontal
  else
    ! Otherwise, no offset since this is only a strictly vertical or horizontal
    ! quadrature rule.
    offset = 0
  end if

  ! Distribute the 1D points and weights:
  ! Collect horizontal face information (if any)
  if ( horizontal_faces ) then
    do face = 1, self%nfaces_horizontal
      ij = 1
      ! Horizontal faces (X-Z or Y-Z quadrature + fixed Y or X)
      do i = 1, size(points_weights_1, 1)
        do j = 1, size(points_weights_2, 1)
          self%points_xyz(1, ij, face) = points_weights_1(i, 1)*variable_x(face) &
                                       + (1.0_r_def - variable_x(face))*face_x(face)
          self%points_xyz(2, ij, face) = points_weights_1(i, 1)*(1.0_r_def       &
                                       - variable_x(face)) + variable_x(face)*face_x(face)
          self%points_xyz(3, ij, face) = points_weights_2(j, 1)
          self%weights_xyz(ij,   face) = points_weights_1(i, 2)*points_weights_2(j, 2)
          ij = ij + 1
        end do
      end do
    end do
  end if

  ! Collect vertical face information (if any)
  if ( vertical_faces ) then
    do face = 1, self%nfaces_vertical
      ij = 1
      ! Vertical faces (X-Y quadrature + Z = 0,1), assumes face 1 has z = 0 and
      ! face 2 has z = 1
      do i = 1, size(points_weights_1, 1)
        do j = 1, size(points_weights_2, 1)
          self%points_xyz(1, ij, face + offset) = points_weights_1(i, 1)
          self%points_xyz(2, ij, face + offset) = points_weights_2(j, 1)
          self%points_xyz(3, ij, face + offset) = real(face - 1, r_def)
          self%weights_xyz(ij,   face + offset) = points_weights_1(i, 2)*points_weights_2(j, 2)
          ij = ij + 1
        end do
      end do
    end do
  end if

  deallocate(variable_x, face_x, edge_coords)

  return

end subroutine create_quadrature
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!>        quadrature_face_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_face_type.
type(quadrature_face_proxy_type) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_face_type), target, intent(in) :: self

  get_quadrature_proxy % points_xyz  => self % points_xyz
  get_quadrature_proxy % weights_xyz => self % weights_xyz
  get_quadrature_proxy % np_xyz      = self % np_xyz
  get_quadrature_proxy % nfaces      = self % nfaces

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

  class(quadrature_face_type), intent(in)  :: self
  type(function_space_type),   intent(in)  :: function_space
  integer(kind=i_def),         intent(in)  :: function_to_call
  integer(kind=i_def),         intent(in)  :: fspace_dim
  integer(kind=i_def),         intent(in)  :: ndf
  real(kind=r_def),            intent(out) :: basis(fspace_dim,ndf,self%np_xyz,self%nfaces)

  ! Local variables - loop counters
  integer(kind=i_def) :: df
  integer(kind=i_def) :: qp1
  integer(kind=i_def) :: face

  do face = 1, self%nfaces
    do qp1 = 1, self%np_xyz
      do df = 1, ndf
        basis(:,df,qp1,face) = function_space%call_function(function_to_call,df,&
                                                     self%points_xyz(:,qp1,face))
      end do
    end do
  end do

end subroutine compute_function

!-------------------------------------------------------------------------------
!> @brief Routine to destroy quadrature.
!> @param[in] self, The calling quadrature_type
subroutine quadrature_final(self)

  implicit none

  class(quadrature_face_type) :: self

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

  type(quadrature_face_type) :: self

  call self%quadrature_final()

end subroutine quadrature_destructor
!-------------------------------------------------------------------------------

end module quadrature_face_mod
