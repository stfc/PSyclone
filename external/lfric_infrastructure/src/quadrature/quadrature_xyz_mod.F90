!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains quadrature_xyz_type and quadrature_xyz_type.

!> @details This module contains the quadrature_xyz_type.

!> This type contains points and weights stored in 3D (x-y-z). A proxy
!> is used to access the data. A type bound procedure 'compute_function'
!> is also available. This method uses the call_function defined in
!> function_space_type. The function is evaluated for the xyz arrangement
!>
!> There are two constructors:
!> init_quadrature_symmetrical(np, rule),
!> init_quadrature_variable(np_x, np_y, np_z, rule)
!> where rule is the quadrature rule and np is the number of points in each
!> direction.


module quadrature_xyz_mod

use constants_mod,           only: r_def, i_def, PI, EPS
use log_mod,                 only: LOG_LEVEL_ERROR, log_event, log_scratch_space
use quadrature_rule_mod,     only: quadrature_rule_type
use quadrature_mod,          only: quadrature_type
use function_space_mod,      only: function_space_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! xyz quadrature type
!-------------------------------------------------------------------------------

type, public, extends(quadrature_type) :: quadrature_xyz_type

  private

  !> Allocatable arrays which holds the quadrature weights
  real(kind=r_def), allocatable :: weights_xyz(:)

  !> Allocatable arrays which holds the points
  real(kind=r_def), allocatable :: points_xyz(:,:)

  !> Total number of points
  integer(kind=i_def) :: np_xyz

contains

  ! Get a proxy with public pointers to the data in a quadrature_xyz type.
  procedure, public :: get_quadrature_proxy

  ! Evaluates the a function for given set of 3d points
  procedure, public :: compute_function

  ! Destroy the quadrature object
  procedure, public :: quadrature_final

  ! Object finalizer
  final :: quadrature_destructor

end type quadrature_xyz_type

!> Psy layer representation of a quadrature_xyz type
!>
!> This is an accessor class that allows access to quadrature_xyz_type
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_xyz_proxy_type

  private
  !> Allocatable arrays which holds the values of the gaussian quadrature
  real(kind=r_def), pointer, public :: weights_xyz(:)  => null()
  real(kind=r_def), pointer, public :: points_xyz(:,:) => null()

  !> Number of points
  integer(kind=i_def), public       :: np_xyz

contains

end type quadrature_xyz_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
interface quadrature_xyz_type

  module procedure init_quadrature_variable
  module procedure init_quadrature_symmetrical

end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!> @brief Initialises the xyz quadrature type.
!> @param[in] np_x integer, The number of points in the x-direction
!> @param[in] np_y integer, The number of points in the y-direction
!> @param[in] np_z integer, The number of points in the z-direction
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!>
!> @return An object of type quadrature_xyz_type.
function init_quadrature_variable(np_x, np_y, np_z, rule) result (self)

  implicit none

  type(quadrature_xyz_type)               :: self
  integer(kind=i_def), intent(in)         :: np_x, np_y, np_z
  class(quadrature_rule_type), intent(in) :: rule

  real(kind=r_def), allocatable           :: points_weights_x(:,:)
  real(kind=r_def), allocatable           :: points_weights_y(:,:)
  real(kind=r_def), allocatable           :: points_weights_z(:,:)

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_x( np_x,2 ) )
  allocate( points_weights_y( np_y,2 ) )
  allocate( points_weights_z( np_z,2 ) )

  ! Get a copy of the 1D points and weights
  points_weights_x = rule % quadrature_rule( np_x )
  points_weights_y = rule % quadrature_rule( np_y )
  points_weights_z = rule % quadrature_rule( np_z )

  ! Initialise object data
  self%np_xyz = np_x*np_y*np_z
  call create_quadrature( self, points_weights_x, points_weights_y, &
                          points_weights_z )

  ! Tidy memory
  deallocate( points_weights_x )
  deallocate( points_weights_y )
  deallocate( points_weights_z )

end function init_quadrature_variable
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Initialises the xyz quadrature type.
!> @param[in] np integer, The number of points
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!>
!> @return An object of type quadrature_xyz_type.
function init_quadrature_symmetrical(np, rule) result (self)

  implicit none

  type(quadrature_xyz_type)               :: self
  integer(kind=i_def), intent(in)         :: np
  class(quadrature_rule_type), intent(in) :: rule

  real(kind=r_def), allocatable, target   :: points_weights_x(:,:)
  real(kind=r_def), pointer               :: points_weights_y(:,:) => null()
  real(kind=r_def), pointer               :: points_weights_z(:,:) => null()

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_x( np,2 ) )

  ! Get a copy of the 1D points and weights
  points_weights_x = rule % quadrature_rule( np )
  points_weights_y => points_weights_x
  points_weights_z => points_weights_x

  ! Initialise object data
  self%np_xyz = np*np*np

  call create_quadrature( self, points_weights_x, points_weights_y, &
                          points_weights_z )

  ! Tidy memory
  deallocate( points_weights_x )
  nullify( points_weights_y, points_weights_z )

end function init_quadrature_symmetrical

!-------------------------------------------------------------------------------
!> @brief Distribute quadrature points and weights.
!> @param[in] self, The calling quadrature_type
!> @param[in] points_weights_x real, 1D points and weights in x-direction
!> @param[in] points_weights_y real, 1D points and weights in y-direction
!> @param[in] points_weights_z real, 1D points and weights in z-direction
!> @todo This code is correct for quads but will need modification for
!>       hexes/triangles)
subroutine create_quadrature(self, points_weights_x, points_weights_y, &
                             points_weights_z)

  implicit none

  class(quadrature_xyz_type)   :: self
  real(kind=r_def), intent(in) :: points_weights_x(:,:)
  real(kind=r_def), intent(in) :: points_weights_y(:,:)
  real(kind=r_def), intent(in) :: points_weights_z(:,:)

  integer(kind=i_def)          :: i, j, k, ic

  ! Allocate space for the points of points weights in the quad type
  allocate( self%points_xyz(3,self%np_xyz) )
  allocate( self%weights_xyz(self%np_xyz) )

  ! Initialise all to zero
  self%points_xyz(:,:) = 0.0_r_def
  self%weights_xyz(:)  = 0.0_r_def

  ! Distribute the 1D points and weights
  ic = 1
  do i = 1, size(points_weights_x,1)
    do j = 1, size(points_weights_y,1)
      do k = 1, size(points_weights_z,1)
        self%points_xyz(1,ic) = points_weights_x(i,1)
        self%points_xyz(2,ic) = points_weights_y(j,1)
        self%points_xyz(3,ic) = points_weights_z(k,1)
        self%weights_xyz(ic)  = points_weights_x(i,2)*points_weights_y(j,2)*&
                                points_weights_z(k,2)
        ic = ic + 1
      end do
    end do
  end do

  return

end subroutine create_quadrature
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!>        quadrature_xyz_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_xyz_type.
type(quadrature_xyz_proxy_type) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_xyz_type), target, intent(in) :: self

  get_quadrature_proxy % points_xyz  => self % points_xyz
  get_quadrature_proxy % weights_xyz => self % weights_xyz
  get_quadrature_proxy % np_xyz      = self % np_xyz

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

  class(quadrature_xyz_type), intent(in)  :: self
  type(function_space_type),  intent(in)  :: function_space
  integer(kind=i_def),        intent(in)  :: function_to_call
  integer(kind=i_def),        intent(in)  :: fspace_dim
  integer(kind=i_def),        intent(in)  :: ndf
  real(kind=r_def),           intent(out) :: basis(fspace_dim,ndf,self%np_xyz)

  ! Local variables - loop counters
  integer(kind=i_def) :: df
  integer(kind=i_def) :: qp1

  do qp1 = 1, self%np_xyz
    do df = 1, ndf
      basis(:,df,qp1) = function_space%call_function(function_to_call,df,&
                                                         self%points_xyz(:,qp1))
    end do
  end do

end subroutine compute_function

!-------------------------------------------------------------------------------
!> @brief Routine to destroy quadrature.
!> @param[in] self, The calling quadrature_type
subroutine quadrature_final(self)

  implicit none

  class(quadrature_xyz_type) :: self

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

  type(quadrature_xyz_type) :: self

  call self%quadrature_final()

end subroutine quadrature_destructor
!-------------------------------------------------------------------------------

end module quadrature_xyz_mod
