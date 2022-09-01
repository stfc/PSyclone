!-----------------------------------------------------------------------------
! (C) Crown copyright 2017-2020 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you should have
! received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

!> @brief Contains quadrature_xyoz_type and quadrature_xyoz_type.

!> @details This module contains the quadrature_xyoz_type.
!>
!> This type contains points and weights stored in 2D (x-y - horizontal)
!> and 1D (z - vertical). A proxy is used to access the data. A type bound
!> procedure 'compute_function' is also available. This method uses
!> call_function defined in function_space_type. The function is evaluated for
!> the xyoz arrangement
!>
!> There are two constructors:
!> init_quadrature_symmetrical(np, rule),
!> init_quadrature_variable(np_x, np_y, np_z, rule)
!> where rule is the quadrature rule and np is the number of points in each
!> direction.
module quadrature_xyoz_mod

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
! xyoz quadrature type
!-------------------------------------------------------------------------------

type, public, extends(quadrature_type) :: quadrature_xyoz_type

  private

  !> Allocatable arrays which holds the quadrature weights
  real(kind=r_def), allocatable :: weights_xy(:), weights_z(:)

  !> Allocatable arrays which holds the points (x)
  real(kind=r_def), allocatable :: points_xy(:,:), points_z(:)

  !> Number of quadrature points xy direction = np_x*np_y
  integer(kind=i_def) :: np_xy

  !> Number of quadrature points in each direction
  integer(kind=i_def) :: np_z

contains

  ! Get a proxy with public pointers to the data in a quadrature_xyoz type.
  procedure, public :: get_quadrature_proxy

  ! Evaluates the function for given set of 3d points
  procedure, public :: compute_function

  ! Destroy the quadrature object
  procedure, public :: quadrature_final

  ! Object finalizer
  final :: quadrature_destructor

end type quadrature_xyoz_type

!> Psy layer representation of a quadrature_xyoz type
!>
!> This is an accessor class that allows access to quadrature_xyoz_type
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_xyoz_proxy_type

  private
  !> Allocatable arrays which holds the values of the gaussian quadrature
  real(kind=r_def), pointer, public :: weights_xy(:)  => null()
  real(kind=r_def), pointer, public :: weights_z(:)   => null()
  real(kind=r_def), pointer, public :: points_xy(:,:) => null()
  real(kind=r_def), pointer, public :: points_z(:)    => null()

  !> Number of points
  integer(kind=i_def), public       :: np_xy, np_z

contains

end type quadrature_xyoz_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
interface quadrature_xyoz_type

  module procedure init_quadrature_variable
  module procedure init_quadrature_symmetrical

end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!> @brief Initialises the xyoz quadrature type.
!> @param[in] np_x integer, The number of points in the x-direction
!> @param[in] np_y integer, The number of points in the y-direction
!> @param[in] np_z integer, The number of points in the z-direction
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!>
!> @return An object of type quadrature_xyoz_type.
function init_quadrature_variable(np_x, np_y, np_z, rule) result (self)

  implicit none

  type(quadrature_xyoz_type)              :: self
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
  self%np_xy = np_x*np_y
  self%np_z = np_z
  call create_quadrature( self, points_weights_x, points_weights_y, &
                          points_weights_z )

  ! Tidy memory
  deallocate( points_weights_x )
  deallocate( points_weights_y )
  deallocate( points_weights_z )

end function init_quadrature_variable
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Initialises the xyoz quadrature type.
!> @param[in] np integer, The number of points
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
!>
!> @return An object of type quadrature_xyoz_type.
function init_quadrature_symmetrical(np, rule) result (self)

  implicit none

  type(quadrature_xyoz_type)              :: self
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
  self%np_xy = np*np
  self%np_z = np

  call create_quadrature( self, points_weights_x, points_weights_y, &
                          points_weights_z)

  ! Tidy memory
  deallocate( points_weights_x )
  nullify( points_weights_y, points_weights_z)

end function init_quadrature_symmetrical
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Distribute quadrature points and weights.
!> @param[in] self, The calling quadrature_type
!> @param[in] points_weights_x real, 1D points and weights in x-direction
!> @param[in] points_weights_y real, 1D points and weights in y-direction
!> @param[in] points_weights_z real, 1D points and weights in z-direction
!> @todo This code is correct for quads but will need modification for
!>       hexes/triangles)
subroutine create_quadrature(self,points_weights_x,points_weights_y,&
                             points_weights_z)

  implicit none

  class(quadrature_xyoz_type)  :: self
  real(kind=r_def), intent(in) :: points_weights_x(:,:)
  real(kind=r_def), intent(in) :: points_weights_y(:,:)
  real(kind=r_def), intent(in) :: points_weights_z(:,:)

  integer(kind=i_def)          :: i, j, ic

  ! Allocate space for the points of points weights in the quad type
  allocate( self%points_z(self%np_z) )
  allocate( self%weights_z(self%np_z) )
  allocate( self%points_xy(self%np_xy,2) )
  allocate( self%weights_xy(self%np_xy) )

  ! Initialise all to zero
  self%points_z(:)    = 0.0_r_def
  self%weights_z(:)   = 0.0_r_def
  self%points_xy(:,:) = 0.0_r_def
  self%weights_xy(:)  = 0.0_r_def

  ! Distribute the 1D points and weights
  ! This is correct for quads (will need modification for hexes/triangles)
  self%points_z = points_weights_z(:,1)
  self%weights_z = points_weights_z(:,2)

  ic = 1
  do i = 1, size(points_weights_x,1)
    do j = 1, size(points_weights_y,1)
      self%points_xy(ic,1) = points_weights_x(i,1)
      self%points_xy(ic,2) = points_weights_y(j,1)
      self%weights_xy(ic) = points_weights_x(i,2)*points_weights_y(j,2)
      ic = ic + 1
    end do
  end do

  return

end subroutine create_quadrature
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!>        quadrature_xyoz_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_xyoz_type.
type(quadrature_xyoz_proxy_type) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_xyoz_type), target, intent(in)  :: self

  get_quadrature_proxy % points_xy  => self % points_xy
  get_quadrature_proxy % points_z   => self % points_z
  get_quadrature_proxy % weights_xy => self % weights_xy
  get_quadrature_proxy % weights_z  => self % weights_z
  get_quadrature_proxy % np_xy      = self % np_xy
  get_quadrature_proxy % np_z       = self % np_z

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
  class(quadrature_xyoz_type), intent(in)  :: self
  type(function_space_type),   intent(in)  :: function_space
  integer(kind=i_def),         intent(in)  :: function_to_call
  integer(kind=i_def),         intent(in)  :: fspace_dim
  integer(kind=i_def),         intent(in)  :: ndf
  real(kind=r_def),            intent(out) :: basis(fspace_dim,ndf, &
                                                    self%np_xy,     &
                                                    self%np_z)

  ! Local variables - loop counters
  integer(kind=i_def) :: df
  real(kind=r_def)    :: xyz(3)
  integer(kind=i_def) :: qp1
  integer(kind=i_def) :: qp2

  do qp2 = 1, self%np_z
    xyz(3) = self%points_z(qp2)
    do qp1 = 1, self%np_xy
      xyz(1) = self%points_xy(qp1,1)
      xyz(2) = self%points_xy(qp1,2)
      do df = 1, ndf
          basis(:,df,qp1,qp2) = function_space%call_function(&
                                               function_to_call,df,xyz)
      end do
    end do
  end do

end subroutine compute_function

!-------------------------------------------------------------------------------
!> @brief Routine to destroy quadrature.
!> @param[in] self, The calling quadrature_type
subroutine quadrature_final(self)

  implicit none

  class(quadrature_xyoz_type) :: self

  if (allocated(self%points_z))   deallocate(self%points_z)
  if (allocated(self%points_xy))  deallocate(self%points_xy)
  if (allocated(self%weights_z))  deallocate(self%weights_z)
  if (allocated(self%weights_xy)) deallocate(self%weights_xy)

end subroutine quadrature_final

!-------------------------------------------------------------------------------
!> @brief Finalizer routine which should automatically call quadrature_final
!>        when object is out of scope.
!> @param[in] self, The calling quadrature_type
subroutine quadrature_destructor(self)

  implicit none

  type(quadrature_xyoz_type) :: self

  call self%quadrature_final()

end subroutine quadrature_destructor

!-------------------------------------------------------------------------------

end module quadrature_xyoz_mod
