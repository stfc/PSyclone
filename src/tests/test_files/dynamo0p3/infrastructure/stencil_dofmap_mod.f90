!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!> @brief A type which holds information about the dofmap.
!> @details Types for storing the general stencil dofmaps of a general size
!> Inherits from linked_list_data_type so it can be stored in a linked_list
!> Allowable stencil types ( the size is variable ):
!> Currently on the point stencil is allowed 
!> 
!> POINT --> [1]
!>   
!> 1DX --> |4|2|1|3|5|
!>   
!>         |5|
!>         |3|
!> 1DY --> |1|
!>         |2|
!>         |4|      
!>
!>           |5|
!> CROSS-> |2|1|4|
!>           |3|
!>
module stencil_dofmap_mod

use constants_mod,     only: i_def
!use master_dofmap_mod, only: master_dofmap_type
use linked_list_data_mod, only : linked_list_data_type


implicit none

private
type, extends(linked_list_data_type), public :: stencil_dofmap_type
  private 
  integer(i_def) :: dofmap_shape
  integer(i_def) :: dofmap_extent
  integer(i_def) :: dofmap_size 
  integer(i_def), allocatable :: dofmap(:,:,:)
contains
  procedure :: get_dofmap
  procedure :: get_whole_dofmap
  procedure :: get_size
end type stencil_dofmap_type

integer(i_def), public, parameter :: STENCIL_POINT = 1100
integer(i_def), public, parameter :: STENCIL_1DX   = 1200
integer(i_def), public, parameter :: STENCIL_1DY   = 1300
integer(i_def), public, parameter :: STENCIL_CROSS = 1400
!!$
!!$interface stencil_dofmap_type
!!$  module procedure stencil_dofmap_constructor
!!$end interface

contains 

!-----------------------------------------------------------------------------
! Construct the stencil dofmap
!-----------------------------------------------------------------------------
!> Function to construct a stencil dofmap
!> @param[in] st_shape The shape of the required stencil
!> @param[in] st_extent Number of stencil cells each side of the centre cell in relevant directions
!> @param[in] master_dofmap The cell dofmap to create the stencil from
!> @return The dofmap object
!!$function stencil_dofmap_constructor( st_shape, st_extent, ndf, mesh, master_dofmap) result(self)
!!$
!!$    use log_mod,  only: log_event,         &
!!$                        log_scratch_space, &
!!$                        LOG_LEVEL_ERROR
!!$    use mesh_mod, only: mesh_type
!!$    use reference_element_mod, only: W, E, N, S
!!$
!!$    integer(i_def),           intent(in) :: st_shape, st_extent, ndf
!!$    type(mesh_type), pointer, intent(in) :: mesh
!!$    type(master_dofmap_type), intent(in) :: master_dofmap
!!$    type(stencil_dofmap_type), target    :: self
!!$
!!$    integer(i_def) :: cell, ncells
!!$    integer(i_def) :: st_size
!!$    integer(i_def), pointer :: map(:) => null()
!!$    integer(i_def) :: cell_in_stencil
!!$    integer(i_def) :: cell_west,  next_cell_west,  &
!!$                      cell_south, next_cell_south, &
!!$                      cell_east,  next_cell_east,  &
!!$                      cell_north, next_cell_north
!!$    integer(i_def) :: west, north, east, south
!!$    integer(i_def) :: direction
!!$    integer(i_def) :: opposite(4)
!!$
!!$    ! Set local directions to be those of the reference element
!!$    opposite(W) = E
!!$    opposite(E) = W
!!$    opposite(N) = S
!!$    opposite(S) = N
!!$
!!$    st_size = compute_dofmap_size(st_shape, st_extent)
!!$
!!$    self%dofmap_shape  = st_shape
!!$    self%dofmap_extent = st_extent
!!$    self%dofmap_size   = st_size
!!$    ! call base class set_id()
!!$    call self%set_id(st_shape*100 + st_extent)
!!$
!!$    ncells = mesh%get_ncells_2d()
!!$    ! Allocate the dofmap array
!!$    allocate( self%dofmap( ndf, self%dofmap_size, ncells ) )
!!$
!!$    ! Compute the dofmap
!!$    select case ( st_shape ) 
!!$      case ( STENCIL_POINT )
!!$        do cell = 1,ncells
!!$          map => master_dofmap%get_master_dofmap(cell)
!!$          self%dofmap(:,1,cell) = map(:)
!!$        end do
!!$      case ( STENCIL_CROSS, STENCIL_1DX, STENCIL_1DY )
!!$        do cell = 1,ncells
!!$          map => master_dofmap%get_master_dofmap(cell)
!!$          self%dofmap(:,1,cell) = map(:)
!!$          cell_in_stencil = 1
!!$          cell_west  = cell
!!$          cell_south = cell
!!$          cell_east  = cell
!!$          cell_north = cell         
!!$          west  = W
!!$          east  = E
!!$          north = N
!!$          south = S
!!$          do while ( cell_in_stencil <= st_size )  
!!$            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DX ) then
!!$              cell_in_stencil = cell_in_stencil + 1
!!$              if (  cell_in_stencil <= st_size ) then
!!$                next_cell_west = mesh%get_cell_next(west,cell_west)
!!$                map => master_dofmap%get_master_dofmap(next_cell_west)
!!$                self%dofmap(:,cell_in_stencil,cell) = map
!!$                ! Compute 'west' direction of new cell west
!!$                do direction = 1,4
!!$                  if ( mesh%get_cell_next(direction,next_cell_west) == cell_west) &
!!$                    west = opposite(direction)
!!$                end do
!!$                cell_west = next_cell_west
!!$              end if
!!$            end if
!!$
!!$            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DY ) then
!!$              cell_in_stencil = cell_in_stencil + 1
!!$              if (  cell_in_stencil <= st_size ) then
!!$                next_cell_south = mesh%get_cell_next(south,cell_south)
!!$                map => master_dofmap%get_master_dofmap(next_cell_south)
!!$                self%dofmap(:,cell_in_stencil,cell) = map
!!$                ! Compute 'south' direction of new cell south
!!$                do direction = 1,4
!!$                  if ( mesh%get_cell_next(direction,next_cell_south) == cell_south) &
!!$                    south = opposite(direction)
!!$                end do
!!$                cell_south = next_cell_south
!!$              end if
!!$            end if
!!$
!!$            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DX ) then
!!$              cell_in_stencil = cell_in_stencil + 1
!!$              if (  cell_in_stencil <= st_size ) then
!!$                next_cell_east = mesh%get_cell_next(east,cell_east)
!!$                map => master_dofmap%get_master_dofmap(next_cell_east)
!!$                self%dofmap(:,cell_in_stencil,cell) = map
!!$                ! Compute 'east' direction of new cell east
!!$                do direction = 1,4
!!$                  if ( mesh%get_cell_next(direction,next_cell_east) == cell_east) &
!!$                    east = opposite(direction)
!!$                end do
!!$                cell_east = next_cell_east
!!$              end if
!!$            end if
!!$
!!$            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DY ) then
!!$              cell_in_stencil = cell_in_stencil + 1
!!$              if (  cell_in_stencil <= st_size ) then
!!$                next_cell_north = mesh%get_cell_next(north,cell_north)
!!$                map => master_dofmap%get_master_dofmap(next_cell_north)
!!$                self%dofmap(:,cell_in_stencil,cell) = map
!!$                ! Compute 'north' direction of new cell north
!!$                do direction = 1,4
!!$                  if ( mesh%get_cell_next(direction,next_cell_north) == cell_north) &
!!$                    north = opposite(direction)
!!$                end do
!!$                cell_north = next_cell_north
!!$              end if
!!$            end if
!!$
!!$          end do
!!$        end do
!!$ 
!!$      case default
!!$        write( log_scratch_space, '( A, I4 )' ) &
!!$           "Invalid stencil type: ", st_shape
!!$        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
!!$    end select 
!!$
!!$  end function stencil_dofmap_constructor

!-----------------------------------------------------------------------------
! Get the stencil dofmap for a single cell
!-----------------------------------------------------------------------------
!> Function Returns a pointer to the dofmap for the cell 
!! @param[in] self The calling function_space
!! @param[in] cell Which cell
!! @return The pointer which points to a slice of the dofmap
function get_dofmap(self,cell) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def),                     intent(in) :: cell
  integer(i_def), pointer                        :: map(:,:) 

  map => self%dofmap(:,:,cell)
  return
end function get_dofmap
!-----------------------------------------------------------------------------
! Get the stencil dofmap for the whole mesh
!-----------------------------------------------------------------------------
!> Function Returns a pointer to the dofmap for the mesh 
!! @param[in] self The calling function_space
!! @return The pointer which points to the dofmap
function get_whole_dofmap(self) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                        :: map(:,:,:) 

  map => self%dofmap(:,:,:)
  return
end function get_whole_dofmap

!> Returns the size of the stencil in cells
!! @param[in] self The calling function_space
!! @return The size of the stencil in cells
function get_size(self) result(size)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def)                                 :: size

  size = self%dofmap_size
  return
end function get_size

!> Returns required stencil size in cells for a given stencil shape and extent
!! @param[in] self The calling function_space
!> @param[in] st_shape The shape of the required stencil
!> @param[in] st_extent The extent of the stencil
!! @return The size of the stencil in cells
function compute_dofmap_size(st_shape, st_extent) result(size)
!!$  use log_mod,  only: log_event,         &
!!$                      log_scratch_space, &
!!$                      LOG_LEVEL_ERROR
  implicit none
  integer(i_def),           intent(in) :: st_shape
  integer(i_def),           intent(in) :: st_extent
  integer(i_def)                       :: size

!!$  select case ( st_shape ) 
!!$  case ( STENCIL_POINT )
!!$    size = 1
!!$  case ( STENCIL_1DX, STENCIL_1DY )
!!$    ! Add st_extent cells either side of the centre cell
!!$    size = 1 + 2 * st_extent
!!$  case ( STENCIL_CROSS )
!!$    ! Add st_extent cells either side, and above and below the centre cell
!!$    size = 1 + 4 * st_extent
!!$  case default
!!$    write( log_scratch_space, '( A, I4 )' ) &
!!$       "Invalid stencil type: ", st_shape
!!$    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
!!$  end select
  size = 1
  return
end function compute_dofmap_size


end module stencil_dofmap_mod

