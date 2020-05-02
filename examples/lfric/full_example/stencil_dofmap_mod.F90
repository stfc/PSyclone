!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
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
!>          |9|5|8|
!> REGION-> |2|1|4|
!>          |6|3|7|

module stencil_dofmap_mod

use constants_mod,        only: i_def, l_def
use master_dofmap_mod,    only: master_dofmap_type
use linked_list_data_mod, only: linked_list_data_type


implicit none

private
public :: generate_stencil_dofmap_id

type, extends(linked_list_data_type), public :: stencil_dofmap_type
  private
  integer(i_def) :: dofmap_shape
  integer(i_def) :: dofmap_extent
  integer(i_def) :: dofmap_size
  integer(i_def), allocatable :: local_size(:)
  integer(i_def), allocatable :: dofmap(:,:,:)
contains
  procedure :: get_dofmap
  procedure :: get_whole_dofmap
  procedure :: get_size
  procedure :: get_local_size
  procedure :: get_stencil_sizes

  !> Forced clear of this object from memory.
  !> This routine should not need to be called manually except
  !> (possibly) in pfunit tests
  procedure, public :: clear

  !> Finalizer routine, should be called automatically
  !> by code when the object is out of scope
  final :: stencil_dofmap_destructor

end type stencil_dofmap_type

integer(i_def), public, parameter :: STENCIL_POINT  = 1100
integer(i_def), public, parameter :: STENCIL_1DX    = 1200
integer(i_def), public, parameter :: STENCIL_1DY    = 1300
integer(i_def), public, parameter :: STENCIL_CROSS  = 1400
integer(i_def), public, parameter :: STENCIL_REGION = 1500

interface stencil_dofmap_type
  module procedure stencil_dofmap_constructor
end interface

contains

!-----------------------------------------------------------------------------
! Construct the stencil dofmap
!-----------------------------------------------------------------------------
!> Function to construct a stencil dofmap
!> @param[in] st_shape The shape of the required stencil
!> @param[in] st_depth Parameter to control how far the stencil is computed to
!> @param[in] ndf Number of degrees of freedom per cell for this function space
!> @param[in] mesh Object to build the stencil on
!> @param[in] master_dofmap The cell dofmap to create the stencil from
!> @return The dofmap object
function stencil_dofmap_constructor( st_shape, st_depth, ndf, mesh, master_dofmap) result(self)

    use log_mod,               only: log_event,         &
                                     log_scratch_space, &
                                     LOG_LEVEL_ERROR
    use mesh_mod,              only: mesh_type
    use reference_element_mod, only: W, E, N, S, &
                                     reference_element_type

    implicit none

    integer(i_def),           intent(in) :: st_shape, st_depth, ndf
    type(mesh_type), pointer, intent(in) :: mesh
    type(master_dofmap_type), intent(in) :: master_dofmap
    type(stencil_dofmap_type)            :: self

    integer(i_def) :: cell, ncells
    integer(i_def) :: st_size
    integer(i_def), pointer :: map(:) => null()
    integer(i_def) :: cell_in_stencil
    integer(i_def) :: cell_west,  next_cell_west,  &
                      cell_south, next_cell_south, &
                      cell_east,  next_cell_east,  &
                      cell_north, next_cell_north
    integer(i_def) :: west, north, east, south
    integer(i_def) :: direction
    integer(i_def) :: last_halo_index
    integer(i_def) :: stencil_dofmap_id
    integer(i_def) :: ncells_in_stencil
    integer(i_def) :: ij
    integer(i_def) :: cell1
    integer(i_def) :: cell2
    integer(i_def) :: nlist
    integer(i_def) :: c1
    integer(i_def) :: c2
    integer(i_def) :: minimum_stencil_size
    integer(i_def), allocatable, dimension(:) :: cell_in_region, &
                                                 list, opposite
    integer(i_def) :: st_extent
    integer(i_def) :: number_of_neighbours
    logical(l_def) :: found, alreadylist, alreadysten

    class(reference_element_type), pointer :: reference_element => null()

    reference_element => mesh%get_reference_element()
    number_of_neighbours = reference_element%get_number_2d_edges()
    ! Since this routine is only valid for quadrilateral elements throw an error
    ! if the number of (horizontal) neighbours is not four
    if ( number_of_neighbours /= 4_i_def ) &
     call log_event( 'Stencil dofmaps only valid for quad elements', LOG_LEVEL_ERROR )

    allocate( opposite(number_of_neighbours) )
    ! Set local directions to be those of the reference element
    opposite(W) = E
    opposite(E) = W
    opposite(N) = S
    opposite(S) = N

    st_size = compute_dofmap_size(st_shape, st_depth, number_of_neighbours)

    ! Compute the number of cells away from the central cell
    ! that the stencil will reach to, this will determine the needed
    ! halo size
    if ( st_shape == STENCIL_REGION ) then
      ! Since region stencils pass in the order as the depth variable the extent
      ! is half of this value
      st_extent = st_depth/2
    else
      st_extent = st_depth
    end if

    self%dofmap_shape  = st_shape
    self%dofmap_extent = st_extent
    self%dofmap_size   = st_size

    stencil_dofmap_id = generate_stencil_dofmap_id(st_shape, st_extent)

    ! call base class set_id()
    call self%set_id(stencil_dofmap_id)

    ! How far can stencil be computed before stencil goes off edge of halo region
    last_halo_index = mesh%get_halo_depth() - st_extent
    if (last_halo_index < 0) then
      write( log_scratch_space, '( A, I4, A, I4, A, I4 )' ) &
         'Attempting to create stencil: ', st_shape,' of extent ',st_extent, &
         ' when halo is depth is too small:',mesh%get_halo_depth()
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    else if (last_halo_index == 0) then
      ! Stencil extent same as halo depth, so compute stencil for all owned cells
      ncells = mesh%get_last_edge_cell()
    else
      ! Stencil extent smaller than halo depth, so compute stencil into halo
      ncells = mesh%get_last_halo_cell(last_halo_index)
    end if

    ! Allocate the dofmap array
    allocate( self%dofmap( ndf, self%dofmap_size, ncells ) )

    ! Compute the dofmap
    select case ( st_shape )
      case ( STENCIL_POINT )
        do cell = 1,ncells
          map => master_dofmap%get_master_dofmap(cell)
          self%dofmap(:,1,cell) = map(:)
        end do
      case ( STENCIL_CROSS, STENCIL_1DX, STENCIL_1DY )
        do cell = 1,ncells
          map => master_dofmap%get_master_dofmap(cell)
          self%dofmap(:,1,cell) = map(:)
          cell_in_stencil = 1
          cell_west  = cell
          cell_south = cell
          cell_east  = cell
          cell_north = cell
          west  = W
          east  = E
          north = N
          south = S
          do while ( cell_in_stencil <= st_size )
            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DX ) then
              cell_in_stencil = cell_in_stencil + 1
              if (  cell_in_stencil <= st_size ) then
                next_cell_west = mesh%get_cell_next(west,cell_west)
                map => master_dofmap%get_master_dofmap(next_cell_west)
                self%dofmap(:,cell_in_stencil,cell) = map
                ! Compute 'west' direction of new cell west
                do direction = 1, number_of_neighbours
                  if ( mesh%get_cell_next(direction,next_cell_west) == cell_west) &
                    west = opposite(direction)
                end do
                cell_west = next_cell_west
              end if
            end if

            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DY ) then
              cell_in_stencil = cell_in_stencil + 1
              if (  cell_in_stencil <= st_size ) then
                next_cell_south = mesh%get_cell_next(south,cell_south)
                map => master_dofmap%get_master_dofmap(next_cell_south)
                self%dofmap(:,cell_in_stencil,cell) = map
                ! Compute 'south' direction of new cell south
                do direction = 1, number_of_neighbours
                  if ( mesh%get_cell_next(direction,next_cell_south) == cell_south) &
                    south = opposite(direction)
                end do
                cell_south = next_cell_south
              end if
            end if

            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DX ) then
              cell_in_stencil = cell_in_stencil + 1
              if (  cell_in_stencil <= st_size ) then
                next_cell_east = mesh%get_cell_next(east,cell_east)
                map => master_dofmap%get_master_dofmap(next_cell_east)
                self%dofmap(:,cell_in_stencil,cell) = map
                ! Compute 'east' direction of new cell east
                do direction = 1, number_of_neighbours
                  if ( mesh%get_cell_next(direction,next_cell_east) == cell_east) &
                    east = opposite(direction)
                end do
                cell_east = next_cell_east
              end if
            end if

            if ( st_shape ==  STENCIL_CROSS .or. st_shape == STENCIL_1DY ) then
              cell_in_stencil = cell_in_stencil + 1
              if (  cell_in_stencil <= st_size ) then
                next_cell_north = mesh%get_cell_next(north,cell_north)
                map => master_dofmap%get_master_dofmap(next_cell_north)
                self%dofmap(:,cell_in_stencil,cell) = map
                ! Compute 'north' direction of new cell north
                do direction = 1, number_of_neighbours
                  if ( mesh%get_cell_next(direction,next_cell_north) == cell_north) &
                    north = opposite(direction)
                end do
                cell_north = next_cell_north
              end if
            end if

          end do
        end do

      case ( STENCIL_REGION )
        allocate( self%local_size(ncells) )
        allocate( cell_in_region(self%dofmap_size), &
                  list(self%dofmap_size) )
        minimum_stencil_size = (st_depth + 1_i_def)*(st_depth + 2_i_def)/2_i_def
        do cell = 1, ncells
          ! Not all stencils are the same size so initialise stencil to a
          ! negative value that should give an error if it is ever used
          self%dofmap(:,:,cell) = -1_i_def

          ! Put the cell itself in the first entry
          cell_in_region(1_i_def) = cell
          ncells_in_stencil = 1_i_def
          ! Iteratively expand stencil until the number of cells
          ! is less then the maximum number of cells allowed
          do while (ncells_in_stencil < minimum_stencil_size)

            ! And look for some new ones
            nlist = 0_i_def
            list(:) = 0_i_def

            ! Find neighbours of cells currently in the stencil
            do c1 = 1, ncells_in_stencil
              cell1 = cell_in_region(c1)
              do direction = 1, number_of_neighbours
                cell2 = mesh%get_cell_next(direction,cell1)

                ! Is it already in the stencil?
                alreadysten = .false.
                do c2 = 1, ncells_in_stencil
                  if (cell2 == cell_in_region(c2)) alreadysten = .true.
                end do

                if (.not. alreadysten) then
                  ! If it's already on the temporary list, flag the fact that
                  ! we've seen it more than once
                  alreadylist = .false.
                  do c2 = 1, nlist
                    if (cell2 == abs(list(c2))) then
                      ! It's already on the list; make a note, and store as
                      ! a positive number
                        alreadylist = .true.
                        list(c2) = cell2
                    end if
                  end do
                  if (.not. alreadylist) then
                    ! It's not already on the list; add it as a negative number
                    ! to indicate this is the first time
                    nlist = nlist + 1_i_def
                    list(nlist) = -cell2
                  end if
                end if

              end do
            end do

            ! If we found any that are neighbours more than once then take them
            found = .false.
            do c2 = 1, nlist
              if (list(c2) > 0) then
                ncells_in_stencil = ncells_in_stencil + 1
                cell_in_region(ncells_in_stencil) = list(c2)
                found = .true.
              end if
            end do

            ! Otherwise, take those that are neighbours just once
            if (.not. found) then
              do c2 = 1, nlist
                ncells_in_stencil = ncells_in_stencil + 1
                cell_in_region(ncells_in_stencil) = abs(list(c2))
              end do
            end if
          end do
          ! Copy all the dofmaps of the cells in the stencil into the stencil
          ! map
          do cell_in_stencil = 1, ncells_in_stencil
           ij = cell_in_region(cell_in_stencil)
           map => master_dofmap%get_master_dofmap( ij )
           self%dofmap(:,cell_in_stencil,cell) = map(:)
          end do
          self%local_size(cell) = ncells_in_stencil

        end do
        deallocate( cell_in_region , list )
      case default
        write( log_scratch_space, '( A, I4 )' ) &
           "Invalid stencil type: ", st_shape
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end select

    deallocate( opposite )
    nullify(map, reference_element)

  end function stencil_dofmap_constructor

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
function get_size(self) result(stencil_size)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def)                                 :: stencil_size

  stencil_size = self%dofmap_size
  return
end function get_size

!> Returns the size of the stencil in cells for this cell
!! @param[in] self The calling function_space
!! @param[in] cell The cell at the centre of the stencil
!! @return The size of the stencil in cells
function get_local_size(self, cell) result(stencil_size)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def),                     intent(in) :: cell
  integer(i_def)                                 :: stencil_size

  if ( self%dofmap_shape == STENCIL_REGION ) then
    stencil_size = self%local_size(cell)
  else
    stencil_size = self%dofmap_size
  end if
  return
end function get_local_size

!-----------------------------------------------------------------------------
! Get the local stencil sizes
!-----------------------------------------------------------------------------
!> Returns a pointer to the stencil local_size array
!! @param[in] self The calling space function
!! @return Pointer to the array of local stencil sizes
! TODO - Ticket #2019 Design discussion on how stencil sizes are passed to
!        kernels.
function get_stencil_sizes(self) result(stencil_sizes)
  use log_mod,                only: log_event, &
                                    log_scratch_space, &
                                    LOG_LEVEL_ERROR
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                        :: stencil_sizes(:)

  if (self%dofmap_shape == STENCIL_REGION) then
    stencil_sizes => self%local_size(:)
  else
    write( log_scratch_space, '( A, I4 )' ) &
    "Stencil sizes array not available for stencil ", self%dofmap_shape
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if
  return
end function get_stencil_sizes

!> Returns required stencil size in cells for a given stencil shape and extent
!! @param[in] self The calling function_space
!> @param[in] st_shape The shape of the required stencil
!> @param[in] st_extent The extent of the stencil
!> @param[in] number_of_neighbours Number of neighbouring cells for each cell
!! @return The size of the stencil in cells
function compute_dofmap_size(st_shape, st_extent, number_of_neighbours) result(stencil_size)
  use log_mod,  only: log_event,         &
                      log_scratch_space, &
                      LOG_LEVEL_ERROR
  implicit none
  integer(i_def),           intent(in) :: st_shape
  integer(i_def),           intent(in) :: st_extent
  integer(i_def),           intent(in) :: number_of_neighbours
  integer(i_def)                       :: stencil_size
  integer(i_def)                       :: s

  select case ( st_shape )
  case ( STENCIL_POINT )
    stencil_size = 1
  case ( STENCIL_1DX, STENCIL_1DY )
    ! Add st_extent cells either side of the centre cell
    stencil_size = 1 + 2 * st_extent
  case ( STENCIL_CROSS )
    ! Add st_extent cells either side, and above and below the centre cell
    stencil_size = 1 + number_of_neighbours * st_extent
  case ( STENCIL_REGION )
    ! This is the maximum size of the stencil, some stencils (such as near
    ! corners will be smaller
    ! Build up size iteratively, alternatively adding on 4 cells (if we are
    ! filling in the corners of a square stencil)
    ! or 4*s (if we are extending the stencil to all cells that have a neighbour
    ! not in the stencil)
    stencil_size = 1
    do s = 1, st_extent
      if ( mod(s,2) == 0 ) then
        stencil_size = stencil_size + number_of_neighbours
      else
        stencil_size = stencil_size + number_of_neighbours*s
      end if
    end do

  case default
    write( log_scratch_space, '( A, I4 )' ) &
       "Invalid stencil type: ", st_shape
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end function compute_dofmap_size

!> Returns a stencil dofmap id using stencil shape and extent
!> @param[in] stencil_shape   Shape code of stencil
!> @param[in] stencil_extent  Extent of stencil
!> @return    stencil_id
!==============================================================================
function generate_stencil_dofmap_id( stencil_shape,   &
                                     stencil_extent ) &
                                     result( stencil_id )

  implicit none

  integer(i_def), intent(in) :: stencil_shape
  integer(i_def), intent(in) :: stencil_extent

  integer(i_def) :: stencil_id

  stencil_id = stencil_shape*100 + stencil_extent

  return
end function generate_stencil_dofmap_id

!==============================================================================
!> @brief Routine to destroy object.
!> @param[in] self, The calling object type
subroutine clear(self)

  implicit none

  class (stencil_dofmap_type), intent(inout) :: self

  if (allocated(self%dofmap)) deallocate( self%dofmap)

  return
end subroutine clear

!==============================================================================
!> @brief Finalizer routine which should automatically call clear
!>        when object is out of scope.
!> @param[in] self, The calling object_type
subroutine stencil_dofmap_destructor(self)

  implicit none

  type (stencil_dofmap_type), intent(inout) :: self

  call self%clear()

  return
end subroutine stencil_dofmap_destructor

end module stencil_dofmap_mod

