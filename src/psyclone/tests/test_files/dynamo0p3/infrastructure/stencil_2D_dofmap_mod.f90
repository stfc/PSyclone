!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

!> @brief A type which holds information about the dofmap.
!> @details Types for storing the general stencil dofmaps of a general size
!> Inherits from linked_list_data_type so it can be stored in a linked_list
!> Allowable stencil types (the size is variable):
!>
!> \verbatim
!>
!>                  |9|
!>                  |8|
!> CROSS  -->   |3|2|1|6|7|
!>                  |4|
!>                  |5|
!>
!> Stored as:
!>               W S E N
!>              |1|1|1|1|
!>              |2|4|6|8|
!>              |3|5|7|9|
!>
!> \endverbatim

module stencil_2D_dofmap_mod

use constants_mod,                       only: i_def, l_def
use master_dofmap_mod,                   only: master_dofmap_type
use linked_list_data_mod,                only: linked_list_data_type


implicit none

private

type, extends(linked_list_data_type), public :: stencil_2D_dofmap_type
  private
  integer(i_def), allocatable :: dofmap_size(:,:)
  integer(i_def), allocatable :: dofmap(:,:,:,:)
contains
  procedure :: get_dofmap
  procedure :: get_whole_dofmap
  procedure :: get_stencil_sizes

  !> Forced clear of this object from memory.
  !> This routine should not need to be called manually except
  !> (possibly) in pfunit tests
  procedure, public :: clear

  !> Finalizer routine, should be called automatically
  !> by code when the object is out of scope
  final :: stencil_2D_dofmap_destructor

end type stencil_2D_dofmap_type

integer(i_def), public, parameter :: STENCIL_2D_CROSS  = 1600

interface stencil_2D_dofmap_type
  module procedure stencil_2D_dofmap_constructor
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
function stencil_2D_dofmap_constructor(st_shape, st_depth, ndf, mesh, master_dofmap) result(self)

  use log_mod,                             only: log_event,         &
                                                 log_scratch_space, &
                                                 LOG_LEVEL_ERROR
  use mesh_mod,                            only: mesh_type
  use reference_element_mod,               only: W, E, N, S, &
                                                 reference_element_type
  use stencil_dofmap_helper_functions_mod, only: get_stencil_cells, &
                                                 generate_stencil_dofmap_id

  implicit none

  integer(i_def),           intent(in) :: st_shape
  integer(i_def),           intent(in) :: st_depth
  integer(i_def),           intent(in) :: ndf
  type(mesh_type), pointer, intent(in) :: mesh
  type(master_dofmap_type), intent(in) :: master_dofmap
  type(stencil_2D_dofmap_type)         :: self

  integer(i_def) :: cell
  integer(i_def) :: ncells
  integer(i_def), pointer :: map(:) => null()
  integer(i_def) :: cells_in_stencil
  integer(i_def) :: last_halo_index
  integer(i_def) :: stencil_2D_dofmap_id
  integer(i_def) :: i
  integer(i_def), allocatable, dimension(:) :: direction_map
  integer(i_def), allocatable, dimension(:) :: stencil_cells
  integer(i_def) :: number_of_neighbours
  integer(i_def) :: branch_direction

  class(reference_element_type), pointer :: reference_element => null()

  reference_element => mesh%get_reference_element()
  number_of_neighbours = reference_element%get_number_2d_edges()

  ! Since this routine is only valid for quadrilateral elements throw an error
  ! if the number of (horizontal) neighbours is not four
  if ( number_of_neighbours /= 4_i_def ) &
  call log_event( 'Stencil dofmaps only valid for quad elements', LOG_LEVEL_ERROR )

  ! Get direction map for the requested stencil shape
  allocate( direction_map(number_of_neighbours) )
  direction_map = 0

  ! Allocate stencil cells array to be of max possible stencil size
  allocate( stencil_cells(st_depth+1) )

  stencil_2D_dofmap_id = generate_stencil_dofmap_id(st_shape, st_depth)

  ! call base class set_id()
  call self%set_id(stencil_2D_dofmap_id)

  ! How far can stencil be computed before stencil goes off edge of halo region
  last_halo_index = mesh%get_halo_depth() - st_depth
  if (last_halo_index < 0) then
    write( log_scratch_space, '( A, I4, A, I4, A, I4 )' ) &
       'Attempting to create stencil: ', st_shape,' of extent ',st_depth, &
       ' when halo is depth is too small:',mesh%get_halo_depth()
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  else if (last_halo_index == 0) then
    ! Stencil extent same as halo depth, so compute stencil for all owned cells
    ncells = mesh%get_last_edge_cell()
  else
    ! Stencil extent smaller than halo depth, so compute stencil into halo
    ncells = mesh%get_last_halo_cell(last_halo_index)
  end if

  ! Allocate the dofmap array for maximum stencil size
  allocate( self%dofmap( ndf, st_depth+1, number_of_neighbours, ncells ) )
  ! Allocate array for storing individual stencil sizes
  allocate( self%dofmap_size(number_of_neighbours, ncells) )

  ! Loop over all cells in mesh and calculate stencil dofmap for each
  do cell = 1, ncells
    do branch_direction = 1, number_of_neighbours
      direction_map = 0
      direction_map(branch_direction) = st_depth
      ! Set default value for stencil cells
      stencil_cells = -1_i_def
      ! Add current cell to stencil and update number of cells in stencil
      stencil_cells(1) = cell
      cells_in_stencil = 1
      ! Call subroutine to get the rest of the cells in the stencil
      call get_stencil_cells( mesh, cell, st_depth, number_of_neighbours, &
                              direction_map, cells_in_stencil, &
                              stencil_cells )
      ! Set actual stencil size for cell
      self%dofmap_size(branch_direction, cell) = cells_in_stencil

      ! Create pointer to master dofmap of each cell in stencil and add to
      ! local stencil dofmap
      do i=1, cells_in_stencil
        map => master_dofmap%get_master_dofmap(stencil_cells(i))
        self%dofmap(:,i,branch_direction,cell) = map(:)
      end do
    end do
  end do

  ! Clean up arrays and pointers
  deallocate( direction_map, stencil_cells )
  nullify(map, reference_element)

  end function stencil_2D_dofmap_constructor

!-----------------------------------------------------------------------------
! Get the stencil dofmap for a single cell
!-----------------------------------------------------------------------------
!> Function Returns a pointer to the dofmap for the cell
!! @param[in] self The calling function_space
!! @param[in] cell Which cell
!! @return The pointer which points to a slice of the dofmap
function get_dofmap(self,cell) result(map)
  implicit none
  class(stencil_2D_dofmap_type), target, intent(in) :: self
  integer(i_def),                        intent(in) :: cell
  integer(i_def), pointer                           :: map(:,:,:)

  map => self%dofmap(:,:,:,cell)

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
  class(stencil_2D_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                           :: map(:,:,:,:)

  map => self%dofmap(:,:,:,:)

  return
end function get_whole_dofmap


!-----------------------------------------------------------------------------
! Get the local stencil sizes
!-----------------------------------------------------------------------------
!> Returns a pointer to the stencil local_size array
!! @param[in] self The calling space function
!! @return Pointer to the array of local stencil sizes
!==============================================================================
function get_stencil_sizes(self) result(stencil_sizes)

  implicit none

  class(stencil_2D_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                           :: stencil_sizes(:,:)

  stencil_sizes => self%dofmap_size(:,:)

  return
end function get_stencil_sizes

!==============================================================================
!> @brief Routine to destroy object.
!> @param[in] self, The calling object type
subroutine clear(self)

  implicit none

  class (stencil_2D_dofmap_type), intent(inout) :: self

  if (allocated(self%dofmap)) deallocate( self%dofmap)

  return
end subroutine clear

!==============================================================================
!> @brief Finalizer routine which should automatically call clear
!>        when object is out of scope.
!> @param[in] self, The calling object_type
subroutine stencil_2D_dofmap_destructor(self)

  implicit none

  type (stencil_2D_dofmap_type), intent(inout) :: self

  call self%clear()

  return
end subroutine stencil_2D_dofmap_destructor

end module stencil_2D_dofmap_mod