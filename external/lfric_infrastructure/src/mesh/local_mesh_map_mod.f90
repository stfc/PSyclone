!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief   Object to hold intermesh connectivity between two local mesh
!>          objects.
!> @details Cell mappings are from LID of a cell in the source mesh to LIDs of
!>          overlapping cells in the target local mesh object.
module local_mesh_map_mod

  use constants_mod,        only: i_def, imdi, l_def
  use log_mod,              only: log_event, log_scratch_space, &
                                  LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use linked_list_data_mod, only: linked_list_data_type

  implicit none

  private
  public :: generate_local_mesh_map_id

  !----------------------------------------------------------------------------
  type, extends(linked_list_data_type), public :: local_mesh_map_type
    private
    integer(i_def), allocatable :: local_mesh_map(:,:,:)
  contains
    procedure, public :: get_source_id
    procedure, public :: get_target_id
    procedure, public :: get_nsource_cells
    procedure, public :: get_ntarget_cells_per_source_cell_x
    procedure, public :: get_ntarget_cells_per_source_cell_y
    procedure, public :: get_selected_cell_map
    procedure, public :: get_whole_cell_map
    generic           :: get_cell_map => &
                            get_selected_cell_map, &
                            get_whole_cell_map
    procedure, public :: clear
    final             :: local_mesh_map_destructor
  end type local_mesh_map_type

  interface local_mesh_map_type
    module procedure local_mesh_map_constructor
  end interface
  !----------------------------------------------------------------------------

contains

  !============================================================================
  !> @brief     Constructor for local mesh map object
  !> @param[in] source_local_mesh_id  ID of source local mesh object
  !> @param[in] target_local_mesh_id  ID of target local mesh object
  !> @param[in] map[:::]   local cell ids in target local mesh object which
  !>                       overlap specified cells in the source local mesh
  !>                       object.
  !> @return    local_mesh_map_type
  function local_mesh_map_constructor ( source_local_mesh_id, &
                                        target_local_mesh_id, &
                                        map )                 &
                                             result ( self )

    implicit none

    integer(i_def), intent(in) :: source_local_mesh_id
    integer(i_def), intent(in) :: target_local_mesh_id
    integer(i_def), intent(in) :: map(:,:,:)

    type(local_mesh_map_type)  :: self

    integer(i_def) :: local_mesh_map_id
    integer(i_def) :: ntarget_cells_per_source_cell_x
    integer(i_def) :: ntarget_cells_per_source_cell_y
    integer(i_def) :: nsource_cells

    if (source_local_mesh_id == target_local_mesh_id) then
      write(log_scratch_space, '(A)') &
          'local mesh ids are the same nothing to do.'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)
      call self%set_id(imdi)
      return
    end if

    ! Set the local mesh map id
    !-------------------------------------------------
    local_mesh_map_id = generate_local_mesh_map_id( source_local_mesh_id, &
                                                    target_local_mesh_id )
    call self%set_id(local_mesh_map_id)

    ntarget_cells_per_source_cell_x = size(map,1)
    ntarget_cells_per_source_cell_y = size(map,2)
    nsource_cells                   = size(map,3)

    ! Populate mesh map
    ! -----------------------------------------
    allocate( self%local_mesh_map( ntarget_cells_per_source_cell_x, &
                                   ntarget_cells_per_source_cell_y, &
                                   nsource_cells ) )
    self%local_mesh_map = map

    return
  end function local_mesh_map_constructor

  !============================================================================
  !> @brief  Gets the ids of target cells mapped to given source cells lids
  !> @param [in]  cell_ids[:]  Source cell local ids which
  !>                           require mapped target cell local ids
  !> @param [out] lid_map[:::] Integer array of target cell local ids which
  !>                           map to the requested cell_ids in the source
  !>                           local mesh object.
  subroutine get_selected_cell_map(self, cell_ids, lid_map)

    implicit none

    class(local_mesh_map_type), intent(in) :: self

    integer(i_def), intent(in)  :: cell_ids(:)
    integer(i_def), intent(out) :: lid_map(:,:,:)

    integer(i_def) :: i
    integer(i_def) :: ncells_request

    ncells_request = size(cell_ids)

    if ((size(lid_map,1) /= size(self%local_mesh_map,1)) .or. &
        (size(lid_map,2) /= size(self%local_mesh_map,2)) .or. &
        (size(lid_map,3) /= ncells_request)) then
       write(log_scratch_space, '(3(A,I0),A)')                      &
          'Output array dimensions are incorrect, dimensions of (', &
           size(self%local_mesh_map,1), ',', size(self%local_mesh_map,2), ',', &
           ncells_request,') required.'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      return
    end if

    do i=1, ncells_request
      lid_map(:,:,i) = self%local_mesh_map(:,:,cell_ids(i))
    end do

    return
  end subroutine get_selected_cell_map

  !============================================================================
  !> @brief  Gets the ids of target cells mapped to all source cells lids
  !> @param [out] lid_map[:::] Integer array of target cell local ids which
  !>                           map to the source local mesh object.
  subroutine get_whole_cell_map(self, lid_map)

    implicit none

    class(local_mesh_map_type), intent(in) :: self

    integer(i_def), intent(out) :: lid_map(:,:,:)

    if ((size(lid_map,1) /= size(self%local_mesh_map,1)) .or. &
        (size(lid_map,2) /= size(self%local_mesh_map,2)) .or. &
        (size(lid_map,3) /= size(self%local_mesh_map,3))) then
       write(log_scratch_space, '(3(A,I0),A)')                      &
          'Output array dimensions are incorrect, dimensions of (', &
           size(self%local_mesh_map,1), ',', size(self%local_mesh_map,2), &
           ',', size(self%local_mesh_map,3),') required.'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      return
    end if

    lid_map(:,:,:) = self%local_mesh_map(:,:,:)

  end subroutine get_whole_cell_map

  !============================================================================
  !> @brief  Returns the ID of the local mesh object used as the source
  !>         for this local mesh map object.
  !> @return ID of source local mesh object
  function get_source_id(self) result (source_local_mesh_id)

    implicit none

    class(local_mesh_map_type), intent(in) :: self
    integer(i_def) :: source_local_mesh_id, local_mesh_map_id

    local_mesh_map_id = self%get_id()
    source_local_mesh_id = floor(real(local_mesh_map_id/1000))

  end function get_source_id

  !============================================================================
  !> @brief  Returns the ID of the local mesh object used as the target
  !>         for this local mesh map object.
  !> @return ID of target local mesh object
  function get_target_id(self) result (target_local_mesh_id)

    implicit none

    class(local_mesh_map_type), intent(in) :: self
    integer(i_def) :: target_local_mesh_id, local_mesh_map_id

    local_mesh_map_id = self%get_id()
    target_local_mesh_id = local_mesh_map_id - self%get_source_id()*1000

  end function get_target_id

  !============================================================================
  !> @brief  Returns the number of target cells in the "x-direction" for each
  !>         source cell in this mapping object.
  !> @return Number of target cells per source cell in the "x-direction"
  function get_ntarget_cells_per_source_cell_x(self) &
                                     result (ntarget_cells_per_source_cell_x)

    implicit none

    class(local_mesh_map_type), intent(in) :: self
    integer(i_def) :: ntarget_cells_per_source_cell_x

    ntarget_cells_per_source_cell_x = size(self%local_mesh_map,1)

  end function get_ntarget_cells_per_source_cell_x

  !============================================================================
  !> @brief  Returns the number of target cells in the "y-direction" for each
  !>         source cell in this mapping object.
  !> @return Number of target cells per source cell in the "y-direction"
  function get_ntarget_cells_per_source_cell_y(self) &
                                     result (ntarget_cells_per_source_cell_y)

    implicit none

    class(local_mesh_map_type), intent(in) :: self
    integer(i_def) :: ntarget_cells_per_source_cell_y

    ntarget_cells_per_source_cell_y = size(self%local_mesh_map,2)

  end function get_ntarget_cells_per_source_cell_y

  !============================================================================
  !> @brief  Returns the number of source cells in this mapping object.
  !> @return Number of source cells in this mapping object
  function get_nsource_cells(self) result (nsource_cells)

    implicit none

    class(local_mesh_map_type), intent(in) :: self
    integer(i_def) :: nsource_cells

    nsource_cells = size(self%local_mesh_map,3)

  end function get_nsource_cells

  !============================================================================
  !> @brief Forced clear of this oject from memory.
  !>        This routine should not need to be called manually except
  !>        (possibly) in pfunit tests
  subroutine clear(self)

    implicit none

    class(local_mesh_map_type), intent(inout) :: self

    if ( allocated(self%local_mesh_map) ) deallocate(self%local_mesh_map)

  end subroutine clear

  !============================================================================
  !> @brief Finalizer routine, should be called automatically by
  !>        code when the object is out of scope
  subroutine local_mesh_map_destructor(self)

    implicit none

    type(local_mesh_map_type), intent(inout) :: self

    call self%clear()

  end subroutine local_mesh_map_destructor

!> Returns a mesh map id using the ids of source and target meshes.
!> @param[in] source_mesh_id  ID of source mesh object
!> @param[in] target_mesh_id  ID of target mesh object
!> @return    mesh_map_id
!==============================================================================
function generate_local_mesh_map_id( source_local_mesh_id,  &
                                      target_local_mesh_id ) &
                              result( local_mesh_map_id )

implicit none

integer(i_def), intent(in) :: source_local_mesh_id
integer(i_def), intent(in) :: target_local_mesh_id

integer(i_def) :: local_mesh_map_id

local_mesh_map_id = 1000*source_local_mesh_id + target_local_mesh_id

return
end function generate_local_mesh_map_id

end module local_mesh_map_mod

