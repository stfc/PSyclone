!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> @brief   Object to hold intermesh connectivity between two global mesh
!>          objects.
!> @details Cell mappings are from GID of a cell in the source mesh to GIDs of
!>          overlapping cells in the target global mesh object.
module global_mesh_map_mod

  use constants_mod,        only: i_def, imdi, l_def
  use log_mod,              only: log_event, log_scratch_space, &
                                  LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use linked_list_data_mod, only: linked_list_data_type

  implicit none

  private
  public :: generate_global_mesh_map_id

  !----------------------------------------------------------------------------
  type, extends(linked_list_data_type), public :: global_mesh_map_type
    private

    integer(i_def), allocatable :: global_mesh_map(:,:,:)

  contains

    !> @brief  Returns the ID of the global mesh object used as the source
    !>         for this global mesh map object.
    !> @return ID of source global mesh object
    procedure, public :: get_source_id
    !>
    !> @brief  Returns the ID of the global mesh object used as the target
    !>         for this global mesh map object.
    !> @return ID of target global mesh object
    procedure, public :: get_target_id
    !>
    !> @brief  Returns the number of source cells in this mapping object.
    !> @return Number of source cells in this mapping object
    procedure, public :: get_nsource_cells
    !>
    !> @brief  Returns the number of target cells for each source cell in
    !>         this mapping object.
    !> @return Number of target cells per source cell
    procedure, public :: get_ntarget_cells_per_source_cell
    !>
    !> @brief  Returns the number of target cells for each source cell in
    !>         the x-direction of this mapping object.
    !> @return Number of target cells per source cell in x direction
    procedure, public :: get_ntarget_cells_per_source_x
    !>
    !> @brief  Returns the number of target cells for each source cell in
    !>         the y-direction of this mapping object.
    !> @return Number of target cells per source cell in y-direction
    procedure, public :: get_ntarget_cells_per_source_y

    !>
    !> @brief  Gets the ids of target cells mapped to given source cells lids
    !> @param [in]  cell_ids[:]  Source cell global ids which
    !>                           require mapped target cell global ids
    !> @param [out] gid_map[:::] Integer array of target cell global ids which
    !>                           map to the requested cell_ids in the source
    !>                           global mesh object.
    procedure, public :: get_selected_cell_map

    !>
    !> @brief  Gets the full cell map of all source to target ids
    !> @param [out] gid_map[:::] Integer array of target cell global ids which
    !>                           map to all cell_ids in the source
    !>                           global mesh object.
    procedure, public :: get_full_cell_map

    generic,   public :: get_cell_map => get_selected_cell_map, &
                                         get_full_cell_map

    !>
    !> @brief Forced clear of this object from memory.
    !>        This routine should not need to be called manually except
    !>        (possibly) in pfunit tests
    procedure, public :: clear

    !> @brief Finalizer routine, should be called automatically by
    !>        code when the object is out of scope
    final             :: global_mesh_map_destructor


  end type global_mesh_map_type

  interface global_mesh_map_type
    module procedure global_mesh_map_constructor
  end interface
  !----------------------------------------------------------------------------

contains

  !>
  !> @brief     Constructor for global mesh map object
  !> @param[in] source_global_mesh_id  ID of source global mesh object
  !> @param[in] target_global_mesh_id  ID of target global mesh object
  !> @param[in] map        Global cell ids in target global mesh object which
  !>                       overlap specified cells in the source global mesh
  !>                       object.
  !> @return    global_mesh_map_type
  !============================================================================
  function global_mesh_map_constructor ( source_global_mesh_id, &
                                         target_global_mesh_id, &
                                         map )                  &
                                result ( instance )

    implicit none

    integer(i_def), intent(in) :: source_global_mesh_id
    integer(i_def), intent(in) :: target_global_mesh_id
    integer(i_def), intent(in) :: map(:,:,:)

    type(global_mesh_map_type) :: instance

    integer(i_def) :: global_mesh_map_id
    integer(i_def) :: ntarget_cells_per_source_cell_x
    integer(i_def) :: ntarget_cells_per_source_cell_y
    integer(i_def) :: nsource_cells

    if (source_global_mesh_id == target_global_mesh_id) then
      write(log_scratch_space, '(A)') &
          'Global mesh ids are the same nothing to do.'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)
      call instance%set_id(imdi)
      return
    end if

    ! Set the global mesh map id
    !-------------------------------------------------
    global_mesh_map_id = generate_global_mesh_map_id( source_global_mesh_id, &
                                                      target_global_mesh_id )
    call instance%set_id(global_mesh_map_id)

    ntarget_cells_per_source_cell_x = size(map,1)
    ntarget_cells_per_source_cell_y = size(map,2)
    nsource_cells        = size(map,3)

    ! Populate instance
    ! -----------------------------------------
    allocate( instance%global_mesh_map( ntarget_cells_per_source_cell_x, &
                                        ntarget_cells_per_source_cell_y, &
                                        nsource_cells ) )

    instance%global_mesh_map = map

    return
  end function global_mesh_map_constructor



  subroutine get_selected_cell_map(self, cell_ids, gid_map)

    implicit none

    class(global_mesh_map_type), intent(in) :: self

    integer(i_def), intent(in)  :: cell_ids(:)
    integer(i_def), intent(out) :: gid_map(:,:,:)

    integer(i_def) :: i
    integer(i_def) :: ncells_request

    ncells_request = size(cell_ids)

    if ((size(gid_map,1) /= size(self%global_mesh_map,1)) .or. &
        (size(gid_map,2) /= size(self%global_mesh_map,2)) .or. &
        (size(gid_map,3) /= ncells_request)) then
       write(log_scratch_space, '(2(A,I0),A)')                      &
          'Output array dimensions are incorrect, dimensions of (', &
           size(self%global_mesh_map,1), ',',                       &
           size(self%global_mesh_map,2), ',', ncells_request,       &
           ') required.'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      return
    end if

    do i=1, ncells_request
      gid_map(:,:,i) = self%global_mesh_map(:,:,cell_ids(i))
    end do

    return
  end subroutine get_selected_cell_map


  subroutine get_full_cell_map(self, cell_map)

    implicit none

    class(global_mesh_map_type), intent(in)  :: self
    integer(i_def), allocatable, intent(out) :: cell_map(:,:,:)

    if (allocated(cell_map)) deallocate (cell_map)
    allocate(cell_map, source=self%global_mesh_map)

  end subroutine get_full_cell_map


  function get_source_id(self) result (source_global_mesh_id)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: source_global_mesh_id, global_mesh_map_id

    global_mesh_map_id = self%get_id()
    source_global_mesh_id = floor(real(global_mesh_map_id/1000))

  end function get_source_id

  function get_target_id(self) result (target_global_mesh_id)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: target_global_mesh_id, global_mesh_map_id

    global_mesh_map_id = self%get_id()
    target_global_mesh_id = global_mesh_map_id - self%get_source_id()*1000

  end function get_target_id

  function get_ntarget_cells_per_source_cell(self) &
                                     result (ntarget_cells_per_source_cell)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: ntarget_cells_per_source_cell
    integer(i_def) :: ntarget_cells_per_source_x
    integer(i_def) :: ntarget_cells_per_source_y

    ntarget_cells_per_source_x = size(self%global_mesh_map,1)
    ntarget_cells_per_source_y = size(self%global_mesh_map,2)
    ntarget_cells_per_source_cell = ntarget_cells_per_source_x * ntarget_cells_per_source_y

  end function get_ntarget_cells_per_source_cell

  function get_ntarget_cells_per_source_x(self) &
                                       result (ntarget_cells_per_source_x)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: ntarget_cells_per_source_x

    ntarget_cells_per_source_x = size(self%global_mesh_map,1)

  end function get_ntarget_cells_per_source_x

  function get_ntarget_cells_per_source_y(self) &
                                       result (ntarget_cells_per_source_y)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: ntarget_cells_per_source_y

    ntarget_cells_per_source_y = size(self%global_mesh_map,2)

  end function get_ntarget_cells_per_source_y

  function get_nsource_cells(self) result (nsource_cells)

    implicit none

    class(global_mesh_map_type), intent(in) :: self
    integer(i_def) :: nsource_cells

    nsource_cells = size(self%global_mesh_map,3)

  end function get_nsource_cells


  !> Clear all allocated components
  subroutine clear(self)

    implicit none

    class(global_mesh_map_type), intent(inout) :: self

    if ( allocated(self%global_mesh_map) ) deallocate(self%global_mesh_map)

  end subroutine clear


  ! Global mesh map destructor
  subroutine global_mesh_map_destructor(self)

    implicit none

    type(global_mesh_map_type), intent(inout) :: self

    call self%clear()

  end subroutine global_mesh_map_destructor

!> Returns a mesh map id using the ids of source and target meshes.
!> @param[in] source_mesh_id  ID of source mesh object
!> @param[in] target_mesh_id  ID of target mesh object
!> @return    mesh_map_id
!==============================================================================
function generate_global_mesh_map_id( source_global_mesh_id,  &
                                      target_global_mesh_id ) &
                              result( global_mesh_map_id )

implicit none

integer(i_def), intent(in) :: source_global_mesh_id
integer(i_def), intent(in) :: target_global_mesh_id

integer(i_def) :: global_mesh_map_id

global_mesh_map_id = 1000*source_global_mesh_id + target_global_mesh_id

return
end function generate_global_mesh_map_id
end module global_mesh_map_mod

