!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

!> @brief Holds support routines for instantiating stencil dofmaps.
!>
module stencil_dofmap_helper_functions_mod
  use constants_mod,        only: i_def, l_def

  implicit none

  private
  public :: generate_stencil_dofmap_id
  public :: get_stencil_cells

contains

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

  !> @brief Get the cell IDs for all the cells included in the stencil
  !> @param[in] mesh Mesh object to find cells from
  !> @param[in] origin Starting cell for stencil (centre cell)
  !> @param[in] st_depth Stencil depth
  !> @param[in] number_of_neighbours Number of cell neighbours
  !> @param[in] direction_map Array of direction sizes for stencil
  !> @param[in] region Boolean option for creating a region stencil
  !> @param[inout] cells_in_stencil Number of cells currently in stencil
  !> @param[inout] stencil_cells Array of cell IDs for stencil
  !==============================================================================
  subroutine get_stencil_cells( mesh,                  &
                                origin,                &
                                st_depth,              &
                                number_of_neighbours,  &
                                direction_map,         &
                                cells_in_stencil,      &
                                stencil_cells,         &
                                region_stencil )

    use mesh_mod,              only: mesh_type
    use master_dofmap_mod,     only: master_dofmap_type

    implicit none

    type(mesh_type), pointer, intent(in) :: mesh
    integer(i_def), intent(in) :: origin
    integer(i_def), intent(in) :: st_depth
    integer(i_def), intent(in) :: number_of_neighbours
    integer(i_def), intent(in), dimension(number_of_neighbours) :: direction_map
    integer(i_def), intent(inout) :: cells_in_stencil
    integer(i_def), allocatable, intent(inout) :: stencil_cells(:)
    logical(l_def), optional, intent(in) :: region_stencil

    integer(i_def) :: i, j, k
    integer(i_def) :: cell
    integer(i_def) :: new_cell
    integer(i_def) :: branch_cell
    integer(i_def) :: branch_new_cell
    integer(i_def) :: direction
    integer(i_def) :: direction_branch
    logical(l_def) :: region

    region = .false.
    if(present(region_stencil)) region = region_stencil

    do i=1, number_of_neighbours
      ! Starting cell is origin
      cell = origin
      direction = i
      if (direction_map(direction) > 0) then
        do j=1, direction_map(direction)
          new_cell = mesh%get_cell_next(direction, cell)
          ! Only add the cell to the stencil_cells list if it is not already
          ! added and is greater than 0
          if (.not.(any(stencil_cells == new_cell)) .and. new_cell > 0) then
            cells_in_stencil = cells_in_stencil + 1
            stencil_cells(cells_in_stencil) = new_cell
          end if
          ! If new_cell ID is greater than 0 then find the correct direction
          ! This is to make sure the direction is correct when moving panels
          if (new_cell > 0) then
            direction = direction_rotation( mesh, number_of_neighbours, &
                                            new_cell, cell, 180_i_def )
            ! If this is a region stencil then find the cells adjacent to the
            ! new_cell using the stencil depth and a direction 90 degrees from
            ! the direction of the origin cell
            if(region) then
              branch_cell = new_cell
              ! Get direction 90 degrees from cell
              direction_branch = direction_rotation( mesh, number_of_neighbours, &
                                                     branch_cell, cell, -90_i_def )
              do k=1, st_depth
                branch_new_cell = mesh%get_cell_next(direction_branch, branch_cell)
                ! Only add the cell to the stencil_cells list if it isn't already
                ! added and is greater than 0
                if ( .not.(any(stencil_cells == branch_new_cell)) &
                     .and. branch_new_cell > 0 ) then
                  cells_in_stencil = cells_in_stencil + 1
                  stencil_cells(cells_in_stencil) = branch_new_cell
                end if
                ! If new_cell ID is greater than 0 then find the correct direction
                ! This is to make sure the direction is correct when moving panels
                if (branch_new_cell > 0) then
                  direction_branch = direction_rotation( mesh, &
                                                         number_of_neighbours, &
                                                         branch_new_cell, &
                                                         branch_cell, 180_i_def )
                  branch_cell = branch_new_cell
                end if
              end do
            end if
            ! new_cell is now origin cell for next step
            cell = new_cell
          end if
        end do
      end if
    end do

  end subroutine get_stencil_cells



  !> @brief Returns a rotated direction from the origin_cell
  !> @param[in] mesh Pointer to mesh
  !> @param[in] number_of_neighbours Number of neighbouring cells
  !> @param[in] current_cell Current cell to find direction from
  !> @param[in] origin_cell Origin cell for start direction
  !> @param[in] rotation Value for rotation
  !> @return rotated_direction The rotated direction from the origin_cell
  !==============================================================================
  function direction_rotation( mesh,   &
                               number_of_neighbours, &
                               current_cell, &
                               origin_cell, &
                               rotation ) &
                               result( rotated_direction )

    use log_mod,               only: log_event,         &
                                     log_scratch_space, &
                                     LOG_LEVEL_ERROR
    use mesh_mod,              only: mesh_type
    use reference_element_mod, only: W, E, N, S

    implicit none

    type(mesh_type), pointer, intent(in) :: mesh
    integer(i_def), intent(in) :: number_of_neighbours
    integer(i_def), intent(in) :: current_cell
    integer(i_def), intent(in) :: origin_cell
    integer(i_def), intent(in) :: rotation

    integer(i_def) :: rotated_direction
    integer(i_def) :: direction
    integer(i_def), dimension(4) :: direction_ref = (/ W, S, E, N /)
    integer(i_def), dimension(4) :: rotated

    ! Rotate the array using cshift to change the order of the direction map
    select case (rotation)
      case (90_i_def, -270_i_def)  ! North becomes East
        rotated = cshift(direction_ref, shift = 1)
      case (-90_i_def, 270_i_def)  ! North becomes West
        rotated = cshift(direction_ref, shift = -1)
      case (180_i_def, -180_i_def) ! North becomes south
        rotated = cshift(direction_ref, shift = 2)
      case default
        write( log_scratch_space, '( A, I3 )' ) &
           'Invalid rotation value', rotation
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end select

    ! Find direction to origin cell and return
    do direction=1, number_of_neighbours
      if (mesh%get_cell_next(direction, current_cell) == origin_cell) then
        rotated_direction = rotated(direction)
      end if
    end do

    return
  end function direction_rotation

end module stencil_dofmap_helper_functions_mod
