!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Computes mesh tiling to improve processor cache utilisation.
!>
!> @details Contains algorithms that split a local 2D mesh into tiles,
!>          changing the order in which columns are computed to
!>          improve processor cache reuse. Tiles are coloured to
!>          enable parallel computation with multithreading.
!>
module mesh_tiling_mod

  use constants_mod,  only : i_def, l_def
  use local_mesh_mod, only : local_mesh_type
  use reference_element_mod, only : W, S, E, N
  use log_mod,        only : log_event, LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, &
                             LOG_LEVEL_WARNING, LOG_LEVEL_ERROR,         &
                             log_scratch_space

  implicit none

  private

  public :: set_tiling

contains

  !============================================================================
  !> @brief Assigns 2D cells to rectangular coloured tiles
  !> @param[in]  ncells_2d   Number of cells in the horizontal dimensions.
  !> @param[in]  cell_next   Adjacency array for the mesh.
  !> @param[in]  local_mesh  A pointer to the local mesh object that
  !>                         the mesh is built on.
  !> @param[in]  ncolours    Number of colours (from cell colouring module)
  !> @param[in]  ncells_per_colour Count of cells per colour (from colouring
  !>                               module)
  !> @param[in]  cells_in_colour List of cell indices in each colour (from
  !>                             colouring module)
  !> @param[in]  tile_size   Sets tile size to (mxn) cells, activates tiling
  !> @param[in]  separate_inner_halo Tile inner halo separately from interior
  !>                                 to enable overlapping computation and
  !>                                 communication
  !> @param[out] ntilecolours Number of colours used for tiling
  !> @param[out] ntiles_per_colour Number of tiles in a given colour
  !> @param[out] ncells_per_coloured_tile Number of cells per tile and colour
  !> @param[out] cells_in_coloured_tile  List of cell IDs for each colour
  !>                                     and tile
  !> @param[out] last_inner_tile_per_colour Loop limit for tiles with inner
  !>                                        halo cells
  !> @param[out] last_inner_cell_per_coloured_tile Loop limit for inner
  !>                                               cells in a tile
  !> @param[out] last_edge_tile_per_colour Loop limit for tiles with edge cells
  !> @param[out] last_edge_cell_per_coloured_tile Loop limit for edge cells in
  !>                                              a tile
  !> @param[out] last_halo_tile_per_colour Loop limit for tiles with halo cells
  !> @param[out] last_halo_cell_per_coloured_tile Loop limit for halo cells in
  !>                                              a tile
  !============================================================================
  subroutine set_tiling( ncells_2d,                         &
                         cell_next,                         &
                         local_mesh,                        &
                         ncolours,                          &
                         ncells_per_colour,                 &
                         cells_in_colour,                   &
                         tile_size,                         &
                         separate_inner_halo,               &
                         ntilecolours,                      &
                         ntiles_per_colour,                 &
                         ncells_per_coloured_tile,          &
                         cells_in_coloured_tile,            &
                         last_inner_tile_per_colour,        &
                         last_inner_cell_per_coloured_tile, &
                         last_edge_tile_per_colour,         &
                         last_edge_cell_per_coloured_tile,  &
                         last_halo_tile_per_colour,         &
                         last_halo_cell_per_coloured_tile )

    implicit none

    integer(i_def),        intent(in)          :: ncells_2d
    integer(i_def),        intent(in)          :: cell_next(:,:)
    type(local_mesh_type), intent(in), pointer :: local_mesh
    integer(i_def),        intent(in)          :: ncolours
    integer(i_def),        intent(in)          :: ncells_per_colour(:)
    integer(i_def),        intent(in)          :: cells_in_colour(:,:)
    integer(i_def),        intent(in)          :: tile_size(2)
    logical(l_def),        intent(in)          :: separate_inner_halo
    integer(i_def), intent(out)                :: ntilecolours
    integer(i_def), intent(out), allocatable   :: ntiles_per_colour(:)
    integer(i_def), intent(out), allocatable   :: ncells_per_coloured_tile(:,:)
    integer(i_def), intent(out), allocatable   :: cells_in_coloured_tile(:,:,:)
    integer(i_def), intent(out), &
         allocatable   :: last_inner_tile_per_colour(:,:)
    integer(i_def), intent(out), &
         allocatable   :: last_inner_cell_per_coloured_tile(:,:,:)
    integer(i_def), intent(out), &
         allocatable   :: last_edge_tile_per_colour(:)
    integer(i_def), intent(out), &
         allocatable   :: last_edge_cell_per_coloured_tile(:,:)
    integer(i_def), intent(out), &
         allocatable   :: last_halo_tile_per_colour(:,:)
    integer(i_def), intent(out), &
         allocatable   :: last_halo_cell_per_coloured_tile(:,:,:)

    integer(i_def) :: npanels_global_mesh
    integer(i_def) :: ncells_2d_global_mesh
    integer(i_def), allocatable :: cell_next_reordered(:,:)
    integer(i_def) :: num_interior_cells_x, num_interior_cells_y
    integer(i_def) :: inner_halo_width, outer_halo_width
    logical(l_def) :: edge_halos(4)
    logical(l_def) :: corner_halos(4)
    integer(i_def) :: nw_cell_partition
    integer(i_def) :: ntiles
    logical(l_def) :: tiling_possible
    logical(l_def) :: correct

    integer(i_def) :: colour

    ! Turn cell colouring into tiling if a tile size of 1x1 is requested
    ! Unlike > 1x1 tiling, this is supported for all meshes
    if ( tile_size(1) == 1 .and. tile_size(2) == 1 ) then
      ! Coloured cells become tiles with a cell count of 1
      ntilecolours = ncolours
      allocate( ntiles_per_colour, source=ncells_per_colour )
      allocate( ncells_per_coloured_tile( ncolours,                    &
                                          maxval(ncells_per_colour) ), &
                source=0_i_def)
      do colour = 1, ntilecolours
        ncells_per_coloured_tile( colour, 1:ntiles_per_colour(colour) ) = &
             1_i_def
      end do
      allocate( cells_in_coloured_tile( ncolours, maxval(ncells_per_colour), &
                                        1 ),                                 &
                source=reshape( cells_in_colour,                             &
                                (/ ncolours, maxval(ncells_per_colour), 1 /) &
                              ) )

      call init_last_cell( local_mesh, ntilecolours, ntiles_per_colour,      &
                           ncells_per_coloured_tile, cells_in_coloured_tile, &
                           last_inner_tile_per_colour,                       &
                           last_inner_cell_per_coloured_tile,                &
                           last_edge_tile_per_colour,                        &
                           last_edge_cell_per_coloured_tile,                 &
                           last_halo_tile_per_colour,                        &
                           last_halo_cell_per_coloured_tile )

      write( log_scratch_space, '(A)' ) &
             'set_tiling: 1x1 tile size requested, falling back on colouring'
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )

    ! Attempt to tile the mesh using the requested tile size
    else

      npanels_global_mesh = local_mesh%get_num_panels_global_mesh()
      ncells_2d_global_mesh = local_mesh%get_ncells_global_mesh()

      if ( local_mesh%get_inner_depth() < 1 .or. &
           local_mesh%get_halo_depth() < 1 ) then
        write( log_scratch_space, '(A)' ) &
             'set_tiling: Outer and inner halos must be present for tiling'
        call log_event( log_scratch_space, LOG_LEVEL_WARNING )
        tiling_possible = .false.

      ! Partitioned cubed-sphere or non-periodic partitioned planar mesh
      else if ( ( npanels_global_mesh == 6 .or.                   &
                ( npanels_global_mesh == 1 .and.                  &
                  local_mesh%is_topology_non_periodic() ) ) .and. &
                ncells_2d_global_mesh /= ncells_2d ) then
        call compute_partition_specs( ncells_2d, cell_next, local_mesh,       &
                                      tile_size, separate_inner_halo,         &
                                      num_interior_cells_x,                   &
                                      num_interior_cells_y, inner_halo_width, &
                                      outer_halo_width, edge_halos,           &
                                      corner_halos, nw_cell_partition,        &
                                      tiling_possible )
      else
        ! Non-partitioned cases and other mesh types are not implemented yet
        write( log_scratch_space, '(A)' ) &
             'set_tiling: tiling not implemented for this mesh type or ' // &
             'non-partitioned meshes'
        call log_event( log_scratch_space, LOG_LEVEL_WARNING )
        tiling_possible = .false.
      end if

      ! Fail if tiling is not possible for this mesh
      if ( .not. tiling_possible ) then
        write( log_scratch_space, '(A)' ) &
               'set_tiling: tiling is not possible for this mesh, ' // &
               'choose 1x1 tile size to fall back on colouring '
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      ! Copy adjacency array for reordering
      allocate( cell_next_reordered, source=cell_next )

      ! Reorder cell_next to ensure that it uses consistent N, S, E, W
      ! orientations in halos of a partitioned cubed-sphere
      if ( npanels_global_mesh == 6 ) then
        call reorder_cell_next( nw_cell_partition, inner_halo_width,           &
                                outer_halo_width, num_interior_cells_x,        &
                                num_interior_cells_y, corner_halos, cell_next, &
                                cell_next_reordered )
      end if

      ! Populate tiling array, catch any failures that will deactivate tiling
      correct = compute_coloured_tiling( nw_cell_partition,                  &
                                         num_interior_cells_x,               &
                                         num_interior_cells_y,               &
                                         inner_halo_width, outer_halo_width, &
                                         edge_halos, corner_halos, tile_size,&
                                         cell_next_reordered,                &
                                         separate_inner_halo,                &
                                         ntiles, ntilecolours,               &
                                         ntiles_per_colour,                  &
                                         ncells_per_coloured_tile,           &
                                         cells_in_coloured_tile )

      ! Compute last tile/cell counts
      call init_last_cell( local_mesh, ntilecolours, ntiles_per_colour,      &
                           ncells_per_coloured_tile, cells_in_coloured_tile, &
                           last_inner_tile_per_colour,                       &
                           last_inner_cell_per_coloured_tile,                &
                           last_edge_tile_per_colour,                        &
                           last_edge_cell_per_coloured_tile,                 &
                           last_halo_tile_per_colour,                        &
                           last_halo_cell_per_coloured_tile )

      ! Verify if tiling is correct
      correct = correct .and.                                                &
                test_coloured_tiling( cell_next_reordered, ncells_2d,        &
                                      local_mesh, ntiles, ntilecolours,      &
                                      ntiles_per_colour,                     &
                                      ncells_per_coloured_tile,              &
                                      cells_in_coloured_tile,                &
                                      last_inner_tile_per_colour,            &
                                      last_inner_cell_per_coloured_tile,     &
                                      last_edge_tile_per_colour,             &
                                      last_edge_cell_per_coloured_tile,      &
                                      last_halo_tile_per_colour,             &
                                      last_halo_cell_per_coloured_tile,      &
                                      separate_inner_halo )

      deallocate(cell_next_reordered)

      if ( correct ) then
        write( log_scratch_space, '(A)' ) 'set_tiling: Successfully set ' // &
                                          'up tiling'
        call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      else
        write( log_scratch_space, '(A)' ) &
               'set_tiling: Problem encountered when trying to tile mesh'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

  end subroutine set_tiling

  !============================================================================
  !> @brief Determines whether a given cell is at a cubed-sphere corner
  !> @param[in]  cell  Cell ID that should be investigated
  !> @param[in]  cell_next   Adjacency array for the mesh.
  !> @result     True if cell is at a cube-sphere corner, false otherwise
  !============================================================================
  function is_cubed_sphere_corner( cell, cell_next ) result( is_corner )

    implicit none
    integer(i_def), intent(in) :: cell
    integer(i_def), intent(in) :: cell_next(:,:)
    logical(l_def)             :: is_corner

    integer(i_def) :: dirs(4)
    logical(l_def) :: same_cell(4,4,4)
    integer(i_def) :: i, j, k

    dirs = (/ N, E, S, W /)

    ! Cells at cubed-sphere corners have only 7 neighbours, so a two-step walk
    ! in the neighbourhood can lead to the same cell as a one-step walk
    ! Need to test all possible directions here, since N, S, E, W orientation
    ! in cell_next can change in outer halos.
    do i = 1, 4
      do j = 1, 4
        do k = 1, 4
          same_cell( i, j, k ) = cell_next(dirs(i), cell) == &
               cell_next(dirs(j), cell_next(dirs(k), cell))
        end do
      end do
    end do
    is_corner = any(same_cell)

    return

  end function is_cubed_sphere_corner

  !============================================================================
  !> @brief Determines partitioned mesh properties needed for tiling
  !> @param[in]  num_cells_2d Number of 2D cells with halo
  !> @param[in]  cell_next   Adjacency array for the mesh.
  !> @param[in]  local_mesh  A pointer to the local mesh object that
  !>                         the mesh is built on.
  !> @param[in]  tile_size   Tile size (mxn) cells
  !> @param[in]  separate_inner_halo Inner halo tiled separately from interior
  !> @param[out] num_interior_cells_x   Number of interior cells in x dim
  !> @param[out] num_interior_cells_y   Number of interior cells in y dim
  !> @param[out] inner_halo_width       Width of the inner halo
  !> @param[out] outer_halo_width       Width of the outer halo
  !> @param[out] edge_halos             Presence of halos on partition edges
  !> @param[out] corner_halos           Presence of halos in partition corners
  !> @param[out] nw_cell_partition      North-westernmost edge cell
  !> @param[out] valid_partition_spec   Spec determination was successful
  !============================================================================
  subroutine compute_partition_specs( num_cells_2d, cell_next, local_mesh,    &
                                      tile_size, separate_inner_halo,         &
                                      num_interior_cells_x,                   &
                                      num_interior_cells_y, inner_halo_width, &
                                      outer_halo_width, edge_halos,           &
                                      corner_halos, nw_cell_partition,        &
                                      valid_partition_spec )

    use lfric_mpi_mod, only : global_mpi

    implicit none

    integer(i_def), intent(in)                 :: num_cells_2d
    integer(i_def), intent(in)                 :: cell_next(:,:)
    type(local_mesh_type), pointer, intent(in) :: local_mesh
    integer(i_def), intent(in)                 :: tile_size(2)
    logical(l_def), intent(in)                 :: separate_inner_halo
    integer(i_def), intent(out)                :: num_interior_cells_x
    integer(i_def), intent(out)                :: num_interior_cells_y
    integer(i_def), intent(out)                :: inner_halo_width
    integer(i_def), intent(out)                :: outer_halo_width
    logical(l_def), intent(out)                :: edge_halos(4)
    logical(l_def), intent(out)                :: corner_halos(4)
    integer(i_def), intent(out)                :: nw_cell_partition
    logical(l_def), intent(out)                :: valid_partition_spec

    integer(i_def) :: myrank
    integer(i_def) :: cell
    integer(i_def) :: num_partition_cells_x
    integer(i_def) :: num_partition_cells_y
    integer(i_def) :: cell_count
    logical(l_def) :: cell_walk_returns
    logical(l_def) :: has_interior_cells
    logical(l_def) :: tile_size_ok
    logical(l_def) :: cell_count_correct
    integer(i_def) :: num

    myrank = global_mpi%get_comm_rank()

    corner_halos = .false.

    ! Walk north-west to find north-westernmost corner cell that belongs to
    ! this partition and check for the presence of outer halos
    cell = 1
    do while ( cell_next( N, cell ) > 0 )
      if ( local_mesh%get_cell_owner( cell_next( N, cell ) ) /= myrank ) exit
      cell = cell_next( N, cell )
    end do
    do while ( cell_next( W, cell ) > 0 )
      if ( local_mesh%get_cell_owner( cell_next( W, cell ) ) /= myrank ) exit
      cell = cell_next( W, cell )
    end do
    nw_cell_partition = cell
    edge_halos(1) = cell_next( N, cell ) > 0
    edge_halos(2) = cell_next( W, cell ) > 0
    ! Check for corner halo - can only be present if the two neighbouring
    ! edge halos are present, and if this is not a cubed-sphere corner
    if ( edge_halos(1) .and. edge_halos(2) ) then
      corner_halos(1) = .not. is_cubed_sphere_corner( cell, cell_next )
    end if

    ! Walk east, then south, count cells in each direction
    num_partition_cells_x = 1
    do while ( cell_next( E, cell ) > 0 )
      if ( local_mesh%get_cell_owner( cell_next( E, cell ) ) /= myrank ) exit
      cell = cell_next( E, cell )
      num_partition_cells_x = num_partition_cells_x + 1
    end do
    edge_halos(3) = cell_next( E, cell ) > 0
    if ( edge_halos(1) .and. edge_halos(3) ) then
      corner_halos(2) = .not. is_cubed_sphere_corner( cell, cell_next )
    end if

    num_partition_cells_y = 1
    do while ( cell_next( S, cell ) > 0 )
      if ( local_mesh%get_cell_owner( cell_next( S, cell ) ) /=  myrank ) exit
      cell = cell_next( S, cell )
      num_partition_cells_y = num_partition_cells_y + 1
    end do
    edge_halos(4) = cell_next( S, cell ) > 0
    if ( edge_halos(3) .and. edge_halos(4) ) then
      corner_halos(4) = .not. is_cubed_sphere_corner( cell, cell_next )
    end if

    ! Walk west to check for a possible corner halo
    do while ( cell_next( W, cell ) > 0 )
      if ( local_mesh%get_cell_owner( cell_next( W, cell ) ) /= myrank ) exit
      cell = cell_next( W, cell )
    end do
    if ( edge_halos(2) .and. edge_halos(4) ) then
      corner_halos(3) = .not. is_cubed_sphere_corner( cell, cell_next )
    end if

    ! Sanity check - we should return to the north-west corner using
    ! exactly num_partition_cells_y-1 steps
    cell_count = 0
    do while ( cell_next( N, cell ) > 0 )
      if ( local_mesh%get_cell_owner( cell_next( N, cell ) ) /= myrank ) exit
      cell = cell_next( N, cell )
      cell_count = cell_count + 1
    end do
    if ( cell == nw_cell_partition .and. &
        cell_count == num_partition_cells_y-1 ) then
      cell_walk_returns = .true.
    else
      write( log_scratch_space, '(A)' ) &
           'compute_partition_specs: Cell walk did not return to ' // &
           'north-west corner, assuming that partition is not rectangular'
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      cell_walk_returns = .false.
    end if

    inner_halo_width = local_mesh%get_inner_depth()
    outer_halo_width = local_mesh%get_halo_depth()

    num_interior_cells_x = num_partition_cells_x - 2*inner_halo_width
    num_interior_cells_y = num_partition_cells_y - 2*inner_halo_width

    ! Sanity check - ensure that that partition has interior cells
    if ( num_interior_cells_x > 0 .and. num_interior_cells_y > 0 ) then
      has_interior_cells = .true.
    else
      write( log_scratch_space, '(A)' ) &
           'compute_partition_specs: Partition has no interior cells'
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      has_interior_cells = .false.
    end if

    ! Sanity check - fail if requested tile size is too large
    if ( separate_inner_halo ) then
      tile_size_ok = minval( (/ num_interior_cells_x, num_interior_cells_y /)  &
                             - tile_size ) >= 0
      if ( .not. tile_size_ok ) then
        write( log_scratch_space, '(4(A,I0),A)' )                 &
               'compute_partition_specs: Requested tile size (',  &
               tile_size(1), 'x', tile_size(2), ') larger ' //    &
               'than partition interior (', num_interior_cells_x, &
               'x', num_interior_cells_y, ')'
        call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      end if
    else
      tile_size_ok = minval( (/ num_partition_cells_x, num_partition_cells_y /)&
                             - tile_size ) >= 0
      if ( .not. tile_size_ok ) then
        write( log_scratch_space, '(4(A,I0),A)' )                &
               'compute_partition_specs: Requested tile size (', &
               tile_size(1), 'x', tile_size(2), ') larger ' //   &
               'than partition (', num_partition_cells_x,        &
               'x', num_partition_cells_y, ')'
        call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      end if
    end if

    ! Sanity check - compute total number of cells in this partition,
    ! including outer halo, to verify that it matches cell count from
    ! the mesh object. Corner halos are only available where a corner
    ! cell is not at a cubed-sphere corner.
    cell_count = num_interior_cells_x*num_interior_cells_y + &
                 2*inner_halo_width*num_interior_cells_x   + &
                 2*inner_halo_width*num_interior_cells_y   + &
                 4*inner_halo_width*inner_halo_width
    do num = 1, 4
      if ( edge_halos(num) ) then
        if ( ( num == 1 .or. num == 4 ) ) then
          cell_count = cell_count + num_partition_cells_x*outer_halo_width
        else
          cell_count = cell_count + num_partition_cells_y*outer_halo_width
        end if
      end if
      if ( corner_halos(num) ) cell_count = cell_count + outer_halo_width**2
    end do
    if ( cell_count == num_cells_2d ) then
      cell_count_correct = .true.
    else
      write( log_scratch_space, '(A)' )                       &
           'compute_partition_specs: Computed cell count ' // &
           'in partition does not match expected cell count'
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
      cell_count_correct = .false.
    end if

    ! Check if spec results are reasonable
    valid_partition_spec = cell_walk_returns .and. has_interior_cells  &
                           .and. tile_size_ok .and. cell_count_correct

    write(log_scratch_space, '(A,L1)')                         &
         'compute_partition_specs: Partition spec is valid: ', &
         valid_partition_spec
    call log_event( log_scratch_space, LOG_LEVEL_DEBUG )

    if (valid_partition_spec) then
      write( log_scratch_space, '(4(A,I0),A)' )                 &
           'compute_partition_specs: Partition size is (',      &
           num_partition_cells_x, 'x', num_partition_cells_y,   &
           ') cells, interior size is (', num_interior_cells_x, &
           'x', num_interior_cells_y, ') cells'
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      write( log_scratch_space, '(3(A,1X,I0))' )                             &
           'compute_partition_specs: Inner halo width is', inner_halo_width, &
           ', outer halo width is', outer_halo_width,                        &
           ', north-westernmost owned local cell ID is', nw_cell_partition
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      write( log_scratch_space, '(2(A,1X,I0,1X))' )                       &
           'compute_partition_specs: Computed cell count is', cell_count, &
           'Actual partition cell count is', num_cells_2d
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      write(log_scratch_space, '(4(A,L1,1X))') 'compute_partition_specs: ' // &
           'N edge halo: ', edge_halos(1), &
           'W edge halo: ', edge_halos(2), &
           'E edge halo: ', edge_halos(3), &
           'S edge halo: ', edge_halos(4)
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      write(log_scratch_space, '(4(A,L1,1X))') 'compute_partition_specs: ' // &
           'NW corner halo: ', corner_halos(1), &
           'NE corner halo: ', corner_halos(2), &
           'SW corner halo: ', corner_halos(3), &
           'SE corner halo: ', corner_halos(4)
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end if

  end subroutine compute_partition_specs

  !============================================================================
  !> @brief Rotates cell connectivity for consistent orientation in halos
  !> @param[in]  start_cell  Halo cell ID for where the algorithm starts
  !> @param[in]  match_cell  Edge cell which rotated orientation should match
  !> @param[in]  match_dir   Neighbourhood direction that should be matched
  !> @param[in]  halo_dim_x  Number of halo cells to process along x axis
  !> @param[in]  halo_dim_y  Number of halo cells to process along y axis
  !> @param[in]  step_dir_x  Direction in halo should be swept along x axis
  !> @param[in]  step_dir_y  Direction in halo should be swept along y axis
  !> @param[in]  cell_next   Adjacency array for the mesh
  !> @param[inout] cell_next_reordered  Reordered adjacency array
  !============================================================================
  subroutine change_neighbour_orientation( start_cell, match_cell, match_dir,  &
                                           halo_dim_x, halo_dim_y, step_dir_x, &
                                           step_dir_y, cell_next,              &
                                           cell_next_reordered )

    implicit none

    integer(i_def), intent(in) :: start_cell
    integer(i_def), intent(in) :: match_cell
    integer(i_def), intent(in) :: match_dir
    integer(i_def), intent(in) :: halo_dim_x
    integer(i_def), intent(in) :: halo_dim_y
    integer(i_def), intent(in) :: step_dir_x
    integer(i_def), intent(in) :: step_dir_y
    integer(i_def), intent(in) :: cell_next(:,:)
    integer(i_def), intent(inout) :: cell_next_reordered(:,:)

    integer(i_def) :: cell
    integer(i_def) :: row_start_cell
    integer(i_def) :: i, j, k
    integer(i_def) :: order_lookup(4)
    integer(i_def) :: neighbours(4)

    ! Initialise look-up array with current order
    order_lookup = (/ 1, 2, 3, 4 /)

    ! Find correct orientation by rotating array with neighbour cell IDs
    ! at the first halo cell and matching cell IDs with the adjacent edge
    ! cell in the requested direction
    neighbours = cell_next( 1:4, start_cell )
    do i = 1, 4
      neighbours = cshift( neighbours, 1 )
      order_lookup = cshift( order_lookup, 1 )
      if ( neighbours(match_dir) == match_cell ) exit
    end do

    ! Loop over all cells in this halo section and rotate orientation
    row_start_cell = start_cell
    do k = 1, halo_dim_y
      cell = row_start_cell
      do j = 1, halo_dim_x
        do i = 1, 4
          cell_next_reordered( i, cell ) = cell_next( order_lookup(i), cell )
        end do
        cell = cell_next( order_lookup(step_dir_x), cell )
      end do
      row_start_cell = cell_next( order_lookup(step_dir_y), row_start_cell )
    end do

  end subroutine change_neighbour_orientation

  !============================================================================
  !> @brief Reorder cell adjacency in halos of a partitioned cubed-sphere
  !>        for consistent N,S,E,W orientation across the partition and halos
  !> @param[in] nw_cell                Cell ID of the north-westernmost cell
  !> @param[in] inner_halo_width       Width of the inner halo
  !> @param[in] outer_halo_width       Width of the outer halo
  !> @param[in] num_interior_cells_x   Number of interior cells in x dim
  !> @param[in] num_interior_cells_y   Number of interior cells in y dim
  !> @param[in] corner_halos           Presence of halos in partition corners
  !> @param[in] cell_next              Adjacency array for the mesh.
  !> @param[inout] cell_next_reordered Reordered adjacency array for the mesh.
  !============================================================================
  subroutine reorder_cell_next( nw_cell, inner_halo_width, outer_halo_width,  &
                                num_interior_cells_x, num_interior_cells_y,   &
                                corner_halos, cell_next, cell_next_reordered )

    implicit none

    integer(i_def), intent(in) :: nw_cell
    integer(i_def), intent(in) :: inner_halo_width
    integer(i_def), intent(in) :: outer_halo_width
    integer(i_def), intent(in) :: num_interior_cells_x
    integer(i_def), intent(in) :: num_interior_cells_y
    logical(l_def), intent(in) :: corner_halos(4)
    integer(i_def), intent(in) :: cell_next(:,:)
    integer(i_def), intent(inout) :: cell_next_reordered(:,:)

    integer(i_def) :: cell
    integer(i_def) :: edge_cell
    integer(i_def) :: halo_cell
    integer(i_def) :: i

    ! Check consistency of cell IDs at northern halo
    edge_cell = nw_cell
    halo_cell = cell_next( N, edge_cell )
    if ( cell_next(S, halo_cell) /= edge_cell ) then
      call change_neighbour_orientation( halo_cell, edge_cell, S,         &
           2*inner_halo_width + num_interior_cells_x, outer_halo_width, &
           E, N, cell_next, cell_next_reordered )
    end if

    ! Eastern halo, need to get there first
    cell = nw_cell
    do i = 1, 2*inner_halo_width + num_interior_cells_x - 1
      cell = cell_next( E, cell )
    end do
    edge_cell = cell
    halo_cell = cell_next( E, edge_cell )
    if ( cell_next( W, halo_cell ) /= edge_cell ) then
      call change_neighbour_orientation( halo_cell, edge_cell, W,       &
           outer_halo_width, 2*inner_halo_width + num_interior_cells_y, &
           E, S, cell_next, cell_next_reordered )
    end if

    ! Southern halo
    cell = nw_cell
    do i = 1, 2*inner_halo_width + num_interior_cells_y - 1
      cell = cell_next( S, cell )
    end do
    edge_cell = cell
    halo_cell = cell_next( S, edge_cell )
    if ( cell_next( N, halo_cell ) /= edge_cell ) then
      call change_neighbour_orientation( halo_cell, edge_cell, N,       &
           2*inner_halo_width + num_interior_cells_x, outer_halo_width, &
           E, S, cell_next, cell_next_reordered )
    end if

    ! Western halo
    edge_cell = nw_cell
    halo_cell = cell_next( W, edge_cell )
    if ( cell_next( E, halo_cell ) /= edge_cell ) then
      call change_neighbour_orientation( halo_cell, edge_cell, E,       &
           outer_halo_width, 2*inner_halo_width + num_interior_cells_y, &
           W, S, cell_next, cell_next_reordered )
    end if

    ! Now all outer halos have consistent order, so we will be able to reach
    ! corner halos reliably using cell_next_reordered, where they exist
    ! North-western corner
    if ( corner_halos(1) ) then
      cell = nw_cell
      cell = cell_next( W, cell )
      edge_cell = cell
      halo_cell = cell_next_reordered( N, edge_cell )
      if ( cell_next( S, halo_cell ) /= edge_cell ) then
        call change_neighbour_orientation( halo_cell, edge_cell, S, &
             outer_halo_width, outer_halo_width, W, N, cell_next,  &
             cell_next_reordered )
      end if
    end if

    ! North-eastern corner
    if ( corner_halos(2) ) then
      cell = nw_cell
      do i = 1, 2*inner_halo_width + num_interior_cells_x
        cell = cell_next( E, cell )
      end do
      edge_cell = cell
      halo_cell = cell_next_reordered( N, edge_cell )
      if ( cell_next( S, halo_cell ) /= edge_cell ) then
        call change_neighbour_orientation( halo_cell, edge_cell, S, &
             outer_halo_width, outer_halo_width, E, N, cell_next,  &
             cell_next_reordered )
      end if
    end if

    ! South-western corner
    if ( corner_halos(3) ) then
      cell = nw_cell
      do i = 1, 2*inner_halo_width + num_interior_cells_y
        cell = cell_next( S, cell )
      end do
      edge_cell = cell
      halo_cell = cell_next_reordered( W, edge_cell )
      if ( cell_next( E, halo_cell ) /= edge_cell ) then
        call change_neighbour_orientation( halo_cell, edge_cell, E, &
             outer_halo_width, outer_halo_width, W, S, cell_next,  &
             cell_next_reordered )
      end if
    end if

    ! South-eastern corner
    if ( corner_halos(4) ) then
      cell = nw_cell
      do i = 1, 2*inner_halo_width + num_interior_cells_x - 1
        cell = cell_next( E, cell )
      end do
      do i = 1, 2*inner_halo_width + num_interior_cells_y
        cell = cell_next( S, cell )
      end do
      edge_cell = cell
      halo_cell = cell_next_reordered( E, edge_cell )
      if ( cell_next( W, halo_cell ) /= edge_cell ) then
        call change_neighbour_orientation( halo_cell, edge_cell, W, &
             outer_halo_width, outer_halo_width, E, S, cell_next,  &
             cell_next_reordered )
      end if
    end if

  end subroutine reorder_cell_next

  !============================================================================
  !> @brief Assign cells to coloured tiles, treating partition interior and
  !>        halo separately to enable overlapping communication and computation
  !> @param[in]  nw_cell_partition  North-westernmost cell ID in this partition
  !> @param[in]  num_interior_cells_x   Number of interior cells in x dim.
  !> @param[in]  num_interior_cells_y   Number of interior cells in y dim.
  !> @param[in]  inner_halo_width       Width of the inner halo
  !> @param[in]  outer_halo_width       Width of the inner halo
  !> @param[in]  edge_halos             Presence of halos on partition edges
  !> @param[in]  corner_halos           Presence of halos in partition corners
  !> @param[in]  tile_size              Tile size in (mxn) cells
  !> @param[in]  cell_next              Adjacency array for the mesh.
  !> @param[in]  separate_inner_halo    Tile inner halo separately
  !> @param[out] ntiles                 Total number of tiles
  !> @param[out] ntilecolours Number of colours used for tiling
  !> @param[out] ntiles_per_colour Number of tiles in a given colour
  !> @param[out] ncells_per_coloured_tile Number of cells per tile and colour
  !> @param[out] cells_in_coloured_tile  List of cell IDs for each colour
  !>                                     and tile
  !> @result     True if no problem was found
  !============================================================================
  function compute_coloured_tiling( nw_cell_partition, num_interior_cells_x,   &
                                    num_interior_cells_y, inner_halo_width,    &
                                    outer_halo_width, edge_halos, corner_halos,&
                                    tile_size, cell_next, separate_inner_halo, &
                                    ntiles, ntilecolours, ntiles_per_colour,   &
                                    ncells_per_coloured_tile,                  &
                                    cells_in_coloured_tile ) result( success )

    use remove_duplicates_mod, only: remove_duplicates
    use sort_mod, only: bubble_sort

    implicit none

    integer(i_def), intent(in) :: nw_cell_partition
    integer(i_def), intent(in) :: num_interior_cells_x
    integer(i_def), intent(in) :: num_interior_cells_y
    integer(i_def), intent(in) :: inner_halo_width
    integer(i_def), intent(in) :: outer_halo_width
    logical(l_def), intent(in) :: edge_halos(4)
    logical(l_def), intent(in) :: corner_halos(4)
    integer(i_def), intent(in) :: tile_size(2)
    integer(i_def), intent(in) :: cell_next(:,:)
    logical(l_def), intent(in) :: separate_inner_halo
    integer(i_def), intent(out) :: ntiles
    integer(i_def), intent(out) :: ntilecolours
    integer(i_def), allocatable, intent(out) :: ntiles_per_colour(:)
    integer(i_def), allocatable, intent(out) :: ncells_per_coloured_tile(:,:)
    integer(i_def), allocatable, intent(out) :: cells_in_coloured_tile(:,:,:)
    logical(l_def) :: success

    integer(i_def) :: max_ntiles_x
    integer(i_def) :: max_ntiles_y
    integer(i_def) :: tile
    integer(i_def) :: max_num_cells_in_tile
    logical(l_def), allocatable :: tile_active(:,:)
    integer(i_def), allocatable :: cells_in_tile(:,:)
    integer(i_def), allocatable :: min_cellid_in_tile(:)
    integer(i_def), allocatable :: min_cellid_in_tile_idx(:)
    integer(i_def), allocatable :: tile_colour(:)
    integer(i_def), allocatable :: actual_colours(:)
    integer(i_def) :: tile_x
    integer(i_def) :: tile_y
    integer(i_def) :: row_start_cell
    integer(i_def) :: i, j
    integer(i_def) :: cell_counter
    integer(i_def) :: cell_distance
    integer(i_def) :: direction
    integer(i_def) :: cell
    integer(i_def) :: actual_tile_size(2)
    integer(i_def) :: colour

    success = .true.

    ! Partition and outer halo are always tiled separately to handle partial or
    ! full presence or absence of halo sections. Inner halos + partition edge
    ! can optionally be tiled separately from the interior to enable overlapping
    ! computation and communication. The actual number of tiles may be smaller
    ! if some outer halo tiles are absent, and tiles may be adjusted to fit
    ! into the mesh.
    if ( separate_inner_halo ) then
      max_ntiles_x = ceiling( real(num_interior_cells_x) / &
                              real(tile_size(1)) ) + 4
      max_ntiles_y = ceiling( real(num_interior_cells_y) / &
                              real(tile_size(2)) ) + 4
    else
      max_ntiles_x = ceiling( real(num_interior_cells_x + 2*inner_halo_width) /&
                              real(tile_size(1)) ) + 2
      max_ntiles_y = ceiling( real(num_interior_cells_y + 2*inner_halo_width) /&
                              real(tile_size(2)) ) + 2
    end if

    ! Set up tile availability according to presence of halos.
    allocate( tile_active( max_ntiles_x, max_ntiles_y ), source=.true. )

    tile_active( 1, 1 ) = corner_halos(1)
    tile_active( max_ntiles_x, 1 ) = corner_halos(2)
    tile_active( 1, max_ntiles_y ) = corner_halos(3)
    tile_active( max_ntiles_x, max_ntiles_y ) = corner_halos(4)

    tile_active( 2:max_ntiles_x-1, 1 ) = edge_halos(1)
    tile_active( 1, 2:max_ntiles_y-1 ) = edge_halos(2)
    tile_active( max_ntiles_x, 2:max_ntiles_y-1 ) = edge_halos(3)
    tile_active( 2:max_ntiles_x-1, max_ntiles_y ) = edge_halos(4)

    ntiles = count( tile_active )

    write( log_scratch_space, '(2(A,I0),A)' ) &
           'compute_coloured_tiling: Requested tile size is (', tile_size(1), &
            'x', tile_size(2), ')'
    call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    write( log_scratch_space, '(A,1X,I0,1X,A)' ) &
         'compute_coloured_tiling: Partition has', ntiles, &
         'outer halo and partition tiles'
    call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    if ( separate_inner_halo ) then
      write( log_scratch_space, '(A)' ) &
         'compute_coloured_tiling: Inner halo will be tiled separately ' // &
         'from partition interior'
    else
      write( log_scratch_space, '(A)' ) &
         'compute_coloured_tiling: Inner halo will be tiled together ' // &
         'with partition interior'
    end if
    call log_event( log_scratch_space, LOG_LEVEL_DEBUG )

    ! Maximum number of cells in outer halo, inner halo, and interior tiles
    max_num_cells_in_tile = max( outer_halo_width*outer_halo_width,   &
                                 outer_halo_width*maxval( tile_size ),&
                                 product( tile_size ) )
    if ( separate_inner_halo ) then
      max_num_cells_in_tile = max( max_num_cells_in_tile,               &
                                   outer_halo_width*inner_halo_width,   &
                                   inner_halo_width*inner_halo_width,   &
                                   inner_halo_width*maxval( tile_size ) &
                                 )
    end if

    allocate( cells_in_tile( max_num_cells_in_tile, ntiles ), source=0_i_def )
    allocate( tile_colour( ntiles ), source=0_i_def )
    allocate( min_cellid_in_tile( ntiles ), source=0_i_def )

    ! Loop over all halo and partition tiles and assign cell IDs and colours
    tile = 0
    do tile_y = 1, max_ntiles_y
      do tile_x = 1, max_ntiles_x

        ! Skip outer halos if they are unavailable
        if ( .not. tile_active( tile_x, tile_y ) ) cycle

        ! Determine x dimension of the tile and walk to first cell in x dir
        row_start_cell = nw_cell_partition
        if ( tile_x == 1 ) then
          ! Western outer halo
          cell_distance = outer_halo_width
          direction = W
          actual_tile_size(1) = outer_halo_width
        else if ( tile_x == 2 .and. separate_inner_halo ) then
          ! Western inner halo if required
          cell_distance = 0
          direction = W
          actual_tile_size(1) = inner_halo_width
        else if ( tile_x == max_ntiles_x - 1 .and. separate_inner_halo ) then
          ! Eastern inner halo if required
          cell_distance = inner_halo_width + num_interior_cells_x
          direction = E
          actual_tile_size(1) = inner_halo_width
        else if ( tile_x == max_ntiles_x ) then
          ! Eastern outer halo
          cell_distance = 2*inner_halo_width + num_interior_cells_x
          direction = E
          actual_tile_size(1) = outer_halo_width
        else if ( separate_inner_halo ) then
          ! Partition interior
          cell_distance = inner_halo_width + ( tile_x - 3 )*tile_size(1)
          direction = E
          actual_tile_size(1) = min( tile_size(1),          &
                                     num_interior_cells_x - &
                                     (tile_x - 3 )*tile_size(1) )
        else
          ! Partition with interior, inner halo, and edge
          cell_distance = ( tile_x - 2 )*tile_size(1)
          direction = E
          actual_tile_size(1) = min( tile_size(1),          &
                                     num_interior_cells_x + &
                                     2*inner_halo_width -   &
                                     ( tile_x - 2 )*tile_size(1) )
        end if
        do while ( cell_distance > 0  .and. row_start_cell /= 0 )
          row_start_cell = cell_next( direction, row_start_cell )
          cell_distance = cell_distance - 1
        end do

        if ( cell_distance /= 0 ) then
          success = .false.
          write( log_scratch_space, '(2(A,I0),A,1X,I0,1X,A)' )                 &
               'compute_coloured_tiling: Failed to reach first cell in tile (',&
               tile_x, ',', tile_y, ') with', cell_distance,                   &
               'steps left in x direction'
          call log_event( log_scratch_space, LOG_LEVEL_INFO )
        end if

        ! Same for y dimension
        if ( tile_y == 1 ) then
          ! Northern outer halo
          cell_distance = outer_halo_width
          direction = N
          actual_tile_size(2) = outer_halo_width
        else if ( tile_y == 2 .and. separate_inner_halo ) then
          ! Northern inner halo if required
          cell_distance = 0
          direction = N
          actual_tile_size(2) = inner_halo_width
        else if ( tile_y == max_ntiles_y - 1 .and. separate_inner_halo ) then
          ! Southern inner halo if required
          cell_distance = inner_halo_width + num_interior_cells_y
          direction = S
          actual_tile_size(2) = inner_halo_width
        else if ( tile_y == max_ntiles_y ) then
          ! Southern outer halo
          cell_distance = 2*inner_halo_width + num_interior_cells_y
          direction = S
          actual_tile_size(2) = outer_halo_width
        else if ( separate_inner_halo ) then
          ! Partition interior
          cell_distance = inner_halo_width + ( tile_y - 3 )*tile_size(2)
          direction = S
          actual_tile_size(2) = min( tile_size(2),          &
                                     num_interior_cells_y - &
                                     ( tile_y - 3 )*tile_size(2) )
        else
          ! Partition with interior, inner halo, and edge
          cell_distance = ( tile_y - 2 )*tile_size(2)
          direction = S
          actual_tile_size(2) = min( tile_size(2),          &
                                     num_interior_cells_y + &
                                     2*inner_halo_width -   &
                                     ( tile_y - 2 )*tile_size(2) )
        end if
        do while ( cell_distance > 0 .and. row_start_cell /= 0 )
          row_start_cell = cell_next( direction, row_start_cell )
          cell_distance = cell_distance - 1
        end do

        if ( cell_distance /= 0 ) then
          success = .false.
          write( log_scratch_space, '(2(A,I0),A,1X,I0,1X,A)' )                 &
               'compute_coloured_tiling: Failed to reach first cell in tile (',&
               tile_x, ',', tile_y, ') with', cell_distance,                   &
               'steps left in y direction'
          call log_event( log_scratch_space, LOG_LEVEL_INFO )
        end if

        if ( minval( actual_tile_size ) < 1 ) then
          success = .false.
          write( log_scratch_space, '(4(A,I4),A)' )                    &
               'compute_coloured_tiling: Tile (', tile_x, ',', tile_y, &
               'has erroneous size (', actual_tile_size(1), ',',       &
               actual_tile_size(2), ')'
          call log_event( log_scratch_space, LOG_LEVEL_INFO )
        end if

        tile = tile + 1

        ! Store cell IDs for this tile row-wise
        cell_counter = 0
        do j = 1, actual_tile_size(2)
          cell = row_start_cell
          do i = 1, actual_tile_size(1)
            cell_counter = cell_counter + 1
            cells_in_tile( cell_counter, tile ) = cell
            cell = cell_next( E, cell )
          end do
          row_start_cell = cell_next( S, row_start_cell )
        end do

        ! Sort cell IDs to allow loops to run up to certain halo or edge cells
        ! on partial tiles, as tiles do not necessarily coincide with
        ! halo structure
        call bubble_sort( cell_counter, cells_in_tile( :, tile ) )
        min_cellid_in_tile( tile ) = cells_in_tile( 1, tile )

        ! Set tile colour in a 2x2 pattern
        tile_colour( tile ) =     2 - mod( tile_x, 2 ) + &
                              2*( 1 - mod( tile_y, 2 ) )
      end do
    end do

    ! Tiling can lead to unused colours when inner halo is not tiled separately
    ! Remove any unused colours and reset tile colour to a 1,2,3 sequence to
    ! simplify further handling. This is an identity transformation if
    ! actual_colours is already a sequence that starts at 1.
    actual_colours = remove_duplicates(tile_colour)
    ntilecolours = size(actual_colours)
    call bubble_sort( ntilecolours, actual_colours )
    allocate( ntiles_per_colour(ntilecolours), source=0_i_def )
    do tile = 1, ntiles
      do colour = 1, ntilecolours
        if ( tile_colour(tile) == actual_colours(colour) ) then
          tile_colour(tile) = colour
          ntiles_per_colour(colour) = ntiles_per_colour(colour) + 1
          exit
        end if
      end do
    end do
    deallocate(actual_colours)

    do i = 1, ntilecolours
       write( log_scratch_space, '(A,1X,I1,A,1X,I6)' )                     &
            'compute_coloured_tiling: Number of tiles for colour', i, ':', &
            ntiles_per_colour(i)
       call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end do

    ! Tiles need to be sorted by min cell ID in each tile, to allow loops to
    ! run over certain cell ranges (edge, halos, ...)
    allocate( min_cellid_in_tile_idx(ntiles), source=(/ ( i, i=1, ntiles ) /) )
    call bubble_sort( ntiles, min_cellid_in_tile, min_cellid_in_tile_idx )

    allocate( ncells_per_coloured_tile( ntilecolours, maxval(ntiles_per_colour)&
                                      ), source=0_i_def )

    allocate( cells_in_coloured_tile( ntilecolours, maxval(ntiles_per_colour), &
              max_num_cells_in_tile ), source=0_i_def )

    ! Loop over tiles in order of min cell ID and fill tiling arrays
    ntiles_per_colour = 0_i_def
    do tile = 1, ntiles
      ! Retrieve index where original, unsorted tile data is stored
      j = min_cellid_in_tile_idx(tile)
      colour = tile_colour(j)
      ntiles_per_colour(colour) = ntiles_per_colour(colour) + 1

      ! Copy all valid cell IDs
      do cell = 1, max_num_cells_in_tile
        if ( cells_in_tile( cell, j ) == 0 ) exit
        ncells_per_coloured_tile( colour, ntiles_per_colour(colour) ) = &
             ncells_per_coloured_tile( colour, ntiles_per_colour(colour) ) + 1
        cells_in_coloured_tile( colour, ntiles_per_colour(colour), cell ) = &
             cells_in_tile( cell, j )
      end do
    end do

    deallocate( tile_active, cells_in_tile, min_cellid_in_tile )
    deallocate( min_cellid_in_tile_idx, tile_colour )

    return

  end function compute_coloured_tiling


  !> @brief  Initialises the index of the last cell in various specified regions
  !>         for each colour and tile
  !> @details In the PSy layer, we need to loop over various subsets of the mesh
  !>          such as up to a given halo depth (inner or outer) or up to the
  !>          last edge cell. In order for this to work with tiling, we need to
  !>          know the number of tiles and cells in each tile.
  !> @param[in] local_mesh  A pointer to the local mesh object that
  !>                        the mesh is built on.
  !> @param[in] ntilecolours Number of colours used for tiling
  !> @param[in] ntiles_per_colour Number of tiles in a given colour
  !> @param[in] ncells_per_coloured_tile Number of cells per tile and colour
  !> @param[in] cells_in_coloured_tile  List of cell IDs for each colour
  !>                                    and tile
  !> @param[out] last_inner_tile_per_colour Loop limit for tiles with inner
  !>                                        halo cells
  !> @param[out] last_inner_cell_per_coloured_tile Loop limit for inner
  !>                                               cells in a tile
  !> @param[out] last_edge_tile_per_colour Loop limit for tiles with edge cells
  !> @param[out] last_edge_cell_per_coloured_tile Loop limit for edge cells in
  !>                                              a tile
  !> @param[out] last_halo_tile_per_colour Loop limit for tiles with halo cells
  !> @param[out] last_halo_cell_per_coloured_tile Loop limit for halo cells in
  !>                                              a tile
  !============================================================================
  subroutine init_last_cell( local_mesh, ntilecolours, ntiles_per_colour,      &
                             ncells_per_coloured_tile, cells_in_coloured_tile, &
                             last_inner_tile_per_colour,                       &
                             last_inner_cell_per_coloured_tile,                &
                             last_edge_tile_per_colour,                        &
                             last_edge_cell_per_coloured_tile,                 &
                             last_halo_tile_per_colour,                        &
                             last_halo_cell_per_coloured_tile )

    implicit none

    type(local_mesh_type), pointer, intent(in) :: local_mesh
    integer(i_def), intent(in) :: ntilecolours
    integer(i_def), intent(in) :: ntiles_per_colour(:)
    integer(i_def), intent(in) :: ncells_per_coloured_tile(:,:)
    integer(i_def), intent(in) :: cells_in_coloured_tile(:,:,:)
    integer(i_def), intent(out), &
         allocatable :: last_inner_tile_per_colour(:,:)
    integer(i_def), intent(out), &
         allocatable :: last_inner_cell_per_coloured_tile(:,:,:)
    integer(i_def), intent(out), &
         allocatable :: last_edge_tile_per_colour(:)
    integer(i_def), intent(out), &
         allocatable :: last_edge_cell_per_coloured_tile(:,:)
    integer(i_def), intent(out), &
         allocatable :: last_halo_tile_per_colour(:,:)
    integer(i_def), intent(out), &
         allocatable :: last_halo_cell_per_coloured_tile(:,:,:)

    integer(i_def) :: max_ntiles_per_colour
    integer(i_def) :: inner_depth
    integer(i_def) :: halo_depth
    integer(i_def) :: last_edge_cell
    integer(i_def) :: depth
    integer(i_def) :: colour
    integer(i_def) :: tile
    integer(i_def) :: cell

    integer(i_def), allocatable :: last_inner_cells(:)
    integer(i_def), allocatable :: last_halo_cells(:)

    ! Use same number of tiles for all bounds arrays to simplify loop structure
    ! and avoid out-of-bounds access if there are no tiles for a given colour
    ! and region (interior, inner halo + edge, halo)
    max_ntiles_per_colour = maxval(ntiles_per_colour)

    ! Get max cells for each region and depth
    inner_depth = local_mesh%get_inner_depth()
    allocate( last_inner_cells( inner_depth ), source= &
         (/ ( local_mesh%get_last_inner_cell(depth), depth=1, inner_depth) /) )

    last_edge_cell = local_mesh%get_last_edge_cell()

    halo_depth = local_mesh%get_halo_depth()
    allocate( last_halo_cells( halo_depth ), source= &
         (/ ( local_mesh%get_last_halo_cell(depth), depth=1, halo_depth) /) )


    ! Tile/cell storage for inner halos
    allocate( last_inner_tile_per_colour( ntilecolours, inner_depth ),   &
              source=0_i_def )
    allocate( last_inner_cell_per_coloured_tile( ntilecolours,           &
                                                 max_ntiles_per_colour,  &
                                                 inner_depth ),          &
              source=0_i_def )

    ! Edge
    allocate( last_edge_tile_per_colour( ntilecolours ), source=0_i_def )
    allocate( last_edge_cell_per_coloured_tile( ntilecolours,            &
                                                max_ntiles_per_colour ), &
              source=0_i_def )

    ! Outer halos
    allocate( last_halo_tile_per_colour( ntilecolours, halo_depth ),     &
              source=0_i_def )
    allocate( last_halo_cell_per_coloured_tile( ntilecolours,            &
                                                max_ntiles_per_colour,   &
                                                halo_depth             ),&
              source=0_i_def)

    ! Loop over all colours, tiles, and valid cells inside each tile and
    ! set up tile and cell loop trip counts by finding the maximum tile and
    ! cell numbers with cell IDs below or at a given cell ID limit
    do colour = 1, ntilecolours
      do tile = 1, ntiles_per_colour(colour)
        do cell = 1, ncells_per_coloured_tile( colour, tile )

          do depth = 1, inner_depth
            if ( cells_in_coloured_tile( colour, tile, cell ) <= &
                 last_inner_cells(depth) ) then
              last_inner_cell_per_coloured_tile( colour, tile, depth ) = cell
              last_inner_tile_per_colour( colour, depth ) = tile
            end if
          end do

          if ( cells_in_coloured_tile( colour, tile, cell ) <= &
               last_edge_cell ) then
            last_edge_cell_per_coloured_tile( colour, tile ) = cell
            last_edge_tile_per_colour( colour ) = tile
          end if

          do depth = 1, halo_depth
            if ( cells_in_coloured_tile( colour, tile, cell ) <= &
                 last_halo_cells(depth) ) then
              last_halo_cell_per_coloured_tile( colour, tile, depth ) = cell
              last_halo_tile_per_colour( colour, depth ) = tile
            end if
          end do

        end do
      end do
    end do

    deallocate( last_inner_cells, last_halo_cells )

  end subroutine init_last_cell


  !============================================================================
  !> @brief  Check tiling for correctness - cells must appear only once, tiles
  !>         of the same colour must not be neighbours, tiles must be ordered
  !> @param[in] cell_next   Adjacency array for the mesh.
  !> @param[in] ncells_expected  Expected number of cells in tiling array
  !> @param[in] local_mesh  A pointer to the local mesh object that
  !>                        the mesh is built on.
  !> @param[in] ntiles Total number of tiles
  !> @param[in] ntilecolours Number of colours used for tiling
  !> @param[in] ntiles_per_colour Number of tiles in a given colour
  !> @param[in] ncells_per_coloured_tile Number of cells per tile and colour
  !> @param[in] cells_in_coloured_tile  List of cell IDs for each colour
  !>                                    and tile
  !> @param[in] last_inner_tile_per_colour Loop limit for tiles with inner
  !>                                       halo cells
  !> @param[in] last_inner_cell_per_coloured_tile Loop limit for inner
  !>                                              cells in a tile
  !> @param[in] last_edge_tile_per_colour Loop limit for tiles with edge cells
  !> @param[in] last_edge_cell_per_coloured_tile Loop limit for edge cells in
  !>                                             a tile
  !> @param[in] last_halo_tile_per_colour Loop limit for tiles with halo cells
  !> @param[in] last_halo_cell_per_coloured_tile Loop limit for halo cells in
  !>                                             a tile
  !> @param[in] separate_inner_halo  Inner halo is tiled separately
  !> @result    True if tiling is deemed correct
  !============================================================================
  function test_coloured_tiling( cell_next, ncells_expected, local_mesh,    &
                                 ntiles, ntilecolours, ntiles_per_colour,   &
                                 ncells_per_coloured_tile,                  &
                                 cells_in_coloured_tile,                    &
                                 last_inner_tile_per_colour,                &
                                 last_inner_cell_per_coloured_tile,         &
                                 last_edge_tile_per_colour,                 &
                                 last_edge_cell_per_coloured_tile,          &
                                 last_halo_tile_per_colour,                 &
                                 last_halo_cell_per_coloured_tile,          &
                                 separate_inner_halo ) result( is_correct )

    use remove_duplicates_mod, only : remove_duplicates

    implicit none

    integer(i_def), intent(in) :: cell_next(:,:)
    integer(i_def), intent(in) :: ncells_expected
    type(local_mesh_type), pointer, intent(in) :: local_mesh
    integer(i_def), intent(in) :: ntiles
    integer(i_def), intent(in) :: ntilecolours
    integer(i_def), intent(in) :: ntiles_per_colour(:)
    integer(i_def), intent(in) :: ncells_per_coloured_tile(:,:)
    integer(i_def), intent(in) :: cells_in_coloured_tile(:,:,:)
    integer(i_def), intent(in) :: last_inner_tile_per_colour(:,:)
    integer(i_def), intent(in) :: last_inner_cell_per_coloured_tile(:,:,:)
    integer(i_def), intent(in) :: last_edge_tile_per_colour(:)
    integer(i_def), intent(in) :: last_edge_cell_per_coloured_tile(:,:)
    integer(i_def), intent(in) :: last_halo_tile_per_colour(:,:)
    integer(i_def), intent(in) :: last_halo_cell_per_coloured_tile(:,:,:)
    logical(l_def), intent(in) :: separate_inner_halo
    logical(l_def)             :: is_correct

    integer(i_def), allocatable :: cell_list(:)
    integer(i_def) :: num_cells
    integer(i_def) :: colour
    integer(i_def) :: tile, other_tile
    integer(i_def) :: cell
    integer(i_def) :: cell_number, other_cell_number
    integer(i_def) :: cell_id, other_cell_id
    logical(l_def) :: match(5)
    logical(l_def) :: any_match
    logical(l_def) :: loop_ranges_correct
    integer(i_def) :: depth
    integer(i_def) :: last_cell
    integer(i_def) :: last_tile
    logical(l_def) :: tiling_crosses

    is_correct = .true.

    ! Check total number of tiles in ntiles_per_colour array
    if ( sum(ntiles_per_colour) /= ntiles ) then
      is_correct = .false.
      write( log_scratch_space, '(2(A,1X,I0,1X))' )                         &
           'test_coloured_tiling: tiling array was expected to contain', &
           ntiles, 'tiles, but found', sum(ntiles_per_colour)
      call log_event( log_scratch_space, LOG_LEVEL_INFO )
    end if

    ! Check total number of cells in ncells_per_coloured_tile array
    if ( sum(ncells_per_coloured_tile) /= ncells_expected ) then
      is_correct = .false.
      write( log_scratch_space, '(2(A,1X,I0,1X))' )                            &
           'test_coloured_tiling: tiling arrays were expected to cover',       &
           ncells_expected, 'cells, but found', sum(ncells_per_coloured_tile)
      call log_event( log_scratch_space, LOG_LEVEL_INFO )
    end if

    ! Check that the number of unique cell IDs in the cells_in_coloured_tile
    ! array matches the number of cells in the partition. Some entries may
    ! be zero.
    cell_list = remove_duplicates( reshape( cells_in_coloured_tile, &
                                          (/ size(cells_in_coloured_tile) /) ) )
    num_cells = size(cell_list)
    if ( count( cell_list == 0_i_def ) > 0 ) num_cells = num_cells - 1
    deallocate( cell_list )

    if ( num_cells /= ncells_expected ) then
      is_correct = .false.
      write( log_scratch_space, '(A,I0,A,I0,A)' )              &
           'test_coloured_tiling: Expected ', ncells_expected, &
           ' unique cell IDs but found ', num_cells
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    else
      write( log_scratch_space, '(A)' )                       &
           'test_coloured_tiling: All cells in partition ' // &
           'are accounted for in tiling array'
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end if

    ! Check that no tiles of the same colour are neighbours in the
    ! cells_in_coloured_tile array
    any_match = .false.
    do colour = 1, ntilecolours
      ! The test only makes sense if there are at least 2 tiles for this colour
      if ( ntiles_per_colour(colour) > 1 ) then
        do tile = 1, ntiles_per_colour(colour) - 1
          do other_tile = tile + 1, ntiles_per_colour(colour)

            ! Check each cell - we don't know which ones are at the tile edge
            do cell_number = 1, ncells_per_coloured_tile( colour, tile )
              cell_id = cells_in_coloured_tile( colour, tile, cell_number )

              ! Compare first cell with second cell and all its 8 neighbours
              ! The cell_next array used here has been reordered for consistent
              ! NESW direction in the partition.
              do other_cell_number = 1, &
                   ncells_per_coloured_tile( colour, other_tile )
                other_cell_id = cells_in_coloured_tile( colour, other_tile, &
                                                        other_cell_number )
                match = (/                                                   &
                   cell_id == other_cell_id,                                 &
                   cell_id == cell_next( N, other_cell_id ),                 &
                   cell_id == cell_next( E, other_cell_id ),                 &
                   cell_id == cell_next( S, other_cell_id ),                 &
                   cell_id == cell_next( W, other_cell_id ) /)
                any_match = any_match .or. any(match)

                if ( cell_next( E, other_cell_id ) > 0 ) then
                  any_match = any_match .or. &
                       cell_id == cell_next( N, cell_next( E, other_cell_id ) )
                  any_match = any_match .or. &
                       cell_id == cell_next( S, cell_next( E, other_cell_id ) )
                end if
                if ( cell_next( W, other_cell_id ) > 0 ) then
                  any_match = any_match .or. &
                       cell_id == cell_next( N, cell_next( W, other_cell_id ) )
                  any_match = any_match .or. &
                       cell_id == cell_next( S, cell_next( W, other_cell_id ) )
                end if

              end do
            end do
          end do
        end do
      end if
    end do
    if ( any_match ) then
      is_correct = .false.
      write( log_scratch_space, '(A)' ) 'test_coloured_tiling: Tiles of ' // &
           'the same colour either overlap or are direct neighbours'
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
    else
      write( log_scratch_space, '(A)' ) 'test_coloured_tiling: Tile ' // &
           'neighbourhood relations are correct'
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end if

    ! Check that loop ranges in the "last_..." arrays are correct, so that
    ! loops actually reach the correct cell ID
    loop_ranges_correct = .true.

    ! Inner halo
    do depth = 1, local_mesh%get_inner_depth()
      last_cell = local_mesh%get_last_inner_cell(depth)
      num_cells = 0
      do colour = 1, ntilecolours
        do tile = 1, last_inner_tile_per_colour( colour, depth )
          do cell = 1, last_inner_cell_per_coloured_tile( colour, tile, depth )
            num_cells = num_cells + 1
          end do
        end do
      end do
      if ( num_cells /= last_cell ) loop_ranges_correct = .false.
    end do

    ! Edge
    last_cell = local_mesh%get_last_edge_cell()
    num_cells = 0
    do colour = 1, ntilecolours
      do tile = 1, last_edge_tile_per_colour( colour )
        do cell = 1, last_edge_cell_per_coloured_tile( colour, tile )
          num_cells = num_cells + 1
        end do
      end do
    end do
    if ( num_cells /= last_cell ) loop_ranges_correct = .false.

    ! Outer halo
    do depth = 1, local_mesh%get_halo_depth()
      last_cell = local_mesh%get_last_halo_cell(depth)
      num_cells = 0
      do colour = 1, ntilecolours
        do tile = 1, last_halo_tile_per_colour( colour, depth )
          do cell = 1, last_halo_cell_per_coloured_tile( colour, tile, depth )
            num_cells = num_cells + 1
          end do
        end do
      end do
      if ( num_cells /= last_cell ) loop_ranges_correct = .false.
    end do

    if ( .not. loop_ranges_correct ) then
      is_correct = .false.
      write( log_scratch_space, '(A)' ) &
           'test_coloured_tiling: Tiled loops have incorrect trip count'
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
    else
      write( log_scratch_space, '(A)' ) &
           'test_coloured_tiling: Tiled loops have correct trip count'
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end if

    ! Check that no tile crosses the boundaries between partition interior and
    ! inner halo (if tiled separately), as well as edge and outer halo
    tiling_crosses = .false.
    if ( separate_inner_halo ) then
      ! Check boundary between interior and inner halo
      depth = local_mesh%get_inner_depth()
      last_cell = local_mesh%get_last_inner_cell(depth)
      do colour = 1, ntilecolours
        last_tile = last_inner_tile_per_colour( colour, depth )
        do tile = 1, ntiles_per_colour(colour)
          do cell = 1, ncells_per_coloured_tile( colour, tile )
            cell_id = cells_in_coloured_tile ( colour, tile, cell )
            if ( cell_id /= 0 ) then
              tiling_crosses = tiling_crosses .or. &
                   ( tile <= last_tile .and. cell_id > last_cell ) .or. &
                   ( tile > last_tile .and. cell_id <= last_cell )
            end if
          end do
        end do
      end do
    end if

    ! Check boundary between partition edge and outer halo
    last_cell = local_mesh%get_last_edge_cell()
    do colour = 1, ntilecolours
      last_tile = last_edge_tile_per_colour( colour )
      do tile = 1, ntiles_per_colour(colour)
        do cell = 1, ncells_per_coloured_tile( colour, tile )
          cell_id = cells_in_coloured_tile ( colour, tile, cell )
          if ( cell_id /= 0 ) then
            tiling_crosses = tiling_crosses .or. &
                 ( tile <= last_tile .and. cell_id > last_cell ) .or. &
                 ( tile > last_tile .and. cell_id <= last_cell )
          end if
        end do
      end do
    end do

    if ( tiling_crosses ) then
      is_correct = .false.
      write( log_scratch_space, '(A)' ) &
             'test_coloured_tiling: Tiling is not cleanly segmented'
      call log_event( log_scratch_space, LOG_LEVEL_WARNING )
    else
      write( log_scratch_space, '(A)' ) &
             'test_coloured_tiling: Tiling is cleanly segmented'
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
    end if

    return

  end function test_coloured_tiling

end module mesh_tiling_mod
