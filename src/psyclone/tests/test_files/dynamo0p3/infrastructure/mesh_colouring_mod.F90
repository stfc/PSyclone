!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Computes mesh colouring for vector spaces.
!>
!> @details Contains algorithms for colouring of meshes according to different
!>          traversal orders and different colouring policies.
!>          At present, greedy and balanced colouring are available for each
!>          mesh type.
!>
module mesh_colouring_mod

  use constants_mod,           only : i_def, i_native, l_def
  use local_mesh_mod,          only : local_mesh_type
  use log_mod,                 only : log_event, LOG_LEVEL_ERROR,   &
                                                 LOG_LEVEL_DEBUG,   &
                                                 log_scratch_space
  use reference_element_mod,   only : W, S, E, N

  implicit none

  private

  public :: set_colours

  integer, parameter               :: MAXCOLS = 50 ! Temporary hardcode until
                                                   ! dynamic palette
  integer(kind=i_def), target      :: cells_per_colour(MAXCOLS)

contains
  !---------------------------------------------------------------------------
  !>  @brief Sets colouring pattern for vector spaces.
  !>
  !>  @details Acquires details of cells in partition and applies colouring
  !>          to this space according to the specified colouring policy.
  !>
  !>  @param[in]  num_cells   Number of cells defined on the mesh.
  !>  @param[in]  cell_next   Adjacency array for the mesh.
  !>  @param[out] num_colours   Number of colours used.
  !>  @param[out] num_cell_per_colour   Count of cells in each colour
  !>  @param[out] cells_in_colour   List of cell indices in each colour
  !>  @param[in]  number_horizontal_faces Number of faces on the reference
  !>                      element intersected by a horizontal plane.
  !>  @param[in]  local_mesh A pointer to the local mesh object that
  !>                      the mesh is built on

  subroutine set_colours(num_cells,               &
                         cell_next,               &
                         num_colours,             &
                         num_cell_per_colour,     &
                         cells_in_colour,         &
                         number_horizontal_faces, &
                         local_mesh)

    implicit none

    integer(i_def),                 intent(in)    :: num_cells
    integer(i_def),                 intent(in)    :: cell_next(:,:)
    integer(i_def),                 intent(out)   :: num_colours
    integer(i_def), allocatable,    intent(out)   :: num_cell_per_colour(:)
    integer(i_def), allocatable,    intent(out)   :: cells_in_colour(:,:)
    integer(i_def),                 intent(in)    :: number_horizontal_faces
    type(local_mesh_type), pointer, intent(in)    :: local_mesh

    integer(i_def) :: i
    integer(i_def) :: npanels_global_mesh

    ! Colour routines return true or false depending on whether they worked
    logical(l_def) :: colour_ok

    colour_ok = .false.
    npanels_global_mesh = local_mesh%get_num_panels_global_mesh()
    if (npanels_global_mesh == 6) then
      ! 6 panel global mesh so assume a standard cubed sphere
      call set_colours_cubed_sphere(num_cells,    &
                        cell_next,               &
                        local_mesh,              &
                        num_colours,             &
                        num_cell_per_colour,     &
                        cells_in_colour,         &
                        number_horizontal_faces, &
                        colour_ok)

    else if (npanels_global_mesh == 1) then
      ! Single panel, so assume a quadrilateral mesh (LAM or biperiodic)
      if (test_mesh_planar_quadrilateral(num_cells, cell_next) &
          .eqv. .true.) then
        call set_colours_planar_quadrilateral(num_cells,       &
                        cell_next,               &
                        num_colours,             &
                        num_cell_per_colour,     &
                        cells_in_colour,         &
                        number_horizontal_faces, &
                        colour_ok)

      end if
    end if

    if (colour_ok .eqv. .false.) then
      ! Either npanels is not 1 or 6, or above algorithms have failed to
      ! colour correctly In all cases, use a generic colour algorithm.
      write(log_scratch_space,*) &
                           'set_colours: Applying generic colouring algorithm'
      call log_event(log_scratch_space,LOG_LEVEL_DEBUG)
      call set_colours_generic(num_cells,               &
                              cell_next,               &
                              num_colours,             &
                              num_cell_per_colour,     &
                              cells_in_colour,         &
                              number_horizontal_faces, &
                              colour_ok)

    end if

    ! Report the result when running with debug
    write(log_scratch_space,*) 'set_colours: Local mesh coloured with ', &
                               num_colours, ' colours'
    call log_event(log_scratch_space,LOG_LEVEL_DEBUG)

    do i=1,num_colours
      write(log_scratch_space,*) 'Number of cells in colour ',i, &
                                ' are ',num_cell_per_colour(i)
      call log_event(log_scratch_space,LOG_LEVEL_DEBUG)
    end do

  end subroutine set_colours

  !---------------------------------------------------------------------------
  !>  @brief Set colouring pattern for vector spaces on a cubed sphere.
  !>
  !>  A 4-colour colouring scheme is defined for the global cubed-sphere mesh
  !> and applied to the cells local to the partition. It assumes a consistent
  !> ordering of global cell IDs, but it tests the result and will return a
  !> failure condition if the result of the algorithm results in incorrect
  !> colouring.
  !>
  !>  @param[in]  num_cells   Number of cells on the locally partitioned mesh.
  !>  @param[in]  cell_next   Adjacency array for the mesh.
  !>  @param[in]  local_mesh A pointer to the local mesh object that
  !>                      the mesh is built on
  !>  @param[out] num_colours   Number of colours used to colour this mesh
  !>  @param[out] num_cell_per_colour   Count of cells in each colour
  !>  @param[out] cells_in_colour   List of cell indices in each colour
  !>  @param[in]  number_horizontal_faces Number of faces on the reference
  !>                      element intersected by a horizontal plane.
  !>  @param[out] colour_ok  Set to .true. if colouring was successful and
  !>                      .false. if not
  !>
  subroutine set_colours_cubed_sphere( num_cells,               &
                                       cell_next,               &
                                       local_mesh,              &
                                       num_colours,             &
                                       num_cell_per_colour,     &
                                       cells_in_colour,         &
                                       number_horizontal_faces, &
                                       colour_ok )

    implicit none

    integer(i_def),              intent(in)    :: num_cells
    integer(i_def),              intent(in)    :: cell_next(:,:)
    type(local_mesh_type), pointer, intent(in) :: local_mesh
    integer(i_def),              intent(out)   :: num_colours
    integer(i_def), allocatable, intent(out)   :: num_cell_per_colour(:)
    integer(i_def), allocatable, intent(out)   :: cells_in_colour(:,:)
    integer(i_def),              intent(in)    :: number_horizontal_faces
    logical(l_def),              intent(inout) :: colour_ok

    ! Local Variables
    ! Stores error status from allocate statement
    integer(i_native) :: astat

    ! Array holding the colour of each cell.
    integer(i_def), allocatable :: colour_map(:)

    ! Loop and status variables
    integer(i_def) :: i
    integer(i_def) :: cell

    ! Cube dimensions
    integer(i_def) :: ncells_per_panel
    integer(i_def) :: ncells_per_dimension
    ! Global cell ID
    integer(i_def) :: gid
    ! Cell ID converted to global panel number and cell number of panel
    integer(i_def) :: panel_number
    integer(i_def) :: gid_relative_to_panel

    ! Size of colour map array
    integer(i_def) :: max_cell
    ! The colour order for a 2x2 block of cells at the corner of each panel
    ! The pattern is simply repeated over the whole of each panel
    integer(i_def) :: cell_colour_order_per_panel(2,2,6)
    integer(i_def) :: row,column

    ! Get dimensions of cubed sphere from the total cell number and check it
    ! makes sense
    ncells_per_panel = local_mesh%get_ncells_global_mesh()/6
    ncells_per_dimension = nint( sqrt (real(ncells_per_panel)))

    if (local_mesh%get_ncells_global_mesh() /= &
          (ncells_per_dimension**2) * 6) then
      write(log_scratch_space,'(a,i0,a)')                 &
        'set_colours_cubed_sphere: cells in global map ', &
        local_mesh%get_ncells_global_mesh(),              &
        ' should be a square multiplied by 6'
      call log_event(log_scratch_space,LOG_LEVEL_ERROR)
      stop
    end if

    ! Cubed sphere colour order is defined for the global mesh based on tiling
    ! each panel with the same 2x2 colour pattern. Note that it is different
    ! for even and odd dimensioned meshes. It assumes the cell number order
    ! follows the following example pattern for a cube with 2x2 cells per panel
    !       -------
    !       |18 20|
    !       |17 19|
    ! -------------------------
    ! |13 14| 1  2| 5  6| 9 10|
    ! |15 16| 3  4| 7  8|11 12|
    ! -------------------------
    !       |23 21|
    !       |24 22|
    !       -------
    if (mod(ncells_per_dimension,2) == 0) then
      ! Even number of cells per edge
      ! Even number of cells per edge
      !     -----
      !     |2 1|
      !     |3 4|
      ! -----------------
      ! |1 4|1 2|3 2|3 4|
      ! |3 2|3 4|1 4|1 2|
      ! -----------------
      !     |1 2|
      !     |4 3|
      !     -----
      cell_colour_order_per_panel(:,:,1) = reshape((/1,2,3,4/),(/2,2/))
      cell_colour_order_per_panel(:,:,2) = reshape((/3,2,1,4/),(/2,2/))
      cell_colour_order_per_panel(:,:,3) = reshape((/3,4,1,2/),(/2,2/))
      cell_colour_order_per_panel(:,:,4) = reshape((/1,4,3,2/),(/2,2/))
      cell_colour_order_per_panel(:,:,5) = reshape((/3,2,4,1/),(/2,2/))
      cell_colour_order_per_panel(:,:,6) = reshape((/2,3,1,4/),(/2,2/))
    else
      ! Odd number of cells per edge
      !       -------
      !       |3 4 3|
      !       |2 1 2|
      !       |3*4 3|
      ! -------------------------
      ! |1*4 1|1 2 1|4*1 4|1*2 1|
      ! |2 3 2|3 4 3|2 3 2|3 4 3|
      ! |1 4 1|1 2 1|4 1 4|1 2 1|
      ! -------------------------
      !       |3 4 3|
      !       |2 1 2|
      !       |3 4 3|
      !       -------
      cell_colour_order_per_panel(:,:,1) = reshape((/1,2,3,4/),(/2,2/))
      cell_colour_order_per_panel(:,:,2) = reshape((/4,1,2,3/),(/2,2/))
      cell_colour_order_per_panel(:,:,3) = reshape((/1,2,3,4/),(/2,2/))
      cell_colour_order_per_panel(:,:,4) = reshape((/4,1,2,3/),(/2,2/))
      cell_colour_order_per_panel(:,:,5) = reshape((/3,2,4,1/),(/2,2/))
      cell_colour_order_per_panel(:,:,6) = reshape((/3,2,4,1/),(/2,2/))
    end if

    ! Determine largest neighbour element that will be a subscript to cell_next
    max_cell = maxval(cell_next(:number_horizontal_faces, :))

    ! colour_map is only set to a colour for 1 to num_cells, but needs to be
    ! allocated larger to support the testing algorithm in test_set_colours
    allocate(colour_map(0:max_cell), stat=astat)
    if(astat/=0) then
      call log_event( 'set_colours_cubed_sphere: Allocate failure: colour_map.', &
                      LOG_LEVEL_ERROR )
      ! Although the logger will call "stop" for errors GFortran is unable to
      ! perform inter-file analysis so believes it is possible to use
      ! colour_map without initialising it.
      ! The following "stop" will never be reached but it lets GFortran know
      ! that colour_map will always be initialised.
      stop
      !> @todo Keep an eye on GFortran development and remove uncalled "stop"
      !>       when it is no longer necessary to satisfy "Uninitialised
      !>       variable may be used" warning. This warning is only seen for
      !>       the "production" target.
    end if
    colour_map = 0_i_def

    ! Loop over local cells
    do cell = 1, num_cells
      ! The global cell ID identifies where on the mesh it is
      gid = local_mesh%get_gid_from_lid(cell)
      panel_number = (gid - 1) / ncells_per_panel + 1

      ! Get the panel number and work out whether it is an even/odd row and
      ! even/odd column
      gid_relative_to_panel = gid - (ncells_per_panel * (panel_number - 1))
      column = 2 - mod (mod(gid_relative_to_panel,ncells_per_dimension),2)
      row    = mod ((gid_relative_to_panel-1)/ncells_per_dimension,2) + 1

      ! The colour can be looked up now
      colour_map(cell) = cell_colour_order_per_panel(column, row, panel_number)
    end do

    do i=1,MAXCOLS
      cells_per_colour(i) = 0_i_def
    end do

    ! Check that colouring has been done correctly
    call test_set_colours(num_cells,             &
                        cell_next,              &
                        colour_map,             &
                        colour_ok)

    if (colour_ok) then
      ! Number of colours is fixed and is an output of this subroutine
      num_colours = 4
      ! Create cells_in_colour lookup array that list all cells of each colour
      call create_colour_map_lookup (num_cells, &
                                    num_colours,            &
                                    colour_map,             &
                                    num_cell_per_colour,    &
                                    cells_in_colour)
    end if
    deallocate(colour_map)

  end subroutine set_colours_cubed_sphere

  !---------------------------------------------------------------------------
  !>  @brief Sets colouring pattern for vector spaces on a planar mesh with
  !>         optional periodicity
  !>
  !> A planar rectangular mesh can be coloured with 4 colours by this
  !> subroutine unless it is periodic in one or both directions, and has an
  !> odd number of cells in a direction that is periodic.
  !>
  !> @param[in]  num_cells   Number of cells on the locally partitioned mesh.
  !> @param[in]  cell_next   Adjacency array for the mesh.
  !> @param[out] num_colours   Number of colours used to colour this mesh
  !> @param[out] num_cell_per_colour   Count of cells in each colour
  !> @param[out] cells_in_colour   List of cell indices in each colour
  !> @param[in]  number_horizontal_faces Number of faces on the reference
  !>                     element intersected by a horizontal plane.
  !> @param[out] colour_ok  Set to .true. if colouring was successful and
  !>                        .false. if not
  !>
  subroutine set_colours_planar_quadrilateral( num_cells,               &
                                               cell_next,               &
                                               num_colours,             &
                                               num_cell_per_colour,     &
                                               cells_in_colour,         &
                                               number_horizontal_faces, &
                                               colour_ok )
    implicit none
    integer(i_def),              intent(in)    :: num_cells
    integer(i_def),              intent(in)    :: cell_next(:,:)
    integer,                     intent(out)   :: num_colours
    integer(i_def), allocatable, intent(out)   :: num_cell_per_colour(:)
    integer(i_def), allocatable, intent(out)   :: cells_in_colour(:,:)
    integer(i_def),              intent(in)    :: number_horizontal_faces
    logical(l_def),              intent(inout) :: colour_ok

    ! Supports looping over faces
    integer(i_def),parameter :: direction(4) = (/N,E,S,W/)

    ! Local Variables
    ! Stores error status from allocate statement
    integer(i_native) :: astat

    ! Array holding the colour of each cell
    integer(kind=i_def), allocatable :: colour_map(:)

    ! Loop variables
    integer(i_def) :: i
    integer(i_def) :: cell
    integer(i_def) :: dir

    ! Colour of neighbour cell relative to cell colour and direction
    integer(i_def) :: cell_next_colour(4,4)
    ! Size of colour map array
    integer(i_def) :: max_cell
    ! Algorithm will colour cells in the to-do list while adding un-coloured
    ! neighbours to the end of the to-do list as it goes
    integer(i_def) :: cell_list_todo_neighbours(num_cells)
    ! These variables delimit chunks of cells in the to-do list that will be
    ! done next
    integer(i_def) :: cell_list_todo_start_pos
    integer(i_def) :: cell_list_todo_end_pos
    ! Support variables
    integer(i_def) :: neighbour_cell
    integer(i_def) :: coloured_cell

    ! Determine largest neighbour element that will be a subscript to cell_next
    max_cell = maxval(cell_next(:number_horizontal_faces, :))

    ! colour_map is only set to a colour for 1 to num_cells, but needs to be
    ! allocated larger to support the testing algorithm in test_set_colours
    ! Additionally, cell_next=0 for some off-partition cells. colour_map(0)
    ! is set to zero to identify these efficiently
    allocate(colour_map(0:max_cell), stat=astat)
    if(astat/=0) then
      call log_event( 'set_colours_planar_quadrilateral: Allocate failure: colour_map.', &
                      LOG_LEVEL_ERROR )
      ! Although the logger will call "stop" for errors GFortran is unable to
      ! perform inter-file analysis so believes it is possible to use
      ! colour_map without initialising it.
      ! The following "stop" will never be reached but it lets GFortran know
      ! that colour_map will always be initialised.
      stop
      !> @todo Keep an eye on GFortran development and remove uncalled "stop"
      !>       when it is no longer necessary to satisfy "Uninitialised
      !>       variable may be used" warning. This warning is only seen for
      !>       the "production" target.
    end if
    colour_map = 0_i_def

    ! Defines which colour to set neighbour cells in different directions
    ! relative to colour of cell. For example, the cell to the North (dir(1))
    ! of a cell of colour 2 will be:
    !     cell_next_colour( dir(1), 2) = 1
    cell_next_colour(:,1) = (/2,3,2,3/)
    cell_next_colour(:,2) = (/1,4,1,4/)
    cell_next_colour(:,3) = (/4,1,4,1/)
    cell_next_colour(:,4) = (/3,2,3,2/)

    ! This is a list of cells whose neighbours need colouring
    cell_list_todo_neighbours(:) = 0
    ! Start with cell 1 and a list of neighbours comprising only cell 1
    cell = 1
    cell_list_todo_neighbours(1) = cell
    cell_list_todo_start_pos = 1
    cell_list_todo_end_pos = 1
    do while (cell <= num_cells)
      ! Here, cell has no neighbours that have been coloured, so give it
      ! colour 1
      colour_map(cell) = 1
      ! Colour all the neighbour cells then the neighbours of neighbour cells
      ! until all neighbours of neighbours have been coloured
      do while (cell_list_todo_start_pos <= cell_list_todo_end_pos)
        ! Loop over the last set of cells added to the coloured neighbours list
        do i = cell_list_todo_start_pos,cell_list_todo_end_pos
          coloured_cell = cell_list_todo_neighbours(i)

          ! Colour the four edge neighbours if they are not already coloured
          do dir = 1,4
            neighbour_cell = cell_next(direction(dir),coloured_cell)
            if (neighbour_cell > 0 .and. neighbour_cell <= num_cells) then
              ! Neighbour cell is within this partition
              if (colour_map(neighbour_cell) == 0) then
                ! Neighbour cell needs to be coloured
                colour_map(neighbour_cell)                         &
                               = cell_next_colour( direction(dir), &
                                                   colour_map(coloured_cell) )

                ! Add neighbour cell to the list of cells whose neighbours
                ! need to be checked
                cell_list_todo_end_pos = cell_list_todo_end_pos + 1
                cell_list_todo_neighbours(cell_list_todo_end_pos) &
                                                              = neighbour_cell
              end if
            end if
          end do
        end do
        ! Reset the start position to the start of the next set of cells to
        ! colour around
        cell_list_todo_start_pos = i
      end do
      ! Find any remaining blank cells. If the partition is contiguous there
      ! should be none. If the partition is not contiguous we will colour each
      ! separated sub-partition in the same way
      do while (colour_map(cell) /= 0)
        cell = cell + 1
        if (cell > num_cells) exit
      end do
    end do

    ! Check that colouring was done correctly.
    call test_set_colours(num_cells,             &
                        cell_next,              &
                        colour_map,             &
                        colour_ok)

    if (colour_ok) then
      ! Number of colours is an output of this subroutine
      num_colours = 4
      call create_colour_map_lookup (num_cells, &
        num_colours,            &
        colour_map,             &
        num_cell_per_colour,    &
        cells_in_colour)
    end if
    deallocate(colour_map)

  end subroutine set_colours_planar_quadrilateral

  !---------------------------------------------------------------------------
  !> @brief Sets colouring pattern for generic vector spaces.
  !>
  !> Generic routine which for each cells on the local domain, creates a list
  !> of colours of its neighbours and colours the cell with a different
  !> colour.
  !>
  !> @param[in]  num_cells   Number of cells on the locally partitioned mesh.
  !> @param[in]  cell_next   Adjacency array for the mesh.
  !> @param[out] num_colours Number of colours used to colour this mesh
  !> @param[out] num_cell_per_colour   Count of cells in each colour
  !> @param[out] cells_in_colour   List of cell indices in each colour
  !> @param[in]  number_horizontal_faces  Number of faces on the reference
  !>                     element intersected by a horizontal plane.
  !> @param[out] colour_ok  Set to .true. if colouring was successful and
  !>                        .false. if not
  !>
  subroutine set_colours_generic( num_cells,               &
                                  cell_next,               &
                                  num_colours,             &
                                  num_cell_per_colour,     &
                                  cells_in_colour,         &
                                  number_horizontal_faces, &
                                  colour_ok )
    implicit none
    integer(i_def),              intent(in)    :: num_cells
    integer(i_def),              intent(in)    :: cell_next(:,:)
    integer,                     intent(out)   :: num_colours
    integer(i_def), allocatable, intent(out)   :: num_cell_per_colour(:)
    integer(i_def), allocatable, intent(out)   :: cells_in_colour(:,:)
    integer(i_def),              intent(in)    :: number_horizontal_faces
    logical(l_def),              intent(inout) :: colour_ok

    ! Local Variables
    ! Stores error status from allocate statement
    integer                           :: astat

    ! Array holding the colour of each cell. Index 0 cell holds 0 value for
    ! elements having fewer than maximum count of neighbours.
    integer(i_def), allocatable        :: colour_map(:)

    ! Array for marking used (unavailable) colours for a cell
    integer(i_def)                     :: used_colours(0:MAXCOLS)
    ! The next available colour
    integer(i_def)                     :: free_colour
    ! Loop and status variables
    integer(i_def)                     :: i
    integer(i_def)                     :: test_cell, cell
    ! Size of colour map array
    integer(i_def)                     :: max_cell

    ! Determine largest neighbour element that will be a subscript to cell_next
    max_cell = maxval(cell_next(:number_horizontal_faces, :))

    ! colour_map is only set to a colour for 1 to num_cells, but needs to be
    ! allocated larger to support the algorithm here and in test_set_colours
    ! which looks for neighbours that are outside the local domain.
    ! Additionally, cell_next=0 for some off-partition cells. colour_map(0)
    ! is set to zero to identify these efficiently
    allocate(colour_map(0:max_cell), stat=astat)
    if(astat/=0) then
      call log_event( 'set_colours_generic: Allocate failure: colour_map.', &
                      LOG_LEVEL_ERROR )
      ! Although the logger will call "stop" for errors GFortran is unable to
      ! perform inter-file analysis so believes it is possible to use
      ! colour_map without initialising it.
      ! The following "stop" will never be reached but it lets GFortran know
      ! that colour_map will always be initialised.
      stop
      !> @todo Keep an eye on GFortran development and remove uncalled "stop"
      !>       when it is no longer necessary to satisfy "Uninitialised
      !>       variable may be used" warning. This warning is only seen for the
      !>       "production" target.
    end if

    colour_map = 0_i_def
    cells_per_colour = 0_i_def

    do cell = 1, num_cells
      do i=0, MAXCOLS
        used_colours(i) = 0_i_def
      end do

      test_cell = cell_next(N, cell)
      if(test_cell > 0) then
        used_colours(colour_map(test_cell)) = 1

        ! Cardinal point orientation may vary when crossing a panel boundary
        ! for cubed-sphere mesh types.
        if(cell == cell_next(S, test_cell)) then
          used_colours(colour_map(cell_next(E, test_cell))) = 1
          used_colours(colour_map(cell_next(W, test_cell))) = 1
        else
          used_colours(colour_map(cell_next(N, test_cell))) = 1
          used_colours(colour_map(cell_next(S, test_cell))) = 1
        end if
      end if

      test_cell = cell_next(E, cell)
      if(test_cell > 0) used_colours(colour_map(test_cell)) = 1

      test_cell = cell_next(W, cell)
      if(test_cell > 0) used_colours(colour_map(test_cell)) = 1

      test_cell = cell_next(S, cell)
      if(test_cell > 0) then
        used_colours(colour_map(test_cell)) = 1

        ! Cardinal point orientation may vary when crossing a panel boundary
        ! for cubed-sphere mesh types.
        if(cell == cell_next(N, test_cell)) then
          used_colours(colour_map(cell_next(E, test_cell))) = 1
          used_colours(colour_map(cell_next(W, test_cell))) = 1
        else
          used_colours(colour_map(cell_next(N, test_cell))) = 1
          used_colours(colour_map(cell_next(S, test_cell))) = 1
        end if
      end if

      free_colour = choose_colour_greedy(used_colours)
      ! Alternate colour choice procedures may be applied here, e.g...
      ! free_colour = choose_colour_balanced(used_colours)
      colour_map(cell) = free_colour
      cells_per_colour(free_colour) = cells_per_colour(free_colour) + 1
    end do

    num_colours = MAXCOLS
    ! Allocate return data and populate
    do i = 1, MAXCOLS
      if(cells_per_colour(i) == 0) then
        num_colours = i-1
        exit
      end if
    end do

    call test_set_colours(num_cells,             &
                        cell_next,              &
                        colour_map,             &
                        colour_ok)

    if (colour_ok) then
      ! Create cells_in_colour lookup array that list all cells of each colour
      call create_colour_map_lookup (num_cells,           &
                                    num_colours,         &
                                    colour_map,          &
                                    num_cell_per_colour, &
                                    cells_in_colour)
    end if
    deallocate(colour_map)

  end subroutine set_colours_generic

  !---------------------------------------------------------------------------
  !> @brief Tests whether all cells have same orientation as their neighbour.
  !>
  !> The planar mesh colouring requires all cells are oriented the same way as
  !> their edge neighbours. This tests the mesh beforehand
  !>
  !> @param[in]  num_cells   Number of cells on the locally partitioned mesh.
  !> @param[in]  cell_next   Adjacency array for the mesh.
  !>
  !> @return Indicates whether or not the mesh was planar.
  !>
  function test_mesh_planar_quadrilateral( num_cells, &
                                           cell_next) result (planar)
    implicit none
    integer(i_def), intent(in)                    :: num_cells
    integer(i_def), intent(in)                    :: cell_next(:,:)
    logical(l_def)                                :: planar

    integer(i_def)                   :: cell
    integer(i_def)                   :: test_cell
    integer(i_def)                   :: dir
    integer(i_def),parameter         :: direction(4) = (/N, E, S, W/)
    integer(i_def),parameter         :: opposite(4)  = (/S, W, N, E/)

    ! Assume a planar mesh until we find an anomaly
    planar = .true.
    do cell = 1, num_cells
      do dir = 1,4
        test_cell = cell_next(direction(dir), cell)
        ! Test only when the neighbouring cell is on the local domain
        if (test_cell > 0 .and. test_cell <= num_cells) then
          ! If we go E then W, N then S etc. from cell we should get back to
          ! cell
          if (cell_next(opposite(dir),test_cell) /= cell) planar = .false.
        end if
      end do
      if (planar .eqv. .false.) exit
    end do

  end function test_mesh_planar_quadrilateral

  !---------------------------------------------------------------------------
  !> @brief Takes a map of colours and creates list and count of cells of each
  !>        colour.
  !>
  !> @param[in]  num_cells   Number of cells on the locally partitioned mesh.
  !> @param[in]  num_colours Number of colours used to colour this mesh
  !> @param[in]  colour_map  Map providing colour of each cell.
  !> @param[out] num_cell_per_colour  Count of cells in each colour
  !> @param[out] cells_in_colour  List of cell indices in each colour
  !>
  subroutine create_colour_map_lookup ( num_cells,           &
                                        num_colours,         &
                                        colour_map,          &
                                        num_cell_per_colour, &
                                        cells_in_colour )
    implicit none
    integer(i_def),              intent(in)       :: num_cells
    integer(i_def),              intent(in)       :: num_colours
    integer(i_def),              intent(in)       :: colour_map(0:)
    integer(i_def), allocatable, intent(out)      :: num_cell_per_colour(:)
    integer(i_def), allocatable, intent(out)      :: cells_in_colour(:,:)

    ! Prefix for allocation error messages
    character(len=*), parameter :: prefix &
                                  = "[create_colour_map] Failure to allocate "

    integer                           :: astat
    integer(i_def)                    :: colour, cell, i

    allocate(num_cell_per_colour(num_colours), stat=astat)

    if(astat/=0) call log_event(prefix//"num_cell_per_colour.", &
                                  LOG_LEVEL_ERROR)
    do i = 1, num_colours
      num_cell_per_colour(i) = 0
    end do

    ! Output 1: Count the number of cells of each colour
    do cell=1,num_cells
      num_cell_per_colour(colour_map(cell)) &
              = num_cell_per_colour(colour_map(cell)) + 1
    end do

    allocate(cells_in_colour(num_colours, maxval(num_cell_per_colour)), &
            stat=astat)
    if(astat/=0) call log_event(prefix//"cells_in_colour.", &
                                  LOG_LEVEL_ERROR)
    cells_in_colour = 0_i_def

    ! Output 2: Create a list of cells for each colour
    do colour = 1, num_colours
      i = 0_i_def
      do cell = 1, num_cells
        if (colour_map(cell) == colour) then
          i = i+1
          cells_in_colour(colour, i) = cell
        end if
      end do
    end do
  end subroutine create_colour_map_lookup

  !---------------------------------------------------------------------------
  ! Tests whether the mesh has been coloured correctly.
  !
  ! For each cell, checks that the cells that share edges and vertices do not
  ! have the same colour as the original cell
  !
  ! num_cells   Number of cells on the locally partitioned mesh.
  ! cell_next   Adjacency array for the mesh.
  ! colour_map  Map providing colour of each cell.
  ! colour_ok   Set to .true. if colouring was successful and .false. if not.
  !
  subroutine test_set_colours(num_cells, &
                        cell_next,              &
                        colour_map,             &
                        colour_ok)
    implicit none
    integer(i_def), intent(in)                    :: num_cells
    integer(i_def), intent(in)                    :: cell_next(:,:)
    integer(i_def), intent(in)                    :: colour_map(0:)
    logical(l_def), intent(out)                   :: colour_ok

    integer(i_def)                     :: cell
    integer(i_def)                     :: test_cell
    integer(i_def)                     :: colour

    ! Test the result by checking all neighbours of each cell is a different
    ! colour where the neighbour is outside of the domain, cell_next returns
    ! zero and colour_map(0)=0
    colour_ok = .true.
    do cell = 1, num_cells
      colour = colour_map(cell)

      ! Check the colours of the cells that share an edge
      if (colour == colour_map(cell_next(N, cell)) .or. &
          colour == colour_map(cell_next(S, cell)) .or. &
          colour == colour_map(cell_next(E, cell)) .or. &
          colour == colour_map(cell_next(W, cell))      &
        ) then
        colour_ok = .false.
      end if

      ! Check the colours of the cells that share only a vertex
      test_cell = cell_next(N, cell)
      if(test_cell > 0) then

        ! To check the (up to 4) cells that share only vertices we need
        ! to go North then left and right, and then South then left and
        ! right. However, left and right directions depend on
        ! orientation of the N/S cell relative to cell. This is tested
        ! for by going N then S and seeing if we get back to the same
        ! cell.

        if(cell == cell_next(S, test_cell)) then
          if (colour == colour_map(cell_next(E, test_cell)) .or. &
              colour == colour_map(cell_next(W, test_cell))) then
            colour_ok = .false.
          end if
        else
          if (colour == colour_map(cell_next(N, test_cell)) .or. &
              colour == colour_map(cell_next(S, test_cell))) then
            colour_ok = .false.
          end if
        end if
      end if ! if (test_cell > 0) for test_cell = cell_next(N, cell)

      test_cell = cell_next(S, cell)
      if (test_cell > 0) then
        ! Cardinal point orientation may vary when crossing a panel boundary
        ! for cubed-sphere mesh types.
        if(cell == cell_next(N, test_cell)) then
          if (colour == colour_map(cell_next(E, test_cell)) .or. &
              colour == colour_map(cell_next(W, test_cell))) then
            colour_ok = .false.
          end if
        else
          if (colour == colour_map(cell_next(N, test_cell)) .or. &
              colour == colour_map(cell_next(S, test_cell))) then
            colour_ok = .false.
          end if
        end if
      end if ! if (test_cell > 0) for test_cell = cell_next(S, cell)
    end do ! Loop over cells

  end subroutine test_set_colours

  !---------------------------------------------------------------------------
  ! Choose next available colour using greedy algorithm
  !
  ! Function to select the next available colour; creates an imbalanced
  ! palette using a minimal number of colours.
  !
  ! used_colours  array of colours already used.
  ! colour        integer signifying the chosen colour.
  !
  pure function choose_colour_greedy(used_colours)  result(colour)

    implicit none

    integer, intent(in)     :: used_colours(0:MAXCOLS)
    integer                 :: colour
    integer                 :: idx

    colour = 0
    do idx=1, MAXCOLS
      if(used_colours(idx) == 0) then
        colour = idx
        exit
      endif
    end do

  end function choose_colour_greedy

  !---------------------------------------------------------------------------
  ! Choose next available colour using balanced algorithm.
  !
  ! Function to select the next available colour; creates a balanced palette
  ! using all available colours.
  !
  ! used_colours array of colours already used
  !
  ! Returns chosen colour.
  !
  pure function choose_colour_balanced(used_colours)  result(colour)

    implicit none

    integer, intent(in)     :: used_colours(0:MAXCOLS)
    integer                 :: colour

    colour = minloc(cells_per_colour, 1, mask=(used_colours(1:MAXCOLS)==0))

  end function choose_colour_balanced

end module mesh_colouring_mod
