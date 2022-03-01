module read_config_mod

    private
    public :: read_config

contains

! ----------------------------------------------------------------------------
subroutine read_config(grid, initial, time_steps)

    USE grid_mod, only           : grid_type, grid_init, GO_ARAKAWA_C,     &
                                   GO_BC_EXTERNAL, GO_BC_NONE, GO_OFFSET_SW
    use parallel_mod, only       : parallel_init
    USE field_mod, only          : r2d_field, go_t_points

    implicit none
    TYPE(grid_type), intent(out), target &
                                 :: grid
    TYPE(r2d_field), intent(out) :: initial
    integer, intent(out)         :: time_steps

    character(len=256)           :: filename
    integer                      :: n_rows, n_cols
    real(kind=8), dimension(:,:), allocatable :: initial_state

    call getarg(1, filename)

    print *,"Reading [", trim(filename),"]"

    open (unit=15, file=filename, status='old',    &
          access='sequential', form='formatted', action='read')

    read (15, *)   ! Skip comment row
    ! Read number of columns, number of rows, and number of timesteps
    read (15, *) n_cols, n_rows, time_steps

    ! Now initialise dl_esm_inf
    ! =========================

    ! 1) Initialise MPI - if dl_esm_inf is compiled with MPI
    call parallel_init()

    ! 2) Create the information about the grid type to be used.
    grid = grid_type(GO_ARAKAWA_C,                                  &
                     (/GO_BC_EXTERNAL,GO_BC_EXTERNAL,GO_BC_EXTERNAL/),  &
                     GO_OFFSET_SW)

    ! 3) Create the domain decomposition - we are using 4 process,
    !    allow for a boundary size of 1
    call grid%decompose(n_cols, n_rows, ndomains=1, &
                        halo_width=1)

    ! 4) Grid init
    call grid_init(grid, dxarg=1.0_8, dyarg=1.0_8)

    ! Now we can use the grid to create the field that stores the
    ! initial data.
    allocate(initial_state(n_rows, n_cols))
    initial_state = 0.0

    call get_initial_state(initial_state, n_rows, n_cols)

    initial = r2d_field(grid, grid_points = GO_T_POINTS,   &
                        init_global_data = initial_state)

    close(15)

end subroutine read_config

! ----------------------------------------------------------------------------
subroutine get_initial_state(initial_state, n_rows, n_cols)
    USE field_mod, only            : r2d_field
    implicit none
    real(kind=8), dimension(:,:), allocatable :: initial_state
    integer, intent(in)            :: n_rows, n_cols

    integer :: line_count, ios, x, y

    line_count = 0

    do
        read(15, *, iostat=ios) x, y
        ! Read till end of file
        if (ios <0) then
            exit
        endif
        if (x>=1 .and. x<=n_cols .and. y>=1 .and. y<=n_rows) then
            initial_state(x, y) = 1.0_8
        else
            print *,"Ignoring line ",line_count,": ",x, y
        endif

        line_count = line_count + 1
    enddo

end subroutine get_initial_state

end module read_config_mod