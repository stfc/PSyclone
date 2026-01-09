module read_config_mod

    private
    public :: read_config

contains

! ----------------------------------------------------------------------------
subroutine read_config(initial, time_steps)

    !> @brief Reads the configuration file and initialised dl_esm_inf
    !> and the grid.
    !>
    !> This subroutine reads the configuration file specified on the command
    !> line. It takes the grid size from the file, initialises dl_esm_inf and
    !> creates an appropriate grid. It then fills the field `initial` with
    !> the initial condition taken from the file.

    implicit none
    real(kind=8), dimension(:,:), intent(inout), allocatable :: initial
    integer, intent(out)         :: time_steps

    character(len=256)           :: filename
    integer                      :: n_rows, n_cols

    call getarg(1, filename)

    print *,"Reading [", trim(filename),"]"

    open (unit=15, file=filename, status='old',    &
          access='sequential', form='formatted', action='read')

    read (15, *)   ! Skip comment row
    ! Read number of columns, number of rows, and number of timesteps
    read (15, *) n_cols, n_rows, time_steps

    ! Now we can use the grid to create the field that stores the
    ! initial data.
    allocate(initial(1:n_rows+2, 1:n_cols+2))
    initial = 0.0

    call get_initial_state(initial, n_rows, n_cols)

    close(15)

end subroutine read_config

! ----------------------------------------------------------------------------
subroutine get_initial_state(initial_state, n_rows, n_cols)

    !> @brief Reads the initial state from the config file into a field
    !>
    !> This subroutine reads the state information from the config file
    !> and stores it in the `initial_state` field.

    implicit none
    real(kind=8), dimension(:,:), allocatable, intent(inout) :: initial_state
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
            ! Row/column 1 are the halo region, so add 1
            initial_state(x+1, y+1) = 1.0_8
        else
            print *,"Ignoring line ",line_count,": ",x, y
        endif

        line_count = line_count + 1
    enddo

end subroutine get_initial_state

end module read_config_mod