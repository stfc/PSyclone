program GameOfLife

    use read_config_mod, only: read_config
    use time_step_mod, only  : time_step

    implicit none

    real(kind=8), dimension(:,:), allocatable :: initial

    integer                  :: time_steps

    if (iargc() .ne. 1) then
        print *, "Usage: gol config_file"
        stop
    endif

    ! Read in the initial condition into the field 'current',
    ! and initialise dl_esm_inf.
    call read_config(initial, time_steps)

    call time_step(initial, time_steps)
end program GameOfLife