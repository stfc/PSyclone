program GameOfLife

    use read_config_mod, only: read_config
    use time_step_mod, only  : time_step
!$ use omp_lib, only: omp_get_max_threads

    implicit none

    real(kind=8), dimension(:,:), allocatable :: initial

    integer                  :: time_steps

    if (command_argument_count() .ne. 1) then
        print *, "Usage: gol config_file"
        stop
    endif

!$ print *,"Using", omp_get_max_threads(), "threads"

    ! Read in the initial condition into the field 'current',
    ! and initialise dl_esm_inf.
    call read_config(initial, time_steps)

    call time_step(initial, time_steps)
end program GameOfLife