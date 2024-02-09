program GameOfLife

    use read_config_mod, only  : read_config
    use time_step_alg_mod, only: time_step
    USE grid_mod, only         : grid_type
    USE field_mod, only        : r2d_field
#ifdef PSYDATA_PROFILE
    USE profile_psy_data_mod, only: profile_PSyDataInit, &
                                    profile_PSyDataShutdown
#elif PSYDATA_NAN
    USE nan_test_psy_data_mod, only: nan_test_PSyDataInit, &
                                     nan_test_PSyDataShutdown
#elif PSYDATA_EXTRACT
    USE extract_psy_data_mod, only: extract_PSyDataInit, &
                                    extract_PSyDataShutdown
#endif

    implicit none

    TYPE(grid_type), target    :: grid
    TYPE(r2d_field)            :: initial

    integer                    :: time_steps

    if (iargc() .ne. 1) then
        print *, "Usage: gol config_file"
        stop
    endif

#ifdef PSYDATA_PROFILE
    call profile_PSyDataInit()
#elif PSYDATA_NAN
    call nan_test_PSyDataInit()
#elif PSYDATA_EXTRACT
    call extract_PSyDataInit()
#endif
    ! Read in the initial condition into the field 'current',
    ! and initialise dl_esm_inf.
    call read_config(grid, initial, time_steps) 

    call time_step(grid, initial, time_steps)
#ifdef PSYDATA_PROFILE
    call profile_PSyDataShutdown()
#elif PSYDATA_NAN
    call nan_test_PSyDataShutdown()
#elif PSYDATA_EXTRACT
    call extract_PSyDataShutdown()
#endif
end program GameOfLife
