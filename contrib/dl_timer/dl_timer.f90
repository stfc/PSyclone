
module profiler_mod
  type :: ProfilerData
     character(:), allocatable :: module_name
     character(:), allocatable :: region_name
     integer                   :: timer_index
     logical                   :: registered = .false.
  end type ProfilerData


contains
  ! ---------------------------------------------------------------------------
  ! An optional initialisation subroutine. It is not called directly from
  ! any PSycloen created code, but for most existing profilint libraries a
  ! requirement. In this dummy library it is called once from ProfileStart.
  !
  subroutine ProfileInit()
    use dl_timer, only :timer_init

    implicit none
    call timer_init()

  end subroutine ProfileInit

  ! ---------------------------------------------------------------------------
  ! Starts a profiling area. The module and region name can be used to create
  ! a unique name for each region.
  ! Parameters: 
  ! module_name:  Name of the module in which the region is
  ! region_name:  Name of the region (could be name of an invoke, or
  !               subroutine name).
  ! profile_data: Persistent data used by the profiling library.
  subroutine ProfileStart(module_name, region_name, profiler_data)
    use dl_timer, only : timer_register, timer_start
    implicit none

    character*(*) :: module_name, region_name
    type(ProfilerData) :: profiler_data

    if( .not. profiler_data%registered) then
       call timer_register(profiler_data%timer_index, &
                           label=module_name//":"//region_name)
       profiler_data%registered = .true.
    endif
    call timer_start(profiler_data%timer_index)
  end subroutine ProfileStart

  ! ---------------------------------------------------------------------------
  ! Ends a profiling area. It takes a ProfilerData type that corresponds to
  ! to the ProfileStart call.
  ! profile_data: Persistent data used by the profiling library.
  ! 
  subroutine ProfileEnd(profiler_data)
    use dl_timer, only : timer_stop
    implicit none

    type(ProfilerData) :: profiler_data
    
    call timer_stop(profiler_data%timer_index)
  end subroutine ProfileEnd

  ! ---------------------------------------------------------------------------
  subroutine ProfileFinalise()
    use dl_timer, only : timer_report
    implicit none
    call timer_report()

  end subroutine ProfileFinalise

end module profiler_mod
