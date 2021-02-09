
module profile_psy_data_mod

    use psy_data_base_mod, only : PSyDataBaseType, profile_PSyDataStart, &
                                  profile_PSyDataStop, is_enabled

  type, extends(PSyDataBaseType) :: profile_PSyDataType
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart, PostEnd
  end type profile_PSyDataType

  logical :: has_been_initialised = .false.

contains
  ! ---------------------------------------------------------------------------
  !> An optional initialisation subroutine. It is not called directly from
  !> any PSyclone created code, but for most existing profiling libraries the
  !> application will have to call it. In this dummy library it is called once
  !> from ProfileStart.
  subroutine profile_PSyDataInit()
    implicit none
    if (is_enabled) then
        print *,"profile_PSyDataInit called"
    else
        print *,"profile_PSyDataInit called, but profiling is disabled"
    endif
    has_been_initialised = .true.
  end subroutine profile_PSyDataInit

  ! ---------------------------------------------------------------------------
  !> Starts a profiling area. The module and region name can be used to create
  !! a unique name for each region.
  !! Parameters: 
  !! this:        PSyData instance.
  !! module_name: Name of the module in which the region is
  !! region_name: Name of the region (could be name of an invoke, or
  !!              subroutine name).
  !! @param[in] num_pre_vars The number of variables that are declared and
  !!            written before the instrumented region.
  !! @param[in] num_post_vars The number of variables that are also declared
  !!            before an instrumented region of code, but are written after
  !!            this region.
  subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                      num_post_vars)
    implicit none
    class(profile_PSyDataType), intent(inout), target :: this
    character(len=*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if ( .not. has_been_initialised ) then
       call profile_PSyDataInit()
    endif

    call this%PSyDataBaseType%PreStart(module_name, region_name, 0, 0)
    if (is_enabled) then
        print *, "PreStart called for module '", module_name,  &
                 "' region '", region_name, "'"
    else
        print *, "PreStart called for module '", module_name,  &
                 "' region '", region_name, "', but profiling is disabled"
    endif
  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a ProfileData type that corresponds to
  !> to the ProfileStart call.
  !> this: PSyData instance.
  ! 
  subroutine PostEnd(this)
    implicit none
    class(profile_PSyDataType), intent(inout), target :: this
    
    if (is_enabled) then
        print *,"PostEnd called for module '", trim(this%module_name), &
                "' region '", trim(this%region_name), "'"
    else
        print *,"PostEnd called for module '", trim(this%module_name), &
                "' region '", trim(this%region_name), "', but profiling is disabled"
    endif
  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> The finalise function prints the results. This subroutine must be called
  !> for most profiling libraries, otherwise no results will be produced.
  !> This call must be added to the program explicitly, it is not called
  !> automatically.
  subroutine profile_PSyDataShutdown()
    implicit none
    print *,"profile_PSyDataShutdown called"
  end subroutine profile_PSyDataShutdown

end module profile_psy_data_mod
