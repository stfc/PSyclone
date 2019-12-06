
module psy_data_mod
  type :: PSyDataType
      character(:), allocatable :: module_name
      character(:), allocatable :: region_name
  contains
      ! The profiling API uses only the two following calls:
      procedure :: PreStart, PostEnd
  end type PSyDataType

  logical :: has_been_initialised = .false.

contains
  ! ---------------------------------------------------------------------------
  !> An optional initialisation subroutine. It is not called directly from
  !> any PSyclone created code, but for most existing profiling libraries the
  !> application will have to call it. In this dummy library it is called once
  !> from ProfileStart.
  subroutine ProfileInit()
    implicit none
    print *,"ProfileInit called"
    has_been_initialised = .true.
  end subroutine ProfileInit

  ! ---------------------------------------------------------------------------
  !> Starts a profiling area. The module and region name can be used to create
  !> a unique name for each region.
  !> Parameters: 
  !> module_name:  Name of the module in which the region is
  !> region_name:  Name of the region (could be name of an invoke, or
  !>               subroutine name).
  !> profile_data: Persistent data used by the profiling library.
  subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                          num_post_vars)
    implicit none
    class(PSyDataType), intent(inout) :: this
    character*(*), intent(in) :: module_name, region_name
    integer, intent(in) :: num_pre_vars, num_post_vars

    if ( .not. has_been_initialised ) then
       call ProfileInit()
    endif
    print *, "PreStart called for module '", module_name,  &
         "' region '", region_name, "'"
    this%module_name = module_name
    this%region_name = region_name
  end subroutine PreStart

  ! ---------------------------------------------------------------------------
  !> Ends a profiling area. It takes a ProfileData type that corresponds to
  !> to the ProfileStart call.
  !> profile_data: Persistent data used by the profiling library.
  ! 
  subroutine PostEnd(this)
    implicit none
    class(PSyDataType), intent(inout) :: this
    
    print *,"PostEnd called for module '", this%module_name, &
         "' region '", this%region_name, "'"
  end subroutine PostEnd

  ! ---------------------------------------------------------------------------
  !> The finalise function prints the results. This subroutine must be called
  !> for most profiling libraries, otherwise no results will be produced.
  !> This call must be added to the program explicitly, it is not called
  !> automatically.
  subroutine ProfileFinalise()
    implicit none
    print *,"ProfileFinalise called"
    has_been_initialised = .true.
  end subroutine ProfileFinalise

end module psy_data_mod
