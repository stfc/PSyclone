
module profile_mod

  ! The datatype to store information about a region
  type :: ProfileData
     character(:), allocatable :: module_name
     character(:), allocatable :: region_name
     integer                   :: count
     real*4                    :: start, sum, min, max
     logical                   :: initialised = .false.
  end type ProfileData

  ! --------------------------------------------------------
  ! In order to store an array of pointers, Fortran requires
  ! a new type *sigh*
  type ProfileDataPointer
     type(ProfileData), pointer :: p
  end  type ProfileDataPointer
  ! --------------------------------------------------------

  integer, parameter :: MAX_DATA = 100

  ! This keeps track of all user data areas
  type(ProfileDataPointer), dimension(MAX_DATA) :: all_data

  ! How many entries in all_data have been used
  integer :: used_entries

  logical :: has_been_initialised = .false.

contains

  ! ---------------------------------------------------------------------------
  ! An optional initialisation subroutine. It is not called directly from
  ! any PSycloen created code, but for most existing profiling libraries a
  ! requirement. In this dummy library it is called once from ProfileStart.
  !
  subroutine ProfileInit()
    implicit none

    used_entries         = 0
    has_been_initialised = .true.

  end subroutine ProfileInit

  ! ---------------------------------------------------------------------------
  ! Starts a profiling area. The module and region name can be used to create
  ! a unique name for each region.
  ! Parameters: 
  ! module_name:  Name of the module in which the region is
  ! region_name:  Name of the region (could be name of an invoke, or
  !               subroutine name).
  ! profile_data: Persistent data used by the profiling library.
  subroutine ProfileStart(module_name, region_name, profile_data)
    implicit none

    character*(*)      :: module_name, region_name
    type(ProfileData), target :: profile_data
    integer            :: count, count_rate

    if ( .not. has_been_initialised ) then
       call ProfileInit()
    endif

    ! Note that the actual initialisation of profile_data
    ! happens in ProfileEnd, which is when min, sum and
    ! max are properly initialised
    profile_data%module_name = module_name
    profile_data%region_name = region_name
    call system_clock(count, count_rate)
    profile_data%start  = real(count) / count_rate

  end subroutine ProfileStart

  ! ---------------------------------------------------------------------------
  ! Ends a profiling area. It takes a ProfileData type that corresponds to
  ! to the ProfileStart call.
  ! profile_data: Persistent data used by the profiling library.
  ! 
  subroutine ProfileEnd(profile_data)
    implicit none

    type(ProfileData), target :: profile_data

    integer :: count, count_rate
    real *4 :: now, duration
    
    call system_clock(count, count_rate)
    now = real(count) / count_rate
    duration = now - profile_data%start

    ! Now initialise the data
    if (.not. profile_data%initialised) then
       profile_data%sum         = duration
       profile_data%min         = profile_data%sum
       profile_data%max         = profile_data%sum
       profile_data%count       = 1
       profile_data%initialised = .true.

       if (used_entries < MAX_DATA) then
          ! Save a pointer to the profiling data
          used_entries = used_entries + 1
          all_data(used_entries)%p => profile_data
       endif
    else
       profile_data%sum = profile_data%sum + duration
       if (duration < profile_data%min ) profile_data%min = duration
       if (duration > profile_data%max ) profile_data%max = duration
       profile_data%count = profile_data%count + 1
    endif

  end subroutine ProfileEnd

  ! ---------------------------------------------------------------------------
  ! The finalise functoin prints the results
  subroutine ProfileFinalise()
    implicit none
    integer                     :: i
    integer                     :: max_len, this_len
    type(ProfileData), pointer :: p
    character                   :: tab = char(9)
    character(:), allocatable   :: heading
    character(:), allocatable   :: spaces

    heading = "module::region"
    ! Determine maximum header length to get proper alignment
    max_len = len(heading)
    do i=1, used_entries
       p => all_data(i)%p    ! to abbreviate code a bit
       if (len(p%module_name) + len(p%region_name) > max_len) then
          max_len = len(p%module_name) + len(p%region_name)
       endif
    enddo

    ! Allow for "::" and one additional space:
    max_len = max_len + 3

    allocate(character(len=max_len) :: spaces)
    do i=1, max_len
       spaces(i:i) = " "
    enddo

    print *,
    print *,"==========================================="
    print *, heading, spaces(1:max_len - len(heading)),                       &
             tab, "count", tab, tab, "sum", tab, tab, tab, "min", tab, tab,   &
             "average", tab, tab, tab, "max"
    do i=1, used_entries
       p => all_data(i)%p    ! to abbreviate code a bit
       this_len = len(p%module_name) + len(p%region_name)+3
       print *, p%module_name,"::",p%region_name, spaces(1:max_len-this_len), &
                p%count, tab, p%sum, tab,                                     &
                p%min, tab, p%sum/p%count, tab, p%max
    end do
    print *,"==========================================="
  end subroutine ProfileFinalise

end module profile_mod
