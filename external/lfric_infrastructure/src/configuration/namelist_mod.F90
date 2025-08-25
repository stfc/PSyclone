!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief   Defines a generic namelist object to contain namelist
!>          configuration data.
!> @details Namelists object to hold an array of namelist member data.
!>          Namelist data represented by key/value pairs are each held
!>          as a namelist_item_type. The namelist_item_type is a container
!>          classs for an abstract class which allows for the differing
!>          data types that a namelist might contain.
!>
!----------------------------------------------------------------------------
module namelist_mod

  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

  use linked_list_data_mod, only: linked_list_data_type
  use log_mod,              only: log_level, log_event, log_scratch_space, &
                                  LOG_LEVEL_ERROR, LOG_LEVEL_DEBUG,        &
                                  LOG_LEVEL_INFO
  use constants_mod,        only: imdi, rmdi, cmdi, str_def, i_def
  use namelist_item_mod,    only: namelist_item_type

  implicit none

  private

  public :: namelist_type

  !=========================================
  ! Namelist object (namelist_type)
  !=========================================
  type, extends(linked_list_data_type) :: namelist_type

    private

    character(:), allocatable :: listname
    character(str_def) :: profile_name = trim(cmdi)

#ifdef __NVCOMPILER
    class(namelist_item_type), allocatable :: members(:)
#else
    type(namelist_item_type), allocatable :: members(:)
#endif

  contains

    procedure, private :: get_int32_value
    procedure, private :: get_int64_value
    procedure, private :: get_real32_value
    procedure, private :: get_real64_value
    procedure, private :: get_logical_value
    procedure, private :: get_str_value

    procedure, private :: get_int32_arr_value
    procedure, private :: get_int64_arr_value
    procedure, private :: get_real32_arr_value
    procedure, private :: get_real64_arr_value
    procedure, private :: get_logical_arr_value
    procedure, private :: get_str_arr_value

    procedure, private :: locate_member

    !=======================================================
    procedure :: initialise => namelist_initialise
    procedure :: get_listname
    procedure :: get_profile_name
    procedure :: get_full_name

    generic   :: get_value => get_int32_value,   get_int32_arr_value,   &
                              get_int64_value,   get_int64_arr_value,   &
                              get_real32_value,  get_real32_arr_value,  &
                              get_real64_value,  get_real64_arr_value,  &
                              get_logical_value, get_logical_arr_value, &
                              get_str_value,     get_str_arr_value

    procedure :: clear

    ! Destructor
    final :: namelist_destructor

  end type namelist_type

contains

  !=========================================
  ! Module routines/functions
  !=========================================

  !=========================================
  ! 1.0 Constructors
  !=========================================

  !------------------------------------------
  ! 1.4 GENERIC namelist constructor
  !============================================================================
  !> @brief Contructs a <namelist_type> object
  !> @param [in] name     Name of the namelist.
  !> @return     members  Namelist members in form of an array
  !>                      of <namelist_item_type>.
  subroutine namelist_initialise( self, listname, members, profile_name )

    implicit none

    class(namelist_type),     intent(inout) :: self
    character(*),             intent(in)    :: listname
    type(namelist_item_type), intent(in)    :: members(:)
    character(*), optional,   intent(in)    :: profile_name

    if ( present(profile_name) ) then
      self%profile_name = profile_name
    end if

    if (size(members) <= 0) then
      write(log_scratch_space, '(A)')          &
          'Attempted to create '// listname // &
          ' namelist with no member variables.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    allocate( self%listname, source=listname )
#ifndef __NVCOMPILER
    allocate( self%members,  source=members )
#else
    self%members = members
#endif

  end subroutine namelist_initialise


  !=========================================
  ! 2.0 GENERIC namelist type methods
  !=========================================

  !-----------------------------------------------
  ! 2.1a Get namelist definition name
  !============================================================================
  !> @brief  Queries the name of the namelist.
  !> @return listname  The name of the namelist
  function get_listname( self ) result( listname )

    implicit none
    class(namelist_type), intent(in) :: self
    character(:), allocatable        :: listname

    allocate( listname, source=self%listname )

  end function get_listname

  ! 2.1b Get profile name of the namelist instance
  function get_profile_name( self ) result( profile_name )

    implicit none

    class(namelist_type), intent(in) :: self
    character(:), allocatable        :: profile_name

    allocate( profile_name, source=self%profile_name )

  end function get_profile_name

  ! 2.1c Get profile name of the namelist instance
  function get_full_name( self ) result( full_name )

    implicit none

    class(namelist_type), intent(in) :: self
    character(:), allocatable        :: full_name

    if ( trim(self%profile_name) == trim(cmdi) ) then
      allocate( full_name, source=trim(self%listname) )
    else
      allocate( full_name, &
                source=trim(self%listname)//':'//trim(self%profile_name) )
    end if

  end function get_full_name


  !-----------------------------------------------
  ! 2.2 Getters for NON-ARRAY variables
  !-----------------------------------------------

  ! 2.2a Namelist member getter: 32-BIT INTEGER
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 32-bit integer.
  !> @param [in]  name  Namelist member to return data for.
  !> @param [out] value Namelist mamber 32-bit integer data.
  subroutine get_int32_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    integer(int32),       intent(out) :: value

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_int32_value


  ! 2.2b Namelist member getter: 64-BIT INTEGER
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 64-bit integer.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_int64_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    integer(int64),       intent(out) :: value

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_int64_value


  ! 2.2c Namelist member getter: 32-BIT REAL
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 32-bit real.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_real32_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),    intent(in)  :: name
    real(real32),    intent(out) :: value

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_real32_value


  ! 2.2d Namelist member getter: 64-BIT REAL
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 64-bit real.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_real64_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    real(real64),         intent(out) :: value

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_real64_value


  ! 2.2e Namelist member getter: logical
  !-------------------------------------------------------
  !> @brief Private subroutine to return a logical.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_logical_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    logical,              intent(out) :: value

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_logical_value


  ! 2.2f Namelist member getter: String
  !-------------------------------------------------------
  !> @brief Private subroutine to return a string.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_str_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    character(*),         intent(out) :: value

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value(value)

  end subroutine get_str_value


  !-----------------------------------------------
  ! 2.3 Getters for ARRAY variables
  !-----------------------------------------------

  ! 2.3a Namelist member getter: 32-BIT INTEGER ARRAY
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 32-bit integer array.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_int32_arr_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    integer(int32),       intent(out), &
                          allocatable :: value(:)

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_int32_arr_value


  ! 2.3b Namelist member getter: 64-BIT INTEGER ARRAY
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 64-bit integer array.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_int64_arr_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    integer(int64),       intent(out), &
                          allocatable :: value(:)

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_int64_arr_value


  ! 2.3c Namelist member getter: 32-BIT REAL ARRAY
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 32-bit real array.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_real32_arr_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    real(real32),         intent(out), &
                          allocatable :: value(:)

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_real32_arr_value


  ! 2.3d Namelist member getter: 64-BIT REAL ARRAY
  !-------------------------------------------------------
  !> @brief Private subroutine to return a 64-bit array.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_real64_arr_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    real(real64),         intent(out), &
                          allocatable :: value(:)

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_real64_arr_value


  ! 2.3e Namelist member getter: logical array
  !-------------------------------------------------------
  !> @brief Private subroutine to return a logical array.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_logical_arr_value( self, name, value )

    implicit none

    class(namelist_type), intent(in)  :: self
    character(*),         intent(in)  :: name
    logical,              intent(out), &
                          allocatable :: value(:)

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value( value )

  end subroutine get_logical_arr_value


  ! 2.3f Namelist member getter: STRING ARRAY
  !-------------------------------------------------------
  !> @brief Private subroutine to return a string array.
  !> @param [in]  name  Namelist member name to return data for.
  !> @param [out] value Namelist member data.
  subroutine get_str_arr_value( self, name, value )

    implicit none

    class(namelist_type), intent(in) :: self
    character(*),         intent(in) :: name

    !> @todo This was applied with #3547. This would have been
    !>       similar to the scalar string: i.e.
    !>
    !>       character(*), allocatable, intent(out) :: value(:)
    !>
    !>       However, the revision of the Intel compiler on the XC40
    !>       produced unexpected behaviour so the length has been
    !>       limited to str_def. This should be revisited when the
    !>       XC40 compilers are later than 17.0.0.098/5.
    character(str_def), intent(out), &
                        allocatable :: value(:)

    integer(i_def) :: i

    i = self%locate_member( name )
    call self%members(i)%get_value(value)

    return
  end subroutine get_str_arr_value


  !-------------------------------------------------------
  !> @brief Private function to locate a namelist
  !>        member within the the <namelist_type>
  !> @param[in]  name          Name of namelist member to
  !>                           check for.
  !> @return     member_index  Location in member array of
  !>                           <namelist_item_type> object.
  function locate_member( self, name ) result( member_index )

    implicit none

    class(namelist_type), intent(in) :: self
    character(*),         intent(in) :: name

    integer(i_def) :: i
    integer(i_def) :: member_index

    character(:), allocatable :: key

    member_index = -1

    if ( allocated(self%members) ) then
      do i=1, size(self%members)
        key = self%members(i)%get_key()
        if ( trim(key) == trim(name) ) then
          member_index = i
          return
        end if
      end do

      write( log_scratch_space, '(A)' )             &
         'Member "' // trim(name) // '" is not ' // &
         'present in ' // self%listname // ' namelist.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )

    else
      write( log_scratch_space, '(A)' ) &
         self%listname // ' namelist is empty.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

  end function locate_member


  !-------------------------------------------------------
  !> @brief Deallocates memory for the <namelist_type> object.
  subroutine clear(self)

    implicit none

    class(namelist_type), intent(inout) :: self

    if ( allocated(self%members) )  deallocate( self%members )
    if ( allocated(self%listname) ) deallocate( self%listname )

  end subroutine clear


  !-------------------------------------------------------
  !> @brief <namelist_type> destructor.
  subroutine namelist_destructor(self)

    implicit none

    type (namelist_type), intent(inout) :: self

    call self%clear()

    return
  end subroutine namelist_destructor

end module namelist_mod
