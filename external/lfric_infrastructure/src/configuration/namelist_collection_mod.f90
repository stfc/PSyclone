!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief   Defines \<namelist_collection_type\> object.
!> @details A container object that holds a collection of namelist
!>          objects (namelist_type) as a linked list array.
!>
!>          \<namelist_type\> objects are search via a hasing routine. The
!>          \<namelist_collection_type\> need to be initialised with the
!>          hashing table size before namelists can be added.
!>
module namelist_collection_mod

  use constants_mod,   only: i_def, l_def, str_def, cmdi
  use namelist_mod,         only: namelist_type
  use log_mod,         only: log_event, log_scratch_space, &
                             log_level, LOG_LEVEL_ERROR,   &
                             LOG_LEVEL_DEBUG, LOG_LEVEL_INFO
  use linked_list_mod, only: linked_list_type, &
                             linked_list_item_type

  implicit none

  private

  ! Set the default table length to 1 - which produces a namelist
  ! collection constructed of a single linked list
  integer(i_def), parameter :: default_table_len = 1

  !-----------------------------------------------------------------------------
  ! Type that holds a collection of namelists in a linked list
  !-----------------------------------------------------------------------------
  type, public :: namelist_collection_type

    private

    !> The name of the namelist collection if provided.
    character(str_def) :: name = 'unnamed_collection'

    !> A hash table of linked lists of namelists
    !> contained within the collection
    type(linked_list_type), allocatable :: namelist_list(:)

    character(str_def) :: head_name

    !> The size of the hash table to use.
    ! (Default to a value that represents an uninitialised hash table)
    integer(i_def) :: table_len = 0

    !> Whether object has been initialised or not
    logical :: isinitialised = .false.

  contains

    procedure, public :: initialise
    procedure, public :: add_namelist
    procedure, public :: get_name
    procedure, public :: namelist_exists
    procedure, public :: get_namelist_names
    procedure, public :: get_namelist_profiles
    procedure, public :: get_n_namelists
    procedure, public :: get_table_len

    procedure, public :: get_namelist
    procedure, public :: get_next_item

    procedure, public :: clear

    final :: namelist_collection_destructor

  end type namelist_collection_type

contains


!> @brief Initialises a namelist collection (namelist_collection_type).
!> @param [in] name       Optional: The name given to the collection.
!> @param [in] table_len  Optional: Length of hash table to use for
!>                                  lookup (default=1).
!=====================================================================
subroutine initialise( self, name, table_len )

  implicit none

  class(namelist_collection_type), intent(inout) :: self

  character(*),   optional, intent(in) :: name
  integer(i_def), optional, intent(in) :: table_len

  integer(i_def) :: i

  if ( present(name) ) self%name = trim(name)

  if (self%isinitialised) then
    write(log_scratch_space, '(3A)') &
    'Namelist collection [', trim(self%name),'] has already been '// &
    'initiaised and should not be initialised for a second time'
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
  end if

  if ( present(table_len) ) then
    self%table_len = table_len
  else
    self%table_len = default_table_len
  end if

  ! Create the hash table of namelist lists
  if ( .not. allocated(self%namelist_list) ) then
    allocate( self%namelist_list(0:self%table_len-1) )
  end if

  do i=0, self%table_len-1
    self%namelist_list(i) = linked_list_type()
  end do

  self%isinitialised = .true.

  return
end subroutine initialise


!> @brief Adds a new namelist object to the collection.
!> @param [in] namelist The namelist that is to be added
!>                      into the collection.
!===================================================================
subroutine add_namelist( self, namelist_obj )

  implicit none

  class(namelist_collection_type), intent(inout) :: self
  class(namelist_type),            intent(in)    :: namelist_obj

  integer(i_def) :: hash
  integer(i_def) :: n_lists

  character(len=str_def) :: name
  character(len=str_def) :: profile_name
  character(len=str_def) :: full_name

  ! Check namelist name is valid, if not then exit with error
  name = namelist_obj%get_listname()
  profile_name = namelist_obj%get_profile_name()

  if ( trim(name) == 'none' .or. trim(name) == 'unset' ) then
    write(log_scratch_space, '(A)') &
        'Namelist [' // trim(name) // '] has not been set.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  full_name = namelist_obj%get_listname()

  hash = mod( sum_string( trim(full_name) ), self%get_table_len() )

  call self%namelist_list(hash)%insert_item( namelist_obj )

  n_lists = self%get_n_namelists()

  if (n_lists == 1) self%head_name = full_name

end subroutine add_namelist


!> @brief Check if a namelist is present the collection.
!> @param [in] name         The name of the namelist to be checked.
!> @param [in] profile_name Optional: In the case of namelists which
!>                          are permitted to have multiple instances,
!>                          the profile name distiguishes the instances
!>                          of <name> namelists.
!> @return exists           Flag stating if namelist is present or not
!=====================================================================
function namelist_exists( self, name, profile_name ) result( exists )

  implicit none

  class(namelist_collection_type), intent(in) :: self

  character(*),           intent(in) :: name
  character(*), optional, intent(in) :: profile_name

  logical(l_def) :: exists
  integer(i_def) :: hash

  character(str_def) :: full_name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()
  type(namelist_type),         pointer :: namelist_obj => null()

  ! Calculate the hash of the namelist being searched for
  if ( present(profile_name) ) then
    full_name = trim(name)//':'//trim(profile_name)
  else
    full_name = trim(name)
  end if

  hash = mod( sum_string(trim(name)), self%get_table_len() )

  ! Start at the head of the linked list identified
  ! by the hash number.
  loop => self%namelist_list(hash)%get_head()
  do

    ! If list is empty or we're at the end of list and
    ! we didn't find the namelist, set 'exists' to be false.
    if ( .not. associated(loop) ) then
      exists = .false.
      exit
    end if

    ! Otherwise search list for the name of namelist we want
    ! 'cast' to the namelist_type
    select type( payload => loop%payload )
    type is (namelist_type)
       namelist_obj => payload
       if ( trim(full_name) == trim(namelist_obj%get_full_name()) ) then
         exists = .true.
         exit
       end if
    end select

    loop => loop%next
  end do

end function namelist_exists


!> @brief    Access the next item from the collection that
!>           follows 'start'.
!> @details  If start is null then the routine returns the first
!>           item from the collection.
!> @param[in] start  The point to look for the next item from.
!> @return    item   Pointer to the next item in the collection.
!=====================================================================
function get_next_item( self, start ) result( item )

  implicit none

  class(namelist_collection_type),      intent(in) :: self
  type(linked_list_item_type), pointer, intent(in) :: start

  type(linked_list_item_type), pointer :: item
  type(linked_list_item_type), pointer :: new_item => null()

  character(str_def) :: listname
  integer(i_def)     :: hash
  integer(i_def)     :: i

  if ( associated(start) ) then

    ! Find the item that follows 'start'
    new_item => start%next
    if ( .not. associated(new_item) ) then

      ! Next item is in the following linked list.
      ! Calculate the hash of 'start'
      select type( payload => start%payload)
      type is (namelist_type)
        listname = payload%get_listname()
      end select

      hash = mod( sum_string(trim(listname)), self%get_table_len() )

      ! Find next valid item - or end of collection
      do i=hash+1, self%get_table_len()-1

        new_item => self%namelist_list(i)%get_head()

        ! If a namelist is found, this is the next one,
        ! so exit the loop
        if ( associated(new_item) ) then
          exit
        end if
      end do

    end if

  else

    ! Find the first item in the collection
    do i=0, self%get_table_len()-1

      new_item => self%namelist_list(i)%get_head()
      ! If a namelist is found, this is the first one,
      ! so exit the loop
      if (associated(new_item)) then
        exit
      end if

    end do

  end if

  item => new_item

end function get_next_item


!> @brief Returns a pointer to an namelist instance in the collection.
!> @param [in] name         The name of the namelist definition.
!> @param [in] profile_name Optional: Where a namelist definition is
!>                          allowed to have multiple instances
!>                          (duplicates), a profile name is used to
!>                          identify a particular instance of the
!                           namelist definition.
!> @return namelist_obj     Pointer to the requested namelist object.
!=====================================================================
function get_namelist( self, name, profile_name ) result( namelist_obj )

  implicit none

  class(namelist_collection_type), intent(in) :: self
  character(*),                    intent(in) :: name
  character(*), optional,          intent(in) :: profile_name

  type(namelist_type), pointer :: namelist_obj

  character(str_def) :: full_name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  integer(i_def) :: hash

  nullify(namelist_obj)

  if ( present(profile_name) ) then
    full_name = trim(name) // ':' // trim(profile_name)
  else
    full_name = trim(name)
  end if

  ! Calculate the hash of the namelist being searched for.
  hash = mod( sum_string(trim(name)), self%get_table_len() )

  ! Start at the head of the linked list identified by the hash.
  loop => self%namelist_list(hash)%get_head()

  do
    ! If list is empty or the end of list was reached without
    ! finding the namelist, fail with an error.
    if ( .not. associated(loop) ) then
      write( log_scratch_space, '(A)')           &
          'Namelist '//trim(full_name)//         &
          ' not found in namelist collection '// &
          trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    ! Otherwise search list for the namelist we want
    ! 'cast' to the namelist_type
    select type( payload => loop%payload)
    type is (namelist_type)
      if ( trim(full_name) == trim(payload%get_full_name()) ) then
        namelist_obj => payload
        exit
      end if
    end select

    loop => loop%next
  end do

  nullify(loop)

end function get_namelist


!> @brief Queries the namelist collection for the total number
!>        of namelists stored.
!> @return n_namelists The number of namelists in the namelist collection
!=====================================================================
function get_n_namelists( self ) result( n_namelists )

  implicit none

  class( namelist_collection_type ), intent(in) :: self

  integer( i_def ) :: n_namelists
  integer( i_def ) :: i

  n_namelists = 0
  do i=0, self%get_table_len()-1
    n_namelists = n_namelists + self%namelist_list(i)%get_length()
  end do

end function get_n_namelists


!> @brief Queries the name of the namelist collection.
!> @return name  The name identifying this namelist collection
!>               on initialisation.
!=====================================================================
function get_name( self ) result( name )

  implicit none

  class(namelist_collection_type), intent(in) :: self
  character(str_def) :: name

  name = self%name

end function get_name


!> @brief Queries the unique names of the namelists in the collection.
!> @param[in] show_full     Optional: Logical to return the full name of
!>                          the namelists. This is constructed in the form
!>                          <namelist_name>:<profile_name>, if multiple
!>                          instances of <namelist_name> are permitted.
!>                          If this option is not specified, in the case
!>                          of duplicates, only <namelist_name> is returned.
!> @return  namelist_names  Array of unique names of namelists in the
!>                          collection.
!=====================================================================
function get_namelist_names( self, show_full ) result( namelist_names )

  implicit none

  class(namelist_collection_type), intent(in) :: self
  logical(l_def), optional,        intent(in) :: show_full

  character(str_def), allocatable :: namelist_names(:)
  character(str_def), allocatable :: tmp_names_found(:)

  logical(l_def)     :: get_fullname
  character(str_def) :: name_to_add

  type(namelist_type), pointer :: namelist_obj => null()

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  integer(i_def) :: i
  integer(i_def) :: n_lists
  integer(i_def) :: lists_found

  get_fullname = .false.
  if ( present(show_full) ) then
    get_fullname = show_full
  end if

  n_lists = self%get_n_namelists()

  if (n_lists > 1) then

    lists_found = 0
    allocate(tmp_names_found(n_lists))
    tmp_names_found(:) = cmdi

    do i=0, self%get_table_len()-1

      loop => self%namelist_list(i)%get_head()

      do while ( associated(loop) )

        ! Loop over all items in this list
        if ( associated(loop) ) then

          select type( payload => loop%payload )
          type is ( namelist_type )
            namelist_obj => payload

            if ( get_fullname ) then
              name_to_add = trim(namelist_obj%get_full_name())
            else
              name_to_add = trim(namelist_obj%get_listname())
            end if

            if (.not. any(name_to_add == tmp_names_found(:))) then
              lists_found = lists_found + 1
              tmp_names_found(lists_found) = trim(name_to_add)
            end if
          end select

          loop => loop%next
        else
          loop => loop%next
        end if
      end do

      if (lists_found == n_lists) exit

    end do

  else
    write( log_scratch_space, '(A)')              &
        'Namelist collection '//trim(self%name)// &
        ' is empty.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  if (allocated(namelist_names)) deallocate(namelist_names)
  allocate( namelist_names(lists_found), &
            source=tmp_names_found(1:lists_found) )
  deallocate(tmp_names_found)

  nullify(namelist_obj)

  return
end function get_namelist_names


!> @brief Queries the unique names of the namelists which are permitted
!>        to have multiple instances.
!> @param[in] name       Optional: If present the <namelist_names> returned
!>                       will be for instances of this namelist
!>                       definition.
!> @return namelist_names
!>                       Array of unique names of namelists in the
!>                       collection which are permitted to have
!>                       multiple instances.
!=====================================================================
function get_namelist_profiles( self, name ) result( profile_names )

  implicit none

  class(namelist_collection_type), intent(in) :: self
  character(*), optional,          intent(in) :: name

  character(str_def) :: listname
  character(str_def) :: profile_name

  character(str_def), allocatable :: tmp_names_found(:)
  character(str_def), allocatable :: profile_names(:)

  logical :: add_name
  integer(i_def) :: i
  integer(i_def) :: lists_found
  integer(i_def) :: n_lists

  type(namelist_type), pointer :: namelist_obj => null()

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  n_lists = self%get_n_namelists()

  if (n_lists > 1) then

    lists_found = 0
    allocate(tmp_names_found(n_lists))

    do i=0, self%get_table_len()-1

      loop => self%namelist_list(i)%get_head()

      do while ( associated(loop) )
        if ( associated(loop) ) then
          ! loop over all items in this list
          select type( payload => loop%payload )
          type is ( namelist_type )
            namelist_obj => payload
            listname     = trim(namelist_obj%get_listname())
            profile_name = trim(namelist_obj%get_profile_name())
            if (present(name)) then
              add_name = ( ( .not. profile_name == cmdi ) .and. &
                           ( trim(listname) == trim(name) ) )
            else
              add_name = ( .not. profile_name == cmdi )
            end if

            if (add_name) then
              lists_found = lists_found + 1
              if ( present(name) ) then
                tmp_names_found(lists_found) = trim(namelist_obj%get_profile_name())
              else
                tmp_names_found(lists_found) = trim(namelist_obj%get_full_name())
              end if
            end if

          end select
          loop => loop%next
        else
          loop => loop%next
        end if
      end do

      if (lists_found == n_lists) exit

    end do

  else
    write( log_scratch_space, '(A)')              &
        'Namelist collection '//trim(self%name)// &
        ' is empty.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  if (allocated(profile_names)) deallocate(profile_names)
  allocate( profile_names(lists_found), &
            source=tmp_names_found(1:lists_found) )
  deallocate(tmp_names_found)

  nullify( namelist_obj )

  return

end function get_namelist_profiles


!> @brief Queries the size of the hash table used by the collection.
!> @return table_length  The length of hash table used by
!>                       namelist collection
!=====================================================================
function get_table_len( self ) result( table_len )

  implicit none

  class(namelist_collection_type), intent(in) :: self
  integer(i_def) :: table_len

  if ( self%table_len == 0 ) then
    call log_event("namelist_collection: Attempt to use uninitialised collection", &
                    LOG_LEVEL_ERROR)
  end if

  table_len = self%table_len

end function get_table_len


!> @brief Clears all items from the namelist collection.
!=====================================================================
subroutine clear(self)

  implicit none

  class(namelist_collection_type), intent(inout) :: self
  integer(i_def) :: i

  if (allocated(self%namelist_list)) then
    do i=0, self%get_table_len()-1
      call self%namelist_list(i)%clear()
    end do
    deallocate(self%namelist_list)
  end if

  self%isinitialised = .false.

  return
end subroutine clear


!> @brief Destructor for the namelist collection
!=====================================================================
subroutine namelist_collection_destructor(self)

  implicit none

  type (namelist_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine namelist_collection_destructor


!> @brief        Private function to return a integer hash.
!> @description  The hash is constructed as the sum of the character
!>               values in a string.
!> @param[in] string  The string to be summed.
!> @return    ch_sum  The sum of the character values in the string.
!=====================================================================
function sum_string( string ) result( ch_sum )

  implicit none

  character(len=*), intent(in)   :: string
  integer(i_def) :: ch_sum
  integer(i_def) :: i

  ch_sum = 0
  do i=1, len(string)
    ch_sum = ch_sum + ichar(string(i:i))
  end do

  return
end function sum_string

end module namelist_collection_mod
