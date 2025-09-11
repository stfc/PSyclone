










!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Holds and manages key-value pairs in a collection
!>
!> @details A container that holds a collection of key-value pairs. The data
!>          that are put into the collection are copied, so when the original
!>          goes out of scope, the copy in the collection will continue to be
!>          maintained.
!
module key_value_collection_mod

  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

  use constants_mod,        only: i_def, l_def, str_def
  use key_value_mod,        only: key_value_type, &
                                  int32_key_value_type, int64_key_value_type, &
                                  int32_arr_key_value_type, int64_arr_key_value_type, &
                                  real32_key_value_type, real64_key_value_type, &
                                  real32_arr_key_value_type, real64_arr_key_value_type, &
                                  logical_key_value_type, logical_arr_key_value_type, &
                                  str_key_value_type, str_arr_key_value_type, &
                                  abstract_key_value_type, abstract_value_type,&
                                  create_key_value
  use log_mod,              only: log_event, log_scratch_space, &
                                  LOG_LEVEL_ERROR
  use linked_list_data_mod, only: linked_list_data_type
  use linked_list_mod,      only: linked_list_type, &
                                  linked_list_item_type

  implicit none

  private

  ! Set the default table length to 1 - which produces a collection
  ! constructed of a single linked list
  integer(i_def), parameter :: default_table_len = 1

  !-----------------------------------------------------------------------------
  ! Type that holds a collection of key-value pairs in a linked list
  !-----------------------------------------------------------------------------
  type, extends(linked_list_data_type), public :: key_value_collection_type
    private
    !> The name of the collection if provided.
    character(str_def)     :: name = 'unnamed_collection'
    !> A hash table of linked lists of key-value pairs
    type(linked_list_type), allocatable :: key_value_list(:)
    !> The size of the hash table to use.
    ! (Default to a value that represents an uninitialised hash table)
    integer(i_def) :: table_len = 0
    !> Whether object has been initialised or not
    logical :: isinitialised = .false.
  contains
    procedure, public  :: initialise
    procedure, private :: add_key_value_object
    procedure, private :: create_key_value_object
    generic            :: add_key_value => add_key_value_object, &
                                           create_key_value_object
    procedure, public  :: remove_key_value
    procedure, public  :: key_value_exists
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
    procedure, private :: get_abstract_value
    generic            :: get_value => get_int32_value,       &
                                       get_int64_value,       &
                                       get_real32_value,      &
                                       get_real64_value,      &
                                       get_logical_value,     &
                                       get_str_value,         &
                                       get_int32_arr_value,   &
                                       get_int64_arr_value,   &
                                       get_real32_arr_value,  &
                                       get_real64_arr_value,  &
                                       get_logical_arr_value, &
                                       get_str_arr_value,     &
                                       get_abstract_value
    procedure, public  :: get_length
    procedure, public  :: get_name
    procedure, public  :: get_table_len
    procedure, public  :: clear
    procedure, private :: get_hash

    final              :: key_value_collection_destructor
  end type key_value_collection_type

contains

!> Initialises a key-value pair collection
!> @param [in] name The name given to the collection
!> @param [in] table_len The size of the hash table to use
subroutine initialise(self, name, table_len)

  implicit none

  class(key_value_collection_type), intent(inout) :: self
  character(*),           optional, intent(in)    :: name
  integer(i_def),         optional, intent(in)    :: table_len

  integer(i_def) :: i

  if(present(name))self%name = trim(name)

  if (self%isinitialised) then
    write(log_scratch_space, '(3A)') &
    'Key-value collection [', trim(self%name),'] has already been '// &
    'initiaised and should not be initialised for a second time'
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
  end if

  if(present(table_len))then
    self%table_len = table_len
  else
    self%table_len = default_table_len
  end if

  ! Create the hash table of key-value pair lists
  allocate(self%key_value_list(0:self%table_len-1))
  do i = 0, self%table_len-1
    self%key_value_list(i) = linked_list_type()
  end do

  self%isinitialised = .true.

end subroutine initialise

!> Adds a key-value pair to the collection. The pair is maintained in the
!> collection as a copy of the original.
!> @param [in] key_value The key_value pair that is to be copied into the
!>                       collection.
subroutine add_key_value_object(self, key_value)

  implicit none

  class(key_value_collection_type), intent(inout) :: self
  class(key_value_type), intent(in) :: key_value
  character(len=str_def) :: key

  ! Check if key-value pair exists in collection already,
  ! if it does, exit with an error
  key = key_value%get_key()
  if ( self%key_value_exists( trim(key) ) ) then
    write(log_scratch_space, '(4A)') &
          'ERROR: add_key_value: pair with key: ', trim(key_value%get_key()), &
          ' already exists in collection: ', trim(self%name)
        call log_event( log_scratch_space, LOG_LEVEL_ERROR)
  end if

  ! Finished checking - so the key-value must be good to add - so add it
  call self%key_value_list(self%get_hash(key))%insert_item( key_value )

end subroutine add_key_value_object


!> Create a key-value pair object, then adds it to the collection
!> @param [in] key The key of the pair to be added
!> @param [in] value The value of the pair to be added
!>
subroutine create_key_value_object( self, key, value )

  implicit none

  class(key_value_collection_type), intent(inout) :: self
  character(*),                     intent(in)    :: key
  class(*),                         intent(in)    :: value

  class(key_value_type), pointer :: instance

  instance => create_key_value( key, value )
  call self%add_key_value_object( instance )
  deallocate( instance )

end subroutine create_key_value_object


!> Check if a key-value pair is present in the collection
!> @param [in] key The key of the pair to be checked
!> @return exists Flag stating if pair is present or not
function key_value_exists(self, key) result(exists)

  implicit none

  class(key_value_collection_type), intent(in) :: self

  character(*), intent(in) :: key
  logical(l_def)           :: exists

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key, set 'exists' to be false
    if ( .not. associated(loop) ) then
      exists=.false.
      exit
    end if
    ! otherwise search list for the key-value pair we want

    ! 'cast' to the field_type
    select type(listitem => loop%payload)
      class is (key_value_type)
        if ( trim(key) == trim(listitem%get_key()) ) then
          exists=.true.
          exit
        end if
    end select

    loop => loop%next
  end do

end function key_value_exists

!> Remove a key-value pair from the collection
!> @param [in] key The key of the key-value pair to be removed
subroutine remove_key_value(self, key)

  implicit none

  class(key_value_collection_type), intent(inout) :: self
  character(*), intent(in) :: key

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the key-value pair collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key, fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: remove_key_value: No pair with key ', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if

    ! otherwise search list for the name of key we want

    select type(listitem => loop%payload)
      class is (key_value_type)
        if ( trim(key) == trim(listitem%get_key()) ) then
          call self%key_value_list(self%get_hash(key))%remove_item(loop)
          exit
        end if
    end select

    loop => loop%next
  end do

end subroutine remove_key_value

!> Access a 32-bit integer value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 32-bit integer value requested
subroutine get_int32_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),                 intent(in)  :: key
  integer(kind=int32), pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 32-bit integer value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (int32_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_int32_value

!> Access a 64-bit integer value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 64-bit integer value requested
subroutine get_int64_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),                 intent(in)  :: key
  integer(kind=int64), pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 64-bit integer value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (int64_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_int64_value

!> Access a 32-bit real value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 32-bit real value requested
subroutine get_real32_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),               intent(in)  :: key
  real(kind=real32), pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 32-bit real value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (real32_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_real32_value

!> Access a 64-bit real value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 64-bit real value requested
subroutine get_real64_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),               intent(in)  :: key
  real(kind=real64), pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 64-bit real value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (real64_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_real64_value

!> Access a logical value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the logical value requested
subroutine get_logical_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),     intent(in)  :: key
  logical, pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No logical value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (logical_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_logical_value

!> Access a string value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the string value requested
subroutine get_str_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),          intent(in)  :: key
  character(*), pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No string value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (str_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_str_value

!> Access a 32-bit integer array value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 32-bit integer array value requested
subroutine get_int32_arr_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),                 intent(in)  :: key
  integer(kind=int32), pointer, intent(out) :: value(:)

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 32-bit integer array value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (int32_arr_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_int32_arr_value

!> Access a 64-bit integer array value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 64-bit integer array value requested
subroutine get_int64_arr_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),                 intent(in)  :: key
  integer(kind=int64), pointer, intent(out) :: value(:)

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 64-bit integer array value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (int64_arr_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_int64_arr_value

!> Access a 32-bit real array value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 32-bit real array value requested
subroutine get_real32_arr_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),               intent(in)  :: key
  real(kind=real32), pointer, intent(out) :: value(:)

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 32-bit real array value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (real32_arr_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_real32_arr_value

!> Access a 64-bit real array value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the 64-bit real array value requested
subroutine get_real64_arr_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),               intent(in)  :: key
  real(kind=real64), pointer, intent(out) :: value(:)

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No 64-bit real array value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (real64_arr_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_real64_arr_value

!> Access a logical array value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the logical array value requested
subroutine get_logical_arr_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),     intent(in)  :: key
  logical, pointer, intent(out) :: value(:)

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No logical array value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (logical_arr_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_logical_arr_value

!> Access a string array value from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the string array value requested
subroutine get_str_arr_value(self, key, value)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(*),          intent(in)  :: key
  character(*), pointer, intent(out) :: value(:)

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No string array value for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      type is (str_arr_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_str_arr_value

!> Access a value that is an object derived from the abstract from the key-value pair collection
!> @param [in] key The key of the key-value pair being requested
!> @param [out] value Pointer to the abstract object
subroutine get_abstract_value(self, key, value)

  implicit none

  class(key_value_collection_type),    intent(in)  :: self
  character(*),                        intent(in)  :: key
  class(abstract_value_type), pointer, intent(out) :: value

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()


  ! start at the head of the collection linked list
  loop => self%key_value_list(self%get_hash(key))%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! key-value pair, then fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') &
         'ERROR: get_value: No abstract object for key:', &
         trim(key), ' in collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the key we want

    ! 'cast' to the data type
    select type(listitem => loop%payload)
      class is (abstract_key_value_type)
      if ( trim(key) == trim(listitem%get_key()) ) then
          value => listitem%value
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_abstract_value

!> Returns the number of entries in the collection
!> @return  length The number of entries in the hash table
function get_length(self) result(length)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  integer(kind=i_def) :: length
  integer(kind=i_def) :: i

  length = 0
  do i = 0, self%get_table_len()-1
    length = length + self%key_value_list(i)%get_length()
  end do

end function get_length

!> Returns the name of the collection
!> @return name The name of the hash table
function get_name(self) result(name)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  character(str_def) :: name

  name = self%name

end function get_name

!> Returns the length of hash table for this collection
!> @return table_len The length of the hash table being used
function get_table_len(self) result(table_len)

  implicit none

  class(key_value_collection_type), intent(in) :: self
  integer(i_def) :: table_len

  if ( self%table_len == 0 ) then
    call log_event("key_value_collection: Attempt to use uninitialised collection", &
                    LOG_LEVEL_ERROR)
  end if

  table_len = self%table_len

end function get_table_len

!> Returns the hash of the given key
!> @param [in] key The key of the pair to be checked
!> @return hash The hashed value of the given key
function get_hash(self, key) result(hash)
  implicit none
  class(key_value_collection_type), intent(in) :: self
  character(*), intent(in) :: key
  integer(i_def) :: hash
  hash = mod(sum_string(trim(key)),self%get_table_len())
end function get_hash

!> Clears all items from the collection
subroutine clear(self)

  implicit none

  class(key_value_collection_type), intent(inout) :: self
  integer(i_def) :: i

  if(allocated(self%key_value_list))then
    do i = 0, self%get_table_len()-1
      call self%key_value_list(i)%clear()
    end do
    deallocate(self%key_value_list)
  end if

  self%isinitialised = .false.

  return
end subroutine clear

!> Destructor for the collection
subroutine key_value_collection_destructor(self)

  implicit none

  type (key_value_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine key_value_collection_destructor

!> Private function to return the sum of the character values in a string
!> @param [in] string The string to be summed
!> @return The sum of the character values in the string
function sum_string(string) result(ch_sum)
  implicit none
  character(len=*), intent(in)   :: string
  integer :: ch_sum
  integer :: i
  ch_sum = 0
  do i = 1,len(string)
    ch_sum = ch_sum + ichar(string(i:i))
  end do
end function sum_string

end module key_value_collection_mod
