!-----------------------------------------------------------------------------
! (C) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Holds and manages fields in a collection
!>
!> @details A container that holds a collection of fields. Fields that are
!>          presented to the field_collection through the add_field() method are
!>          copied, so when the original goes out of scope, the copy in the
!>          field_collection will continue to be maintained.
!
module field_collection_mod

  use constants_mod,           only: i_def, l_def, str_def
  use field_array_mod,         only: field_array_type
  use field_mod,               only: field_type, &
                                     field_pointer_type
  use field_parent_mod,        only: field_parent_type
  use field_real32_mod,        only: field_real32_type, &
                                     field_real32_pointer_type
  use field_real64_mod,        only: field_real64_type, &
                                     field_real64_pointer_type
  use integer_field_mod,       only: integer_field_type, &
                                     integer_field_pointer_type
  use pure_abstract_field_mod, only: pure_abstract_field_type
  use log_mod,                 only: log_event, log_scratch_space, &
                                     LOG_LEVEL_ERROR
  use linked_list_data_mod,    only: linked_list_data_type
  use linked_list_mod,         only: linked_list_type, &
                                     linked_list_item_type

  implicit none

  private

  ! Set the default table length to 1 - which produces a field collection
  ! constructed of a single linked list
  integer(i_def), parameter :: default_table_len = 1

  !-----------------------------------------------------------------------------
  ! Type that holds a collection of fields in a linked list
  !-----------------------------------------------------------------------------
  type, extends(linked_list_data_type), public :: field_collection_type
    private
    !> The name of the field collection if provided.
    character(str_def)     :: name = 'unnamed_collection'
    !> A hash table of linked lists of fields contained within the collection
    type(linked_list_type), allocatable :: field_list(:)
    !> The size of the hash table to use.
    ! (Default to a value that represents an uninitialised hash table)
    integer(i_def) :: table_len = 0
    !> Whether object has been initialised or not
    logical :: isinitialised = .false.
  contains
    procedure, public :: initialise
    procedure, public :: copy_collection
    procedure, public :: add_field
    procedure, public :: add_reference_to_field
    procedure, public :: remove_field
    procedure, public :: field_exists
    procedure, public :: get_next_item
    procedure, public :: get_real32_field
    procedure, public :: get_real64_field
    procedure, public :: get_integer_field
    procedure, public :: get_field_array
    generic           :: get_field => get_real32_field,  &
                                      get_real64_field,  &
                                      get_integer_field, &
                                      get_field_array
    procedure, public :: get_length
    procedure, public :: get_name
    procedure, public :: get_table_len
    procedure, public :: clear

    procedure, private :: collection_copy_constructor

    generic, public   :: assignment(=) => collection_copy_constructor
    final             :: field_collection_destructor
  end type field_collection_type

contains

!> Initialises a field collection
!> @param [in] name The name given to the collection
subroutine initialise(self, name, table_len)

  implicit none

  class(field_collection_type), intent(inout) :: self
  character(*),       optional, intent(in)    :: name
  integer(i_def),     optional, intent(in)    :: table_len

  integer(i_def) :: i

  if(present(name))self%name = trim(name)

  if (self%isinitialised) then
    write(log_scratch_space, '(3A)') &
    'Field collection [', trim(self%name),'] has already been '// &
    'initiaised and should not be initialised for a second time'
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
  end if

  if(present(table_len))then
    self%table_len = table_len
  else
    self%table_len = default_table_len
  end if

  ! Create the hash table of field lists
  allocate(self%field_list(0:self%table_len-1))
  do i = 0, self%table_len-1
    self%field_list(i) = linked_list_type()
  end do

  self%isinitialised = .true.

end subroutine initialise

!> Adds a field to the collection. The field maintained in the collection will
!> either be a copy of the original or a field pointer object containing a
!> pointer to a field held elsewhere..
!> @param [in] field The field that is to be copied into the collection or a
!>                   field pointer object that is to be stored in the collection
subroutine add_field(self, field)

  implicit none

  class(field_collection_type), intent(inout) :: self
  class(pure_abstract_field_type), intent(in) :: field
  integer(i_def) :: hash
  character(len=str_def) :: name

  name = get_field_name(field)

  ! Check field name is valid, if not then exit with error
  if ( trim(name) == 'none' .OR. trim(name) == 'unset') then
    write(log_scratch_space, '(3A)') &
    'Field name [', trim(name), &
    '] is an invalid field name, please choose a unique field name.'
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
  end if

  ! Check if field exists in collection already, if it does, exit with error
  if ( self%field_exists( trim(name) ) ) then
    write(log_scratch_space, '(4A)') &
      'Field [', trim(name), &
      '] already exists in field collection: ', trim(self%name)
    call log_event( log_scratch_space, LOG_LEVEL_ERROR)
  end if

  ! Finished checking - so the field must be good to add - so add it
  hash = mod(sum_string(trim(name)),self%get_table_len())
  call self%field_list(hash)%insert_item( field )

end subroutine add_field

!> Check if a field is present the collection
!> @param [in] field_name The name of the field to be checked
!> @return exists Flag stating if field is present or not
function field_exists(self, field_name) result(exists)

  implicit none

  class(field_collection_type), intent(in) :: self

  character(*), intent(in) :: field_name
  logical(l_def)           :: exists
  integer(i_def)           :: hash
  character(len=str_def)   :: name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  ! Calculate the hash of the field being searched for
  hash = mod(sum_string(trim(field_name)),self%get_table_len())

  ! start at the head of the mesh collection linked list
  loop => self%field_list(hash)%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! field, set 'exists' to be false
    if ( .not. associated(loop) ) then
      exists=.false.
      exit
    end if
    ! otherwise search list for the name of field we want
    name = get_field_name(loop%payload)
    if ( trim(field_name) == trim(name) ) then
      exists=.true.
      exit
    end if

    loop => loop%next
  end do

end function field_exists

!> Adds a pointer to a field into the field collection. The pointer will point
!> to a field held elsewhere. If that field is destroyed, the pointer will
!> become an orphan.
!> @param [in] field_ptr : A pointer to the field (real32, real64 or integer)
!>                         that is to be referenced in the collection.
! The routine accepts a pointer to a field (real32, real64 or integer). It
! packages it up into a field pointer object and calls the routine to add this
! to the collection
subroutine add_reference_to_field(self, field_ptr)

  implicit none

  class(field_collection_type), intent(inout)          :: self
  class(pure_abstract_field_type), pointer, intent(in) :: field_ptr

  type(field_real32_type), pointer   :: fld_real32_ptr
  type(field_real32_pointer_type)    :: field_real32_pointer
  type(field_real64_type), pointer   :: fld_real64_ptr
  type(field_real64_pointer_type)    :: field_real64_pointer
  type(integer_field_type), pointer  :: int_fld_ptr
  type(integer_field_pointer_type)   :: integer_field_pointer

  select type(field_ptr)
    type is (field_real32_type)
      ! Create a field pointer object that just contains an real32 field pointer
      fld_real32_ptr => field_ptr
      call field_real32_pointer%initialise(fld_real32_ptr)
      call self%add_field( field_real32_pointer )
    type is (field_real64_type)
      ! Create a field pointer object that just contains an real64 field pointer
      fld_real64_ptr => field_ptr
      call field_real64_pointer%initialise(fld_real64_ptr)
      call self%add_field( field_real64_pointer )
    type is (integer_field_type)
      ! Create an integer field pointer object that just contains a field ptr
      int_fld_ptr => field_ptr
      call integer_field_pointer%initialise(int_fld_ptr)
      call self%add_field( integer_field_pointer )
    class default
      call log_event( &
        'Failed to add an object of unsupported type to a field collection.', &
        LOG_LEVEL_ERROR)
  end select

end subroutine add_reference_to_field

!> Remove a field from the collection
!> @param [in] field_name The name of the field to be removed
subroutine remove_field(self, field_name)

  implicit none

  class(field_collection_type), intent(inout) :: self
  character(*), intent(in) :: field_name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()
  integer(i_def) :: hash

  character(len=str_def)   :: name

  ! Calculate the hash of the field being removed
  hash = mod(sum_string(trim(field_name)),self%get_table_len())

  ! start at the head of the field collection linked list
  loop => self%field_list(hash)%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! field, fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') 'remove_field: No field [', &
         trim(field_name), '] in field collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the name of field we want

    name = get_field_name(loop%payload)
    if ( trim(field_name) == trim(name) ) then
      call self%field_list(hash)%remove_item(loop)
      exit
    end if

    loop => loop%next
  end do

end subroutine remove_field

!> Access the next item from the collection that follows 'start'. If start
!> is null then the routine returns the first item from the collection.
!> @param [in] start The point to look for the next item from.
!> @return item Pointer to the next item in the collection.
function get_next_item(self, start) result(item)

  implicit none

  class(field_collection_type),         intent(in) :: self
  type(linked_list_item_type), pointer, intent(in) :: start

  type(linked_list_item_type), pointer :: item
  type(linked_list_item_type), pointer :: new_item => null()
  character(str_def) :: name
  integer(i_def) :: i, hash

  if(associated(start))then
    ! Find the item that follows 'start'
    new_item => start%next
    if (.not.associated(new_item) ) then
      ! Next item is in the following linked list. Calculate the hash of 'start'
      name = get_field_name(start%payload)
      hash = mod(sum_string(trim(name)),self%get_table_len())
      ! Find next valid item - or end of collection
      do i = hash+1, self%get_table_len()-1
        new_item => self%field_list(i)%get_head()
        ! If a field is found, this is the next one - so exit the loop
        if (associated(new_item)) then
          exit
        end if
      end do
    end if
  else
    ! Find the first item in the collection
    do i = 0, self%get_table_len()-1
      new_item => self%field_list(i)%get_head()
      ! If a field is found, this is the first one - so exit the loop
      if (associated(new_item)) then
        exit
      end if
    end do
  end if
  item => new_item

end function get_next_item

!> Access a 32-bit real field from the collection
!> @param [in] field_name The name of the 32-bit real field to be accessed
!> @param [out] field Pointer to the 32-bit real field that is extracted
subroutine get_real32_field(self, field_name, field)

  implicit none

  class(field_collection_type), intent(in) :: self
  type(field_real32_type), pointer, intent(out) :: field

  character(*), intent(in) :: field_name

  character(len=str_def) :: name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  integer(i_def) :: hash

  ! Calculate the hash of the field being searched for
  hash = mod(sum_string(trim(field_name)),self%get_table_len())

  ! start at the head of the mesh collection linked list
  loop => self%field_list(hash)%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! field, fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') 'get_field: No 32-bit field [', &
         trim(field_name), '] in field collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the name of field we want

    name = get_field_name(loop%payload)
    ! 'cast' to the field_type
    select type(listfield => loop%payload)
      type is (field_real32_type)
      if ( trim(field_name) == trim(name) ) then
          field => listfield
          exit
      end if
      type is (field_real32_pointer_type)
      if ( trim(field_name) == trim(name) ) then
          field => listfield%field_ptr
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_real32_field

!> Access a 64-bit real field from the collection
!> @param [in] field_name The name of the 64-bit real field to be accessed
!> @param [out] field Pointer to the 64-bit real field that is extracted
subroutine get_real64_field(self, field_name, field)

  implicit none

  class(field_collection_type), intent(in) :: self
  type(field_real64_type), pointer, intent(out) :: field

  character(*), intent(in) :: field_name

  character(len=str_def) :: name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  integer(i_def) :: hash

  ! Calculate the hash of the field being searched for
  hash = mod(sum_string(trim(field_name)),self%get_table_len())

  ! start at the head of the mesh collection linked list
  loop => self%field_list(hash)%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! field, fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') 'get_field: No 64-bit field [', &
         trim(field_name), '] in field collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the name of field we want

    name = get_field_name(loop%payload)
    ! 'cast' to the field_type
    select type(listfield => loop%payload)
      type is (field_real64_type)
      if ( trim(field_name) == trim(name) ) then
          field => listfield
          exit
      end if
      type is (field_real64_pointer_type)
      if ( trim(field_name) == trim(name) ) then
          field => listfield%field_ptr
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_real64_field

!> Access an integer field from the collection
!> @param [in] field_name The name of the intager field to be accessed
!> @param [out] field Pointer to the integer field that is extracted
subroutine get_integer_field(self, field_name, field)

  implicit none

  class(field_collection_type), intent(in) :: self
  type(integer_field_type), pointer, intent(out) :: field

  character(*), intent(in) :: field_name

  character(len=str_def) :: name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  integer(i_def) :: hash

  ! Calculate the hash of the field being searched for
  hash = mod(sum_string(trim(field_name)),self%get_table_len())

  ! start at the head of the field collection linked list
  loop => self%field_list(hash)%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! field, fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') 'No integer field [', trim(field_name), &
         '] in field collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the name of field we want

    name = get_field_name(loop%payload)
    ! 'cast' to the integer_field_type
    select type(listfield => loop%payload)
      type is (integer_field_type)
      if ( trim(field_name) == trim(name) ) then
          field => listfield
          exit
      end if
      type is (integer_field_pointer_type)
      if ( trim(field_name) == trim(name) ) then
          field => listfield%field_ptr
          exit
      end if
    end select

    loop => loop%next
  end do

end subroutine get_integer_field

!> Access a field array from the collection
!> @param [in] field_array_name The name of the field array to be accessed
!> @param [out] field_array Pointer to the field array that is returned
subroutine get_field_array(self, field_array_name, field_array)

  implicit none

  class(field_collection_type),    intent(in)  :: self
  type(field_array_type), pointer, intent(out) :: field_array
  character(*),                    intent(in)  :: field_array_name

  character(len=str_def) :: name

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type), pointer :: loop => null()

  integer(i_def) :: hash

  ! Calculate the hash of the field being searched for
  hash = mod(sum_string(trim(field_array_name)),self%get_table_len())

  ! start at the head of the mesh collection linked list
  loop => self%field_list(hash)%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! field, fail with an error
    if ( .not. associated(loop) ) then
      write(log_scratch_space, '(4A)') 'get_field: No field array [', &
         trim(field_array_name), '] in field collection: ', trim(self%name)
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
    end if
    ! otherwise search list for the name of field we want

    name = get_field_name(loop%payload)
    ! 'cast' to the field_array_type
    select type(listfield => loop%payload)
      type is (field_array_type)
        if ( trim(field_array_name) == trim(name) ) then
          field_array => listfield
          exit
        end if
    end select

    loop => loop%next
  end do

end subroutine get_field_array

!> Returns the number of entries in the field collection
function get_length(self) result(length)

  implicit none

  class(field_collection_type), intent(in) :: self
  integer(kind=i_def) :: length
  integer(kind=i_def) :: i

  length = 0
  if ( .not. allocated(self%field_list) ) then
    call log_event("field_collection: get_length called for uninitialised collection", &
                    LOG_LEVEL_ERROR)
  end if

  do i = 0, self%get_table_len()-1
    length = length + self%field_list(i)%get_length()
  end do

end function get_length

!> Returns the name of the field collection
function get_name(self) result(name)

  implicit none

  class(field_collection_type), intent(in) :: self
  character(str_def) :: name

  name = self%name

end function get_name

!> Returns the length of hash table for this field collection
function get_table_len(self) result(table_len)

  implicit none

  class(field_collection_type), intent(in) :: self
  integer(i_def) :: table_len

  if ( self%table_len == 0 ) then
    call log_event("field_collection: Attempt to use uninitialised collection", &
                    LOG_LEVEL_ERROR)
  end if

  table_len = self%table_len

end function get_table_len

!> DEPRECATED: Assignment operator between field_collection_type pairs.
!> Currently, this routine generates a (hopefully) useful message, then
!> forces an error.
!>
!> @param[out] self   field_type lhs
!> @param[in]  source field_type rhs
subroutine collection_copy_constructor(self, source)

  implicit none
  class(field_collection_type), intent(inout) :: self
  type(field_collection_type), intent(in) :: source

  write(log_scratch_space,'(A,A)')&
     '"field_collection2=field_collection1" syntax no longer supported. '// &
     'Use "call field_collection1%copy_collection(field_collection2)". '// &
     'Field collection: ', source%get_name()
  call log_event( log_scratch_space, LOG_LEVEL_ERROR )

end subroutine collection_copy_constructor

!> Returns a copy of the field collection.
!! Fields held by the collection are copied (metadata & data).
!! However, field references will only be pointer copies, and
!! so will share [meta]data with the original field reference.
subroutine copy_collection(self, dest, name)

  implicit none
  class(field_collection_type), intent(in)  :: self
  type(field_collection_type),  intent(out) :: dest
  character(*),      optional,  intent(in)  :: name

  class(linked_list_item_type), pointer :: field_item => null()
  integer(i_def) :: i

  ! First clear any existing fields in the "copy-to" collection.
  if(allocated(dest%field_list))call dest%clear()

  ! Create a new field collection for the destination
  if (present(name)) then
    call dest%initialise(name=name, table_len=self%get_table_len())
  else
    call dest%initialise(name=self%get_name(), table_len=self%get_table_len())
  end if

  ! Populate the new collection with all the items from the source
  if (self%get_length() > 0) then
    do i=-0, self%get_table_len()-1
      field_item => self%field_list(i)%get_head()
      do while (associated(field_item))
        select type(item => field_item%payload)
          class is (pure_abstract_field_type)
            call dest%add_field(item)
        end select
        field_item => field_item%next
      end do
    end do
  end if

end subroutine copy_collection

!> Clears all items from the field collection
subroutine clear(self)

  implicit none

  class(field_collection_type), intent(inout) :: self
  integer(i_def) :: i

  if(allocated(self%field_list))then
    do i = 0, self%get_table_len()-1
      call self%field_list(i)%clear()
    end do
    deallocate(self%field_list)
  end if

  self%isinitialised = .false.

  return
end subroutine clear

!> Destructor for the field collection
subroutine field_collection_destructor(self)

  implicit none

  type (field_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine field_collection_destructor

!> Private helper function extracts the name from any type of field
!  (so hides all the SELECT TYPE nonsense in here)
!> @param [in] field Polymorphic field type
!> @return The name of the given field
 function get_field_name(field) result(name)
  implicit none
  class(linked_list_data_type), intent(in) :: field
  character(len=str_def) :: name
  select type(infield => field)
    ! Call get_name (from the parent) on any non-pointer field
    ! (i.e. any field that inherits from field_parent_type)
    class is (field_parent_type)
      name = infield%get_name()
    ! If the field is actually a pointer, dereference to the specific actual
    ! field type and call get_name in that
    type is (field_real32_pointer_type)
      name = infield%field_ptr%get_name()
    type is (field_real64_pointer_type)
      name = infield%field_ptr%get_name()
    type is (integer_field_pointer_type)
      name = infield%field_ptr%get_name()
    type is (field_array_type)
      name = infield%get_name()
  end select
end function get_field_name

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

end module field_collection_mod
