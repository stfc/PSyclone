










!-------------------------------------------------------------------------------
! (c) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief Holds and manages I/O contexts in a collection
!>
!> @details A container that holds a collection of I/O contexts. Contexts that
!>          are presented to the io_context_collection through the add_context()
!>          method are copied, so when the original goes out of scope, the copy
!>          in the io_context_collection will continue to be maintained.
module io_context_collection_mod
  use constants_mod,   only : str_def, i_def
  use linked_list_mod, only : linked_list_type, linked_list_item_type
  use linked_list_data_mod, only : linked_list_data_type
  use io_context_mod,  only : io_context_type
  use hash_mod,        only : hash_string
  use log_mod,         only : log_event, log_scratch_space, LOG_LEVEL_ERROR

  ! Types which can be stored in collection
  use empty_io_context_mod, only : empty_io_context_type

  implicit none
  private

  integer(i_def), parameter :: default_table_len = 1

  type, public :: io_context_collection_type
    private

    character(str_def) :: name = 'unamed_io_collection'
    type(linked_list_type), allocatable :: context_list(:)
    integer(i_def) :: table_len = 0

  contains
    procedure, public :: initialise
    procedure, public :: add_context
    procedure, public :: remove_context
    procedure, public :: get_empty_io_context
    generic           :: get_io_context => get_empty_io_context
    procedure, public :: context_exists
    procedure, public :: get_table_len
    procedure, public :: clear
    final :: destructor

  end type io_context_collection_type

contains

  !> @brief Initialises an io_context collection
  !> @param [in] name The name given to the collection
  !> @param [in] table_len The size of the hash table to use [default = 1]
  subroutine initialise(this, name, table_len)

    implicit none

    class(io_context_collection_type), intent(inout) :: this
    character(*), optional, intent(in) :: name
    integer(i_def), optional, intent(in) :: table_len

    integer(i_def) :: i

    if(present(name)) this%name = trim(name)

    if(present(table_len)) then
      this%table_len = table_len
    else
      this%table_len = default_table_len
    end if
    allocate(this%context_list(0:this%table_len - 1))
    do i = 0, this%table_len - 1
      this%context_list(i) = linked_list_type()
    end do

  end subroutine initialise

  !> @brief Adds a context to the collection. The context held in the collection
  !>        will be a copy of the original.
  !> @param context The context to be added to the collection
  subroutine add_context(this, context)
    implicit none
    class(io_context_collection_type), intent(inout) :: this
    class(io_context_type), intent(in) :: context

    integer(i_def)  :: hash
    character(str_def) :: name

    name = get_context_name(context)

    if (this%context_exists(trim(name))) then
      write(log_scratch_space, '(4A)') &
              'Context name [', trim(name), &
              '] already exists in collection: ', trim(this%name)
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    hash = mod(hash_string(trim(name)), this%get_table_len())
    call this%context_list(hash)%insert_item(context)

  end subroutine add_context

  !> Remove a io_context from the collection
  !> @param [in] context_name The name of the io_context to be removed
  subroutine remove_context(this, context_name)
    implicit none
    class(io_context_collection_type), intent(inout) :: this
    character(*), intent(in) :: context_name

    type(linked_list_item_type), pointer :: loop => null()
    integer(i_def) :: hash
    character(str_def) :: name

    hash = mod(hash_string(trim(context_name)), this%get_table_len())

    loop => this%context_list(hash)%get_head()

    do
      if (.not. associated(loop)) then
        write(log_scratch_space, '(4A)') 'remove_context: No IO context [', &
          trim(context_name), '] in context collection: ', trim(this%name)
        call log_event( log_scratch_space, LOG_LEVEL_ERROR)
      end if
      name = get_context_name(loop%payload)
      if( trim(context_name) == trim(name)) then
        call this%context_list(hash)%remove_item(loop)
        exit
      end if

      loop => loop%next
    end do

  end subroutine remove_context


  subroutine get_empty_io_context(this, context_name, context)
    implicit none
    class(io_context_collection_type), intent(in) :: this
    type(empty_io_context_type), pointer, intent(out) :: context
    character(*), intent(in) :: context_name

    character(str_def) :: name

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    integer(i_def) :: hash

    ! Calculate hash of context to be searched for
    hash = mod(hash_string(trim(context_name)), this%get_table_len())

    ! Start at the head of the collection linked list
    loop => this%context_list(hash)%get_head()

    do
      ! If the list is empty or we reach the end of the list and didn't find the
      ! context, fail with an error
      if( .not. associated(loop) ) then
        write(log_scratch_space, '(4A)') 'Get field: No empty_io_context [', &
          trim(context_name), '] in context collection: ', trim(this%name)
        call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      end if

      ! Otherwise search list for the name of the context we want

      name = get_context_name(loop%payload)

      select type(listcontext => loop%payload)
        type is (empty_io_context_type)
        if ( trim(context_name) == trim(name)) then
          context => listcontext
          exit
        end if
      end select
      loop => loop%next
    end do

  end subroutine get_empty_io_context

  !> @brief Check if a context is present in the collection
  !> @param context_name The name of the context to be checked for
  !> @return exists Flag stating if the context exists
  function context_exists(this, context_name) result(exists)
    implicit none
    class(io_context_collection_type), intent(in) :: this
    character(*), intent(in) :: context_name
    logical :: exists
    integer(i_def) :: hash
    character(len=str_def) :: name

    type(linked_list_item_type), pointer :: loop => null()

    hash = mod(hash_string(trim(context_name)), this%get_table_len())

    loop => this%context_list(hash)%get_head()
    do
      if (.not. associated(loop)) then
        exists = .false.
        exit
      end if
      name = get_context_name(loop%payload)
      if( trim(context_name) == trim(name)) then
        exists = .true.
        exit
      end if

      loop => loop%next
    end do

  end function context_exists

  !> @brief A private routine to return the name of a context of any type
  !> @param context The context to return the name of
  !> @return name The name of the context
  function get_context_name(context) result(name)
    implicit none
    class(linked_list_data_type), intent(in) :: context
    character(len=str_def) :: name

    select type(incontext => context)
      class is (io_context_type)
        name = incontext%get_context_name()
    end select
  end function get_context_name

  !> @brief Queries the size of the hash table used by the collection.
  !> @return table_length  The length of hash table used by
  !>                       io_context collection
  !=====================================================================
  function get_table_len( this ) result( table_len )
    implicit none

    class(io_context_collection_type), intent(in) :: this
    integer(i_def) :: table_len

    if ( this%table_len == 0 ) then
      call log_event("io_context_collection: Attempt to use uninitialised collection", &
                      LOG_LEVEL_ERROR)
    end if
    table_len = this%table_len

  end function get_table_len

  !> Clears all items from the context collection
  subroutine clear(this)
    implicit none
    class(io_context_collection_type) , intent(inout) :: this
    integer(i_def) :: i

    if(allocated(this%context_list))then
      do i=0,this%get_table_len()-1
        call this%context_list(i)%clear
      end do
      deallocate(this%context_list)
    end if
  end subroutine clear

  !> @brief Destructor for io context collection
  subroutine destructor(this)
    implicit none

    type(io_context_collection_type), intent(inout) :: this

    call this%clear()

  end subroutine destructor

end module io_context_collection_mod
