!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief Linked list container

!> @details A generic linked list container object with procedures for management
!> of the linked list

module linked_list_mod

  use linked_list_data_mod, only    : linked_list_data_type
  use constants_mod,        only    : i_def

  implicit none

  private

  ! constants to control insertion point into linked list
  integer(i_def), parameter, public :: before = -1
  integer(i_def), parameter, public :: after = 1

  type, public                           :: linked_list_type
    private
    integer(i_def) :: length ! The number of items in the list

    ! The first entry in the linked list
    type(linked_list_item_type), pointer :: head => null()
    ! The last entry in the linked list
    type(linked_list_item_type), pointer :: tail => null()
    ! The entry currently pointed to
    type(linked_list_item_type), pointer :: current => null()

  contains
    procedure, public :: insert_item
    procedure, public :: remove_item
    procedure, public :: item_exists
    procedure, public :: get_length
    procedure, public :: get_item
    procedure, public :: get_current
    procedure, public :: set_current
    procedure, public :: get_head
    procedure, public :: get_tail
    procedure, public :: clear

    !> Object finaliser
    final             :: linked_list_destructor

  end type linked_list_type

  type, public :: linked_list_item_type
    class(linked_list_data_type), pointer :: payload => null()
    type(linked_list_item_type),  pointer :: prev    => null()
    type(linked_list_item_type),  pointer :: next    => null()
  end type linked_list_item_type

  interface linked_list_type
     module procedure linked_list_constructor
  end interface linked_list_type



contains

!> Linked list constructor
!> Ensures that the list length and pointers to head, tail, and
!> current items are initialised
type(linked_list_type) function linked_list_constructor()

  implicit none

  linked_list_constructor%length = 0

end function linked_list_constructor

!> Gets the current length of the list
!> @return list length
function get_length(self) result(length)

  implicit none

  class(linked_list_type), intent (in)  :: self
  integer(i_def)                   :: length

  length = self%length

end function get_length

!> Gets the item currently pointed at
!> @return current item
function get_current(self) result(curr_item)

  implicit none

  class(linked_list_type), intent (in)  :: self
  class(linked_list_item_type),pointer  :: curr_item

  curr_item => self%current

end function get_current

!> Gets the item with payload of provided id
function get_item(this, id) result(item)
  implicit none
  class(linked_list_type), intent(in)   :: this
  integer(i_def), intent(in)            :: id

  type(linked_list_item_type),pointer  :: item

  ! temp ptr to loop through list
  type(linked_list_item_type),pointer  :: loop

  ! start from beginning of list
  loop => this%head
  item => null()

  do
    ! if it isn't pointing at anything or we finished just exit
    if ( .not. associated(loop) ) exit

    if (loop%payload%get_id() == id) then
      item => loop
      exit
    end if
    loop=>loop%next
  end do

  return

end function get_item

!> Set the item currently pointed to.
subroutine set_current(self, new_item)

  implicit none

  class(linked_list_type), intent (inout)  :: self
  class(linked_list_item_type),pointer  :: new_item

  self%current => new_item

end subroutine set_current

!> Gets the item at the head of the list
!> @return head item
function get_head(self) result(head_item)

  implicit none

  class(linked_list_type), intent (in)  :: self
  class(linked_list_item_type),pointer  :: head_item

  head_item => self%head

end function get_head

!> Gets the item at the tail of the list
!> @return tail item
function get_tail(self) result(tail_item)

  implicit none

  class(linked_list_type), intent (in)  :: self
  class(linked_list_item_type),pointer  :: tail_item

  tail_item => self%tail

end function get_tail

!> Checks if an item (of the id supplied) exists in the list
!> @param in id of item
!> @param in start (optional) start of extent to check
!> @param in finish (optional) finish of extent to check
!> @return true if the item exists or false if it doesn't
function item_exists(this, id, start, finish) result(exists)

  implicit none

  class(linked_list_type), intent(in)   :: this
  integer(i_def), intent(in)            :: id
  logical                               :: exists


  ! optional start and end points if not considering entire list
  type(linked_list_item_type),optional,pointer,intent(in)  :: start
  type(linked_list_item_type),optional,pointer,intent(in)  :: finish


  ! temp ptr to loop through list
  type(linked_list_item_type),pointer      :: loop => null()

  ! assume item does not exist at first

  exists = .false.


  if(present(start)) then
    ! start from specified element
    loop => start
  else
    ! start from beginning of list
    loop => this%head
  end if

  ! Check the main list between the specified limits
  do
     ! if it isn't pointing at anything or we finished just exit
     ! Compilers may interpret the order of evaluation of conditionals
     ! This structure avoids ambiguity
     if(.not.associated(loop)) exit
     if(present(finish)) then
        if(associated(loop,finish)) exit
     end if
     if (loop%payload%get_id() == id) then
        exists = .true.
        exit
     else
        exists = .false.
     end if
     loop=>loop%next
  end do

return

end function item_exists

!> Inserts an item in the list
!> @param in the new item to insert
!> @param in insert_point (optional) the point to insert at
!> @param in placement (optional) insert before or after insert point
subroutine insert_item(self, new_data, insert_point, placement)

  implicit none

  class(linked_list_type), intent (inout)   :: self
  class(linked_list_data_type), intent (in) :: new_data

  ! Optional argument to decide where to insert
  ! if missing, assumes insert point is 'current'

  type(linked_list_item_type), optional, pointer, intent(inout) :: insert_point

  ! Optional argument to insert before or after insert point
  integer(i_def), optional, intent (in)                      :: placement

  type(linked_list_item_type),pointer       :: new_item ! New list item

  ! Allocate new item
  allocate(new_item)
  allocate(new_item%payload, source=new_data)

  if (.not. associated(self%current)) then
    ! Nothing in the linked list so add the first item
    self%length = 1
    ! nullify prev pointer
    new_item%prev => null()
    ! nullify next pointer
    new_item%next => null()
    ! head, tail and current all point at new item
    self%head => new_item
    self%tail => new_item
    self%current => new_item

  else

     ! There is at least one item in the list so insert
     ! according to insert_point and placement arguments
     ! if present
     ! Compilers may interpret the order of evaluation of conditionals differently
     ! This structure avoids ambiguity
     if (present(placement) ) then
        if (placement == before ) then

           ! is insert point present?
           ! is it the start of the list?
           if ( present(insert_point) ) then
              if ( associated(insert_point,self%head)) then
                 ! need to insert before the list head
                 ! Nullify prev of new item
                 new_item%prev => null()
                 ! point prev of head to new item
                 self%head%prev => new_item
                 ! point next of new item to head
                 new_item%next => self%head
                 ! make new item the head
                 self%head => new_item
                 ! and current
                 self%current => new_item
                 self%length = self%length + 1

              else
                 ! insert_point is not start of list so go ahead
                 ! and insert before it
                 new_item%prev => insert_point%prev
                 new_item%next => insert_point
                 if(associated(insert_point%prev)) insert_point%prev%next => new_item
                 insert_point%prev => new_item
                 insert_point => new_item
                 ! point current at new element
                 self%current => new_item
                 self%length = self%length + 1

              end if

           else ! just insert before whatever is current
              ! insert before current
              ! point prev of new item at prev of current item
              new_item%prev => self%current%prev
              ! point new next at current
              new_item%next => self%current
              ! point prev next at new
              if(associated(self%current%prev)) self%current%prev%next => new_item
              ! point current prev at new
              self%current%prev => new_item
              ! point current at new element
              self%current => new_item
              self%length = self%length + 1
           end if
        end if
    else ! no placement specified, inserting after
      ! is insert point present?
      if ( present(insert_point) ) then
        ! insert after insert point
        new_item%prev => insert_point
        new_item%next => insert_point%next
        if(associated(insert_point%next)) insert_point%next%prev => new_item
        insert_point%next => new_item
        self%current => new_item
        ! if we are inserting after tail then
        ! point at new tail
        if ( associated(insert_point,self%tail)) then
          self%tail => self%current
        end if
        self%length = self%length + 1

      else
        ! just insert after current
        ! prev pointer of the new element needs to point at current
        new_item%prev => self%current
        ! point next pointer of new element at next pointer of current
        new_item%next => self%current%next
        ! point next prev at new
        if(associated(self%current%next)) self%current%next%prev => new_item
        ! point current next at new
        self%current%next => new_item
        ! if we are inserting after tail then
        ! point at new tail
        if ( associated(self%current,self%tail)) then
          self%tail => new_item
        end if

        ! point current at new element
        self%current => new_item
        self%length = self%length + 1
      end if
    end if
  end if

end subroutine insert_item

!> Removes an item from the list
!> @param inout item (optional) The item to be removed.
!>                              Otherwise current is removed
subroutine remove_item(self, item )

  implicit none

  class(linked_list_type), intent (inout)   :: self

  ! Optional argument to decide which item to remove
  ! if missing, remove 'current'
  type(linked_list_item_type), optional, pointer, intent(inout) :: item

  type(linked_list_item_type), pointer :: to_remove

  ! Select the item to be removed
  if ( present(item) ) then
    to_remove => item
  else
    to_remove => self%current
  end if

  ! If the item to be removed doesn't exist, don't try to remove it
  if ( associated(to_remove) ) then

    ! Update the next/prev pointers in the surrounding items in the list
    ! so they bypass the items to be removed
    if ( associated(to_remove%prev) ) to_remove%prev%next => to_remove%next
    if ( associated(to_remove%next) ) to_remove%next%prev => to_remove%prev

    self%length = self%length - 1
    ! If head or tail is being removed set a new head/tail
    if ( associated(to_remove,self%head) ) self%head => to_remove%next
    if ( associated(to_remove,self%tail) ) self%tail => to_remove%prev

    ! If current is being removed set a new current to the next item in the list
    ! If that is null (i.e. at the end of the list) use the previous item
    if ( associated(self%current, to_remove) ) self%current => to_remove%next
    if ( .not.associated(self%current) ) self%current => to_remove%prev

    ! Remove the item
    deallocate(to_remove%payload)
    deallocate(to_remove)

  end if

end subroutine remove_item

!> Clears the list
subroutine clear(self)

  implicit none

  class(linked_list_type), intent(inout):: self

  type(linked_list_item_type), pointer :: tmp ! Temporary ptr used to clear a list
                                              ! item whilst still allowing access
                                              ! to the next item in the list

  do
    ! if it isn't pointing at anything just exit
    if ( .not. associated(self%head) )exit
    ! point tmp at head of list
    tmp=>self%head
    ! point head at next
    self%head=>self%head%next
    ! deallocate contents and item
    deallocate(tmp%payload)
    deallocate(tmp)
  end do

  nullify(self%current)
  nullify(self%tail)

  ! reset number of elements in container
  self%length = 0

end subroutine clear

!-----------------------------------------------------------------------------
! Linked list finalizer
!-----------------------------------------------------------------------------

subroutine linked_list_destructor(self)

  implicit none

  type (linked_list_type), intent(inout) :: self

  call self%clear()

  nullify(self%head)
  nullify(self%tail)
  nullify(self%current)

  return
end subroutine linked_list_destructor

end module linked_list_mod
