!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds and manages function spaces created during a model run.
!>
!> @details A container which holds type definition of a collection of
!>          function spaces. The collection holds function spaces as
!>          singletons. It will handle the creation and storing of
!>          requested function spaces.
!
module function_space_collection_mod

  use constants_mod,      only : i_def, str_short, l_def
  use function_space_mod, only : function_space_type
  use fs_continuity_mod,  only : name_from_functionspace
  use log_mod,            only : log_event, log_scratch_space,    &
                                 LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use mesh_mod,           only : mesh_type
  use linked_list_mod,    only : linked_list_type,                &
                                 linked_list_item_type
  use function_space_constructor_helper_functions_mod, &
                          only : generate_fs_id
  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Type which is is a collection of function spaces held in a linked list
  !-----------------------------------------------------------------------------
  type, public :: function_space_collection_type
    private
    type(linked_list_type) :: fs_list

  contains

    procedure, public :: get_fs
    procedure, public :: get_fs_collection_size
    procedure, public :: clear
    final             :: function_space_collection_destructor
  end type function_space_collection_type
  !-----------------------------------------------------------------------------

  interface function_space_collection_type
    module procedure function_space_collection_constructor
  end interface

  ! Module level variable to make the function space collection
  ! globally available
  type(function_space_collection_type), public, allocatable :: &
                                                function_space_collection

contains
  !-----------------------------------------------------------------------------
  ! Construct the function space collection
  !-----------------------------------------------------------------------------
  !> Function to construct a function space collection

  function function_space_collection_constructor() result(self)

    implicit none

    type(function_space_collection_type) :: self

    self%fs_list = linked_list_type()

  end function function_space_collection_constructor


  !-----------------------------------------------------------------------------
  ! Get or create a function space
  !-----------------------------------------------------------------------------
  !> Function to get an instance of a function space from the linked list
  !> or create it if it doesn't exist
  !> @param[in] mesh            mesh object
  !> @param[in] element_order_h function space order in the horizontal direction
  !> @param[in] element_order_v function space order in the vertical direction
  !> @param[in] lfric_fs        lfric id code for given supported function space
  !> @param[in] ndata           The number of data values to be held at each dof
  !!                            location
  !> @param[in] ndata_first     Flag to set data to be layer first (false) or
  !!                            ndata first (true)
  function get_fs( self,            &
                   mesh,            &
                   element_order_h, &
                   element_order_v, &
                   lfric_fs,        &
                   ndata,           &
                   ndata_first )  result(fs)

    implicit none

    class(function_space_collection_type), intent(inout) :: self

    type(mesh_type), intent(in), pointer  :: mesh
    integer(i_def),  intent(in)           :: element_order_h
    integer(i_def),  intent(in)           :: element_order_v
    integer(i_def),  intent(in)           :: lfric_fs
    integer(i_def),  intent(in), optional :: ndata
    logical(l_def),  intent(in), optional :: ndata_first

    type(function_space_type), pointer :: fs

    integer(i_def)       :: ndata_sz
    integer(i_def)       :: mesh_id
    character(str_short) :: name
    logical(l_def)       :: ndata_first_sz

    nullify(fs)

    if (present(ndata)) then
      ndata_sz = ndata
    else
      ndata_sz = 1
    end if

    if ( present(ndata_first) ) then
      ndata_first_sz = ndata_first
    else
      ndata_first_sz = .false.
    end if

    ! Check if the provided fs enumerator has an associated name
    ! (this will produce an error if a match is not found)
    name = name_from_functionspace(lfric_fs)

    if (element_order_h < 0 .or. element_order_v < 0) then
      write(log_scratch_space, '(A,I0)')              &
      'Function space element order must be >= 0   ', &
      element_order_h, element_order_v
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    mesh_id = mesh%get_id()

    fs => get_existing_fs( self,            &
                           mesh_id,         &
                           element_order_h, &
                           element_order_v, &
                           lfric_fs,        &
                           ndata_sz,        &
                           ndata_first_sz )

    if (.not. associated(fs)) then

      call self%fs_list%insert_item(                 &
               function_space_type( mesh_id,         &
                                    element_order_h, &
                                    element_order_v, &
                                    lfric_fs,        &
                                    ndata_sz,        &
                                    ndata_first_sz) )

      write(log_scratch_space, '(A,I0,A,I0,A)')                          &
      'Generated horizontal order-', element_order_h,' vertical order-', &
      element_order_v, ' ' // trim(name) // '-function space singleton'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)

      fs => get_existing_fs( self,            &
                             mesh_id,         &
                             element_order_h, &
                             element_order_v, &
                             lfric_fs,        &
                             ndata_sz,        &
                             ndata_first_sz )

    end if

  end function get_fs


  !----------------------------------------------------------------------------
  ! Get the size of the function space collection
  ! (only really used in unit tests)
  !-----------------------------------------------------------------------------
  !> Function to return the number of function spaces currently
  !> held in the collection

  function get_fs_collection_size(self) result(fs_list_length)

    implicit none

    class(function_space_collection_type), intent(in) :: self

    integer(i_def) :: fs_list_length

    fs_list_length = self%fs_list%get_length()

  end function get_fs_collection_size


  !-----------------------------------------------------------------------------
  ! Clear the function space collection
  !-----------------------------------------------------------------------------
  !> Function to clear all items from the function space collection
  !> linked list
  subroutine clear(self)

    implicit none

    class(function_space_collection_type), intent(inout) :: self

    call self%fs_list%clear()

  end subroutine clear

  !-----------------------------------------------------------------------------
  ! Function space collection destructor
  !-----------------------------------------------------------------------------

  subroutine function_space_collection_destructor(self)

    implicit none

    type (function_space_collection_type), intent(inout) :: self

    call self%clear()

  end subroutine function_space_collection_destructor


  !------------------------------------------------------------------------------
  ! Private function (not accessible through the API - and only called from
  ! within this module) to scan the function space collection for function space
  ! with the given properties and return a pointer to it. A null pointer is
  ! returned if the requested function space does not exist.
  !
  !> @param[in] mesh_id         ID of mesh object
  !> @param[in] element_order_h function space order in horizontal
  !> @param[in] element_order_v function space order in vertical
  !> @param[in] lfric_fs        lfric id code for given supported function space
  !> @param[in] ndata           The number of data values to be held at each dof location
  !!                            return <pointer> Pointer to function space object or null()
  !> @param[in] ndata_first     Flag to set data to be layer first (false) or
  !!                            ndata first (true)
  function get_existing_fs( self,            &
                            mesh_id,         &
                            element_order_h, &
                            element_order_v, &
                            lfric_fs,        &
                            ndata,           &
                            ndata_first ) result(instance)

    implicit none

    class(function_space_collection_type) :: self
    integer(i_def), intent(in) :: mesh_id
    integer(i_def), intent(in) :: element_order_h
    integer(i_def), intent(in) :: element_order_v
    integer(i_def), intent(in) :: lfric_fs
    integer(i_def), intent(in) :: ndata
    logical(l_def), intent(in) :: ndata_first

    type(function_space_type), pointer :: instance

    type(linked_list_item_type), pointer :: loop

    integer(i_def) :: fs_id

    ! Point to head of the function space linked list
    loop => self%fs_list%get_head()

    ! Loop through the linked list
    do
      if ( .not. associated(loop) ) then
        ! Have reach the end of the list so either
        ! the list is empty or at the end of list.
        instance => null()

        loop => self%fs_list%get_tail()
        exit
      end if

      ! Need to 'cast' the payload as the specific
      ! linked list data type, i.e. function_space_type,
      ! before we can use it.
      select type(listfs => loop%payload)
      type is (function_space_type)
        ! Check the properties of the payload in the current item
        ! to see if it's the one being requested.
        fs_id = generate_fs_id(lfric_fs,        &
                               element_order_h, &
                               element_order_v, &
                               mesh_id,         &
                               ndata,           &
                               ndata_first)

        if ( fs_id == listfs%get_id() ) then
          instance => listfs
          exit
        end if
      end select

      loop => loop%next
    end do

    nullify(loop)

  end function get_existing_fs

end module function_space_collection_mod
