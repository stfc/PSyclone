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

  use constants_mod,      only : i_def
  use function_space_mod, only : function_space_type
  use fs_continuity_mod,  only : fs_enumerator, fs_name, name_from_functionspace
  use log_mod,            only : log_event, log_scratch_space,    &
                                 LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use mesh_mod,           only : mesh_type
  use linked_list_mod,    only : linked_list_type,                &
                                 linked_list_item_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Type which is is a collection of function spaces held in a linked list
  !-----------------------------------------------------------------------------
  type, public :: function_space_collection_type
    private
    type(linked_list_type) :: fs_list

    !> An unused allocatable integer that prevents an intenal compiler error
    !> with the Gnu Fortran compiler. Adding an allocatable forces the compiler
    !> to accept that the object has a finaliser. It gets confused without it.
    !> This is a workaround for GCC bug id 61767 - when this bug is fixed, the
    !> integer can be removed.
    integer(i_def), allocatable :: dummy_for_gnu

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
  !> @param[in] mesh           mesh object
  !> @param[in] element_order  function space order
  !> @param[in] lfric_fs       lfric id code for given supported function space
  !> @param[in] ndata   The number of data values to be held at each dof location
  function get_fs( self,          &
                   mesh,          &
                   element_order, &
                   lfric_fs,      &
                   ndata )  result(fs)

    implicit none

    class(function_space_collection_type), intent(inout) :: self

    type(mesh_type), intent(in), pointer  :: mesh
    integer(i_def),  intent(in)           :: element_order
    integer(i_def),  intent(in)           :: lfric_fs
    integer(i_def),  intent(in), optional :: ndata

    type(function_space_type), pointer :: fs

    integer(i_def) :: ndata_sz, i_fs_name
    integer(i_def) :: mesh_id

    nullify(fs)

    if (present(ndata)) then
      ndata_sz = ndata
    else
      ndata_sz = 1
    end if

    if (.not.(any(fs_enumerator == lfric_fs))) then
      write(log_scratch_space, "(A,I0,3A,*(A,:,', '))")                 &
      "Function space type (", lfric_fs, ") is not defined in LFRic.",  &
      new_line("A"), "Available function space types are: ",            &
      (trim(fs_name(i_fs_name)), i_fs_name = 1, size(fs_name))
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    if (element_order < 0) then
      write(log_scratch_space, '(A,I0)') &
      'Function space element order must be >= 0   ', element_order
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    mesh_id = mesh%get_id()

    fs => get_existing_fs( self,          &
                           mesh_id,       &
                           element_order, &
                           lfric_fs,      &
                           ndata_sz )

    if (.not. associated(fs)) then

      call self%fs_list%insert_item(               &
               function_space_type( mesh_id,       &
                                    element_order, &
                                    lfric_fs,      &
                                    ndata_sz) )

      write(log_scratch_space, '(A,I0,A)')              &
      'Generated order-', element_order,                &
      ' ' // trim(name_from_functionspace(lfric_fs)) // &
      '-function space singleton'
      call log_event(log_scratch_space, LOG_LEVEL_TRACE)

      fs => get_existing_fs( self,          &
                             mesh_id,       &
                             element_order, &
                             lfric_fs,      &
                             ndata_sz )

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
    if (allocated(self%dummy_for_gnu)) deallocate(self%dummy_for_gnu)

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
  ! Private function (not accessible through the API - and only called from within
  ! this module) to scan the function space collection for function space with the
  ! given propeties and return a pointer to it. A null pointer is returned if the
  ! requested function space does not exist.
  !
  ! param[in] mesh_id  ID of mesh object
  ! param[in] element_order  function space order
  ! param[in] lfric_fs       lfric id code for given supported function space
  ! param[in] ndata   The number of data values to be held at each dof location
  ! return <pointer> Pointer to function space object or null()
  function get_existing_fs( self,           &
                            mesh_id,        &
                            element_order,  &
                            lfric_fs,       &
                            ndata ) result(instance)

    implicit none

    class(function_space_collection_type) :: self
    integer(i_def), intent(in) :: mesh_id
    integer(i_def), intent(in) :: element_order
    integer(i_def), intent(in) :: lfric_fs
    integer(i_def), intent(in) :: ndata

    type(function_space_type), pointer :: instance

    type(linked_list_item_type), pointer :: loop

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
        ! to see if its the one being requested.
        if ( mesh_id == listfs%get_mesh_id()             .and. &
             element_order == listfs%get_element_order() .and. &
             lfric_fs == listfs%which()                  .and. &
             ndata == listfs%get_ndata() ) then

          instance => listfs
          exit
        end if
      end select

      loop => loop%next
    end do

    nullify(loop)

  end function get_existing_fs

end module function_space_collection_mod
