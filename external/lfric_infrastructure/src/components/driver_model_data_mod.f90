!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Generic container for model data including methods
!!        to initialise, copy and finalise the data set.
!!
!> @details This module provides a type to hold all the model fields and
!!          methods to initialise (create and read), copy and finalise (write
!!          and destroy) the data contained within the type.
!!
module driver_model_data_mod

  use field_mod,            only : field_type
  use field_collection_mod, only : field_collection_type
  use constants_mod,        only : i_def, l_def, str_def
  use log_mod,              only : log_event,       &
                                   LOG_LEVEL_ERROR, &
                                   log_scratch_space
  use linked_list_mod,      only : linked_list_type, &
                                   linked_list_item_type

  implicit none

  private

  !> Holds the working data set for a model run and other working state.
  !>
  type :: model_data_type

    private

    !> Stores all the field collections used by the model
    type( linked_list_type ), private :: field_collection_list

    contains

    procedure, private :: add_field_collection
    procedure, public  :: field_collection_exists
    procedure, public  :: add_empty_field_collection
    procedure, public  :: get_field_collection
    procedure, public  :: copy_model_data

  end type model_data_type

  public model_data_type

contains

  !> @brief Adds a field_collection to the model data. The field maintained in
  !!        the collection will either be a copy of the original or a field
  !!        pointer object containing a pointer to a field held elsewhere.
  !!
  !> @param [in] field_collection The field_collection that is to be added
  !!                              to the model_data.
  !!
  subroutine add_field_collection(self, field_collection)

    implicit none

    class(model_data_type), intent(inout)   :: self
    type(field_collection_type), intent(in) :: field_collection

    ! Check field collection name is valid, if not then exit with error
    if (trim(field_collection%get_name()) == '' .or. &
            trim(field_collection%get_name()) == 'unnamed_collection') then
      write(log_scratch_space, '(3A)') &
      'Field_collection name [', trim(field_collection%get_name()), &
      '] is an invalid field_collection name'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    ! Check if field collection exists in collection already
    ! If it does, exit with error
    if (self%field_collection_exists(trim(field_collection%get_name()))) then
      write(log_scratch_space, '(3A)') &
        'Field collection [', trim(field_collection%get_name()), &
        '] already exists in model data'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

    ! Finished checking - so the field collection must be good to add
    call self%field_collection_list%insert_item( field_collection )

  end subroutine add_field_collection

  !> @brief Check if a field collection is present the model data.
  !> @param [in] field_collection_name The name of the field collection to be
  !!                                   checked
  !> @return exists Flag stating if field collection is present or not
  !!
  function field_collection_exists(self, field_collection_name) result(exists)

    implicit none

    class(model_data_type), intent(in) :: self

    character(len=*), intent(in) :: field_collection_name
    logical(kind=l_def)          :: exists

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    ! Start at the head of the mesh collection linked list
    loop => self%field_collection_list%get_head()

    do
      ! If list is empty or we're at the end of list and we didn't find the
      ! field collection, set 'exists' to be .false.
      if ( .not. associated(loop) ) then
        exists = .false.
        exit
      end if
      ! Otherwise search list for the name of field collection we want

      ! 'cast' to the field_collection_type
      select type(listfieldcollection => loop%payload)
        type is (field_collection_type)
        if (trim(field_collection_name) == trim( &
                listfieldcollection%get_name())) then
          exists = .true.
          exit
        end if
      end select

      loop => loop%next
    end do

  end function field_collection_exists

  !> @brief Generates a field_collection and adds it to the model data.
  !> @param [in] field_collection_name The name of the field collection that is
  !!                                   to be generated and added to model data
  !> @param [in] optional table_len    Optionally specify the size of the hash
  !>                                   table used for the field collection
  subroutine add_empty_field_collection(self, &
                                        field_collection_name, &
                                        table_len)

    implicit none

    class(model_data_type), intent(inout) :: self
    character(len=*),       intent(in)    :: field_collection_name
    type(field_collection_type)           :: field_collection
    integer(i_def), optional, intent(in)  :: table_len

    integer(i_def)  :: tab_len

    ! If no table length is supplied default to 100
    tab_len=100
    if (present(table_len)) then
      tab_len = table_len
    end if

    call field_collection%initialise(name = field_collection_name, &
                                     table_len = tab_len)

    ! Make sure field collection is empty
    if ( field_collection%get_length() == 0 ) then
      call self%add_field_collection(field_collection)
    else
      write(log_scratch_space, '(3A)')             &
                'Field collection [',              &
                trim(field_collection%get_name()), &
                '] unexpectedly contains field(s)'
      call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end if

  end subroutine add_empty_field_collection

  !> @brief Access a field collection from the collection
  !> @param [in] field_collection_name The name of the field collection to be &
  !!                                   retrieved
  !> @return field Pointer to the field_collection that is extracted
  function get_field_collection(self, field_collection_name) &
          result(field_collection)

    implicit none

    class(model_data_type), intent(in)   :: self

    character(len=*), intent(in)         :: field_collection_name
    type(field_collection_type), pointer :: field_collection

    ! Pointer to linked list - used for looping through the list
    type(linked_list_item_type), pointer :: loop => null()

    ! start at the head of the mesh collection linked list
    loop => self%field_collection_list%get_head()

    do
      ! If list is empty or we're at the end of list and we didn't find the
      ! field collection, fail with an error
      if ( .not. associated(loop) ) then
        write( log_scratch_space, '(3A)' )    &
               'No field collection [',       &
               trim( field_collection_name ), &
               '] in model data'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
      ! Otherwise search list for the name of field collection we want

      ! 'cast' to the field_collection_type
      select type(listfieldcollection => loop%payload)
        type is (field_collection_type)
        if ( trim(field_collection_name) == trim( &
             listfieldcollection%get_name() ) ) then
          field_collection => listfieldcollection
          exit
        end if
      end select

      loop => loop%next
    end do

    nullify(loop)

  end function get_field_collection

  !> @brief Creates a copy of the model data
  !> @param [in] copy The model_data_type object to copy data into
  subroutine copy_model_data(self, copy)

    implicit none

    class(model_data_type), intent(inout) :: self
    type(model_data_type), intent(inout)  :: copy
    character(str_def)                    :: field_collection_name
    type(field_collection_type), pointer  :: field_collection_pointer

    ! Pointer to linked list - used for looping through the field_collection_list
    type(linked_list_item_type), pointer  :: loop => null()

    ! Copy the field_collection_list
    ! Start at the head of the field collection linked list
    loop => self%field_collection_list%get_head()

    do
      ! Loop through field_collection_list and copy
      if ( .not. associated(loop) ) then
        exit
      end if

      select type(listfieldcollection => loop%payload)
        type is (field_collection_type)
          field_collection_name = listfieldcollection%get_name()
          call copy%add_empty_field_collection(field_collection_name)
          field_collection_pointer => copy%get_field_collection(field_collection_name)
          call listfieldcollection%copy_collection(field_collection_pointer)
      end select

      loop => loop%next
    end do

  end subroutine copy_model_data

end module driver_model_data_mod
