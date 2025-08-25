!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

module io_value_mod

  use constants_mod,            only : str_def, r_def, r_double
  use key_value_mod,            only : abstract_value_type
  use key_value_collection_mod, only : key_value_collection_type
  use log_mod,                  only : log_event, &
                                       LOG_LEVEL_ERROR

  implicit none

  private

  public :: io_value_type, get_io_value, io_operation_interface

  !> @brief Value with associated I/O methods
  !>        that can be stored in a key-value pair
  type, extends(abstract_value_type) :: io_value_type
    character(str_def)               :: io_id
    real(kind=r_def),    allocatable :: data(:)
    procedure(io_operation_interface), pointer :: write_method => null()
    procedure(io_operation_interface), pointer :: checkpoint_read_method => null()
    procedure(io_operation_interface), pointer :: checkpoint_write_method => null()
  contains
    procedure, public :: init
    procedure, public :: set_write_behaviour
    procedure, public :: set_checkpoint_write_behaviour
    procedure, public :: set_checkpoint_read_behaviour
    procedure, public :: write_value
    procedure, public :: write_checkpoint
    procedure, public :: read_checkpoint
  end type io_value_type

  abstract interface
    subroutine io_operation_interface(self)
      import io_value_type
      class(io_value_type), intent(inout) :: self
    end subroutine io_operation_interface
  end interface

contains

!> @brief Initialiser for the io_value_type
!> @param[in] io_id The ID used for managing I/O operations
!> @param[in] data  An array holding the data
subroutine init(self, io_id, data)
  class(io_value_type), intent(inout) :: self

  character(len=*),     intent(in) :: io_id
  real(kind=r_def),     intent(in) :: data(:)

  self%io_id = io_id
  allocate(self%data, source=data)

end subroutine init

!> @brief Sets the diagnostic write behaviour for io_value
!> @param[in] write_behaviour Pointer to procedure implementing the write method
subroutine set_write_behaviour(self, write_behaviour)
  class(io_value_type), intent(inout) :: self
  procedure(io_operation_interface), pointer, intent(in) :: write_behaviour

  self%write_method => write_behaviour
end subroutine set_write_behaviour

!> @brief Sets the checkpoint write behaviour for the io_value
!> @param[in] write_behaviour A pointer to the checkpoint write behaviour
subroutine set_checkpoint_write_behaviour(self, write_behaviour)
  class(io_value_type), intent(inout) :: self
  procedure(io_operation_interface), pointer, intent(in) :: write_behaviour

  self%checkpoint_write_method => write_behaviour
end subroutine set_checkpoint_write_behaviour

!> @brief Sets the checkpoint read behavoiur for the io_value
!> @param[in] read_behaviour A pointer to the checkpoint read behaviour
subroutine set_checkpoint_read_behaviour(self, read_behaviour)
  class(io_value_type), intent(inout) :: self
  procedure(io_operation_interface), pointer, intent(in) :: read_behaviour

  self%checkpoint_read_method => read_behaviour
end subroutine set_checkpoint_read_behaviour

!> @brief Subroutine to write to the diagnostic file with write behaviour
subroutine write_value(self)
  class(io_value_type), intent(inout) :: self

  if ( associated(self%write_method) ) then
    call self%write_method()
  else
    call log_event( 'Error trying to write value ' // trim(self%io_id) // &
                    ', write method not set', LOG_LEVEL_ERROR )
  end if

end subroutine write_value

!> @brief Subroutine to write to a checkpoint file with write behaviour
subroutine write_checkpoint(self)
  class(io_value_type), intent(inout) :: self

  if ( associated(self%checkpoint_write_method) ) then
    call self%checkpoint_write_method()
  else
    call log_event( 'Error trying to write value ' // trim(self%io_id) // &
                    ', checkpoint write method not set', LOG_LEVEL_ERROR )
  end if

end subroutine write_checkpoint

!> @brief Subroutine to read data from the checkpoint file to the value
subroutine read_checkpoint(self)
  class(io_value_type), intent(inout) :: self

  if ( associated(self%checkpoint_read_method) ) then
    call self%checkpoint_read_method()
  else
    call log_event( 'Error trying to read value ' // trim(self%io_id) // &
                    ', checkpoint read method not set', LOG_LEVEL_ERROR )
  end if

end subroutine read_checkpoint

!> @brief A helper function to retrieve an io_value_type object
!>        from a key-value collection
!> @param[in] collection The collection from which to get the io_value
!> @param[in] key The key of the io_value
!> @return io_value Pointer to the extracted io_value; null if there is none
function get_io_value(collection, key) result(io_value)
  type(key_value_collection_type), intent(in) :: collection
  character(*),                    intent(in) :: key

  type(io_value_type),        pointer :: io_value
  class(abstract_value_type), pointer :: abstract_value

  call collection%get_value(trim(key), abstract_value)
  io_value => null()
  select type (abstract_value)
    type is (io_value_type)
      io_value => abstract_value
  end select

end function get_io_value

end module io_value_mod