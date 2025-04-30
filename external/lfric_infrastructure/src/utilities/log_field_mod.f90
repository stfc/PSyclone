!-----------------------------------------------------------------------------
! (c) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Provides functionality to output field values to the log. This will be
!>        used for debugging, so there may not be any usage from production
!>        code

module log_field_mod

  use abstract_external_field_mod, only: abstract_external_field_type
  use constants_mod,               only: i_def
  use field_mod,                   only: field_type, field_proxy_type
  use function_space_mod,          only: function_space_type
  use log_mod,                     only: log_event,       &
                                         log_level_trace, &
                                         log_level_error, &
                                         log_scratch_space
  implicit none

  private

  ! Private type can only be instantiated from within this module
  type, extends(abstract_external_field_type) :: log_field_external_type
    !> Log level at which to write the field data
    integer(i_def) :: log_level = log_level_trace
  contains
    !> Initialises the object
    procedure, public :: initialise
    !> Set the logging level for the next message
    procedure, public :: set_log_level
    !> Copy data from the LFRic field and pass it to the logger
    procedure, public :: copy_from_lfric
    !> Dummy required by abstract - just produces an error message
    procedure, public :: copy_to_lfric
  end type  log_field_external_type

public log_field

contains

  !> @brief Initialises the external field for logging field data
  !> @param [in] lfric_field_ptr Pointer to an lfric field
  subroutine initialise( self, lfric_field_ptr )
  implicit none

  class(log_field_external_type),     intent(inout) :: self
  type(field_type), pointer, intent(in)    :: lfric_field_ptr

  call self%abstract_external_field_initialiser(lfric_field_ptr)

  end subroutine initialise


  subroutine set_log_level(self, log_level)
  implicit none

  class(log_field_external_type), intent(inout) :: self
  integer(i_def),                 intent(in)    :: log_level

  self%log_level = log_level

  end subroutine set_log_level

  !>@brief Outputs field values to the log
  !>@param return_code Optional return code from the copy_from procedure
  subroutine copy_from_lfric(self, return_code)
  implicit none

  class(log_field_external_type), intent(inout) :: self
  integer(i_def), intent(out), optional :: return_code

  type(field_type), pointer :: field => null()
  type(field_proxy_type):: fieldp
  type(function_space_type), pointer :: function_space => null()
  integer(i_def) :: df

  field => self%get_lfric_field_ptr()
  fieldp = field%get_proxy()

  ! Get function space from parent
  function_space => field%get_function_space()

  do df=1,function_space%get_undf()
    write( log_scratch_space, '( I6, E16.8 )' ) df,fieldp%data( df )
    call log_event( log_scratch_space, self%log_level )
  end do

  if(present(return_code))return_code = 0

  end subroutine copy_from_lfric


  !>@brief Dummy required by abstract - just produced an error message
  !>@param return_code The return code from the copy_to procedure
  subroutine copy_to_lfric( self, return_code )
  implicit none

  class(log_field_external_type), intent(inout) :: self
  integer(i_def), intent(out), optional :: return_code

  call log_event( "ERROR: log_field_external_type has no copy_to_lfric functionality", &
                  log_level_error )

  if(present(return_code))return_code = 1

  end subroutine copy_to_lfric


  !>@brief Public wrapper to hide all the mechanics of the external field.
  !>       The user can then simply call this subroutine to log field values
  !>@param field The field to output dofs from
  !>@param log_level Optional logging level to log the output to.
  !>                 If not present the output is logged at log_level_trace
  !>@param label Optional label to write to the log before the data
  subroutine log_field(field, log_level, label)
  implicit none

  type(field_type), target, intent(in) :: field
  integer(i_def), optional, intent(in) :: log_level
  character(*),   optional, intent(in) :: label
  type(field_type), pointer            :: fieldp
  type(log_field_external_type)        :: field_log
  integer(i_def)                       :: level

  if(present(log_level))then
    level = log_level
  else
    level = log_level_trace
  end if
  if(present(label)) call log_event( label, level )

  fieldp => field
  call field_log%initialise(fieldp)
  call field_log%set_log_level(level)
  call field_log%copy_from_lfric()

  end subroutine log_field

end module log_field_mod
