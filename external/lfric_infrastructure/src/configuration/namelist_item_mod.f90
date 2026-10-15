










!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief   Defines a container object (namelist_item_type) to hold a concrete
!>          key-value pair object via their allocatable abstract type
!>          (key_value_type).
!> @details This container is required so that arrays of key-value pair objects
!>          can be created. Concrete objects of different data-types
!>          can be contained in a single array of the container object
!>          (namelist_item_type). The following concrete key_value types
!>          are supported in this module.
!>
!>          - int32_key_value_type,   int32_arr_key_value_type
!>          - int64_key_value_type,   int64_arr_key_value_type
!>          - real32_key_value_type,  real32_arr_key_value_type
!>          - real64_key_value_type,  real64_arr_key_value_type
!>          - logical_key_value_type, logical_arr_key_value_type
!>          - str_key_value_type,     str_arr_key_value_type
!>
!>          This container object allows concrete key_value types to be
!>          initialised and their values retrieved. However, the container
!>          object (namelist_item_type) does not expose the concrete types
!>          and so prevents data being altered after initialisation.
!>
!>          All concrete types are aliased so that the only public methods
!>          are:
!>          - key()
!>          - initialise( key, value )
!>          - value( output_variable )
!----------------------------------------------------------------------------
module namelist_item_mod

  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

  use constants_mod,  only: imdi, rmdi, cmdi, str_def
  use log_mod,        only: log_event, log_scratch_space, LOG_LEVEL_ERROR
  use key_value_mod,  only: key_value_type,             &
                            int32_key_value_type,       &
                            int64_key_value_type,       &
                            int32_arr_key_value_type,   &
                            int64_arr_key_value_type,   &
                            real32_key_value_type,      &
                            real64_key_value_type,      &
                            real32_arr_key_value_type,  &
                            real64_arr_key_value_type,  &
                            logical_key_value_type,     &
                            logical_arr_key_value_type, &
                            str_key_value_type,         &
                            str_arr_key_value_type

  implicit none

  private

  public :: namelist_item_type

  !=========================================
  ! Namelist item type to containe abstract
  !=========================================
  type :: namelist_item_type

    private

    class(key_value_type), allocatable :: key_value_pair

  contains
    procedure, private :: init_int32
    procedure, private :: init_int64
    procedure, private :: init_int32_arr
    procedure, private :: init_int64_arr
    procedure, private :: init_real32
    procedure, private :: init_real64
    procedure, private :: init_real32_arr
    procedure, private :: init_real64_arr
    procedure, private :: init_logical
    procedure, private :: init_logical_arr
    procedure, private :: init_str
    procedure, private :: init_str_arr

    procedure, private :: value_int32
    procedure, private :: value_int64
    procedure, private :: value_int32_arr
    procedure, private :: value_int64_arr
    procedure, private :: value_real32
    procedure, private :: value_real64
    procedure, private :: value_real32_arr
    procedure, private :: value_real64_arr
    procedure, private :: value_logical
    procedure, private :: value_logical_arr
    procedure, private :: value_str
    procedure, private :: value_str_arr

    procedure :: get_key

    generic   :: get_value  => value_int32,      value_int64,       &
                               value_int32_arr,  value_int64_arr,   &
                               value_real32,     value_real64,      &
                               value_real32_arr, value_real64_arr,  &
                               value_logical,    value_logical_arr, &
                               value_str,        value_str_arr
    generic   :: initialise => init_int32,       init_int64,        &
                               init_int32_arr,   init_int64_arr,    &
                               init_real32,      init_real64,       &
                               init_real32_arr,  init_real64_arr,   &
                               init_logical,     init_logical_arr,  &
                               init_str,         init_str_arr

  end type namelist_item_type

contains

!=========================================
! 2.0 Initialisers
!=========================================
! Initialisers for concrete key_value pair types
!
! 2.1 Initialisers for SCALAR VALUES
!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 32-bit integer.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  32-bit integer to assign to the "key"
subroutine init_int32( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*),   intent(in) :: key
  integer(int32), intent(in) :: value

  allocate(int32_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (int32_key_value_type)
    call clay%initialise( trim(key), value )
  end select

  return
end subroutine init_int32


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 64-bit integer.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  64-bit integer to assign to the "key"
subroutine init_int64( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*),   intent(in) :: key
  integer(int64), intent(in) :: value

  allocate(int64_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (int64_key_value_type)
    call clay%initialise( trim(key), value )
  end select

  return
end subroutine init_int64


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 32-bit real.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  32-bit real to assign to the "key"
subroutine init_real32( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  real(real32), intent(in) :: value

  allocate(real32_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (real32_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_real32


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 64-bit real.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  64-bit real to assign to the "key"
subroutine init_real64( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  real(real64), intent(in) :: value

  allocate(real64_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (real64_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_real64


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for logical.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  logical value to assign to the "key"
subroutine init_logical( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  logical,      intent(in) :: value

  allocate(logical_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (logical_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_logical


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for string value.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  String value to assign to the "key"
subroutine init_str( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  character(*), intent(in) :: value

  allocate(str_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (str_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_str


!=========================================
! 2.2 Initialisers for ARRAY VALUES
!=========================================

!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 32-bit integer array.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  32-bit integer array to assign to the "key"
subroutine init_int32_arr( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*),   intent(in) :: key
  integer(int32), intent(in) :: value(:)

  allocate(int32_arr_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (int32_arr_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_int32_arr


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 64-bit integer array.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  64-bit integer array to assign to the "key"
subroutine init_int64_arr( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*),   intent(in) :: key
  integer(int64), intent(in) :: value(:)

  allocate(int64_arr_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (int64_arr_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_int64_arr


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 32-bit real array.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  32-bit real array to assign to the "key"
subroutine init_real32_arr( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  real(real32), intent(in) :: value(:)

  allocate(real32_arr_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (real32_arr_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_real32_arr


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for 64-bit real array.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  64-bit real array to assign to the "key"
subroutine init_real64_arr( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  real(real64), intent(in) :: value(:)

  allocate(real64_arr_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (real64_arr_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_real64_arr


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for logical array.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  logical array to assign to the "key"
subroutine init_logical_arr( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  logical,      intent(in) :: value(:)

  allocate(logical_arr_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (logical_arr_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_logical_arr


!===========================================================
!> @brief Initialises abstract key-value pair to concrete
!>        key_value object for string array.
!> @param[in] key    String variable to use as the "key".
!> @param[in] value  String array to assign to the "key"
subroutine init_str_arr( self, key, value )

  implicit none

  class(namelist_item_type), intent(inout) :: self

  character(*), intent(in) :: key
  character(*), intent(in) :: value(:)

  allocate(str_arr_key_value_type :: self%key_value_pair)

  select type( clay => self%key_value_pair )
  class is (str_arr_key_value_type)
    call clay%initialise( key, value )
  end select

  return
end subroutine init_str_arr



!=========================================
! 3.0 Getters
!=========================================
! Retrieve data associated with keys for
! for concrete key_value pair types
!
!===========================================================
! 3.1 Querys the key string for the object
!===========================================================
!> @brief Returns the key string value used by the object.
!> @return key    String variable to use as the "key". This is
!>                the string set on initialising the abstract
!>                object to a concrete.
function get_key( self ) result( key )

  implicit none

  class(namelist_item_type), intent(in) :: self

  character(:), allocatable :: key

  if (allocated(self%key_value_pair)) then
    key = self%key_value_pair%get_key()
  else
    allocate( key, source=trim(cmdi) )
  end if

  return
end function get_key


!===========================================================
! 3.2 Getters for SCALAR VALUES
!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  32-bit integer value held in object.
subroutine value_int32( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  integer(int32),            intent(out) :: value

  select type( clay => self%key_value_pair )
  class is (int32_key_value_type)
    value = clay%value
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected int32_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_int32


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  64-bit integer value held in object.
subroutine value_int64( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  integer(int64),            intent(out) :: value

  select type( clay => self%key_value_pair )
  class is (int64_key_value_type)
    value = clay%value
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected int64_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_int64


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  32-bit real value held in object.
subroutine value_real32( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  real(real32),              intent(out) :: value

  select type (clay => self%key_value_pair)
  class is (real32_key_value_type)
    value = clay%value
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected real32_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_real32


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  64-bit real value held in object.
subroutine value_real64( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  real(real64),              intent(out) :: value

  select type (clay => self%key_value_pair)
  class is (real64_key_value_type)
    value = clay%value
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected real64_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_real64


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  logical value held in object.
subroutine value_logical( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  logical,                   intent(out) :: value

  select type (clay => self%key_value_pair)
  class is (logical_key_value_type)
    value = clay%value
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected logical_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_logical


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  String value held in object.
subroutine value_str( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  character(*),              intent(out) :: value

  select type( clay => self%key_value_pair )
  class is (str_key_value_type)
    value = trim(clay%value)
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected str_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_str


!===========================================================
! 3.3 Getters for ARRAY VALUES
!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  32-bit integer array value held in object.
subroutine value_int32_arr( self, value )

  implicit none

  class(namelist_item_type),   intent(in)  :: self
  integer(int32), allocatable, intent(out) :: value(:)

  select type( clay => self%key_value_pair )
  class is (int32_arr_key_value_type)
    allocate(value, source=clay%value)
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected int32_arr_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_int32_arr


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  64-bit integer array value held in object.
subroutine value_int64_arr( self, value )

  implicit none

  class(namelist_item_type),   intent(in)  :: self
  integer(int64), allocatable, intent(out) :: value(:)

  select type( clay => self%key_value_pair )
  class is (int64_arr_key_value_type)
    allocate(value, source=clay%value)
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected int64_arr_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_int64_arr


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  32-bit real array value held in object.
subroutine value_real32_arr( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  real(real32), allocatable, intent(out) :: value(:)

  select type( clay => self%key_value_pair )
  class is (real32_arr_key_value_type)
    allocate( value(size(clay%value)) )
    value(:) = clay%value(:)
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected real32_arr_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_real32_arr


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  64-bit real array value held in object.
subroutine value_real64_arr( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  real(real64), allocatable, intent(out) :: value(:)

  select type( clay => self%key_value_pair )
  class is (real64_arr_key_value_type)
    allocate( value( size(clay%value)) )
    value(:) = clay%value(:)
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected real64_arr_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_real64_arr


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  logical array value held in object.
subroutine value_logical_arr( self, value )

  implicit none

  class(namelist_item_type), intent(in)  :: self
  logical, allocatable,      intent(out) :: value(:)

  select type( clay => self%key_value_pair )
  class is (logical_arr_key_value_type)
    allocate( value( size(clay%value)) )
    value(:) = clay%value(:)
  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected logical_arr_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_logical_arr


!===========================================================
!> @brief Extracts the value held by the concrete type.
!> @param[out] value  String array value held in object.
subroutine value_str_arr( self, value )

  implicit none

  class(namelist_item_type), intent(in) :: self

  !> @todo This was applied with #3547. This would have been
  !>       similar to the scalar string: i.e.
  !>
  !>       character(*), allocatable, intent(out) :: value(:)
  !>
  !>       However, the revision of the Intel compiler on the XC40
  !>       produced unexpected behaviour so the length has been
  !>       limited to str_def. This should be revisited when the
  !>       XC40 compilers are later than 17.0.0.098/5.
  character(str_def), allocatable, intent(out) :: value(:)

  integer :: arr_len
  integer :: i

  select type( clay => self%key_value_pair )
  class is (str_arr_key_value_type)
    arr_len = size(clay%value)
    allocate( value(arr_len) )
    do i=1, arr_len
      value(i) = trim(clay%value(i))
    end do

  class default
    write( log_scratch_space, '(A)' ) &
        'Object is not the expected str_arr_key_value_type.'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select

  return
end subroutine value_str_arr

end module namelist_item_mod
