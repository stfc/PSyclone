!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!>  @brief Module containing an abstract file type
!!
!!  @details Provides an abstract file type, together with abstract
!!           procedure interfaces.
!-------------------------------------------------------------------------------
module file_mod

use constants_mod,        only: i_def
use linked_list_data_mod, only: linked_list_data_type

implicit none

private

! IO Mode enumerations
integer(i_def), public, parameter :: file_mode_read  = 971
integer(i_def), public, parameter :: file_mode_write = 248
integer(i_def), public, parameter :: file_op_create  = 485
integer(i_def), public, parameter :: file_op_open    = 653

!-------------------------------------------------------------------------------
!> @brief Abstract file type
!!
!! @details  Defines the interface for a family of IO strategies,
!!           which extend this abstract type.
!-------------------------------------------------------------------------------

!--------- File type ---------
type, public, abstract, extends(linked_list_data_type) :: file_type
  private

contains
  procedure (new_interface ),  deferred :: file_new
  procedure (open_interface ), deferred :: file_open
  procedure (close_interface), deferred :: file_close

end type file_type

!-------------------------------------------------------------------------------
! Abstract interfaces
!-------------------------------------------------------------------------------
abstract interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Open an existing file
  !!
  !! @param[in] file_name Filename
  !-----------------------------------------------------------------------------
  subroutine new_interface(self, file_name)

    import :: file_type

    !Arguments
    class(file_type), intent(inout) :: self
    character(len=*), intent(in)    :: file_name

  end subroutine new_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Open an existing file
  !!
  !! @param[in] file_name Filename
  !! @param[in] file_mode Action identifier on file
  !-----------------------------------------------------------------------------
  subroutine open_interface(self, file_name, file_mode)

    import :: file_type, i_def

    !Arguments
    class(file_type),         intent(inout) :: self
    character(len=*),         intent(in)    :: file_name
    integer(i_def), optional, intent(in)    :: file_mode

  end subroutine open_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Close a file
  !-----------------------------------------------------------------------------
  subroutine close_interface(self)

    import :: file_type

    !Arguments
    class(file_type), intent(inout) :: self

  end subroutine close_interface

end interface

end module file_mod
