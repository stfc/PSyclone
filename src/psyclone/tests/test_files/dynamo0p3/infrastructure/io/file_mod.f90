

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------
!>  @brief Module containing an abstract file type and a file type for XIOS
!!         interface
!!
!!  @details Provides an abstract file type, together with abstract
!!           procedure interfaces. Also provides a container for file properties
!!           needed for XIOS interface
!-------------------------------------------------------------------------------
module file_mod

  use constants_mod,        only: i_def, str_def, str_max_filename
  use linked_list_data_mod, only: linked_list_data_type

implicit none

private

!-------------------------------------------------------------------------------
!> @brief Abstract file type
!!
!! @details  Defines the interface for a family of IO strategies,
!!           which extend this abstract type.
!-------------------------------------------------------------------------------

!--------- File type ---------
type, abstract, public :: file_type
  private

contains
  procedure (new_open_interface ),      deferred :: file_open
  procedure (new_open_interface ),      deferred :: file_new
  procedure (close_interface),          deferred :: file_close

end type file_type

!-------------------------------------------------------------------------------
!> @brief XIOS file type
!!
!! @details  Container for file properties need by XIOS.
!-------------------------------------------------------------------------------

!--------- XIOS file type ---------
type, extends(linked_list_data_type), public :: xios_file_type
  private

  !> Unique identifier for XIOS file handle
  character(str_def) :: xios_id
  !> Path to file
  character(str_max_filename) :: path
  !> File output frequency
  integer(i_def) :: output_freq
  !> XIOS ID of associated field group
  character(str_def) :: field_group = "unset"

contains
  procedure, public :: init_xios_file
  procedure, public :: get_xios_id
  procedure, public :: get_path
  procedure, public :: get_output_freq
  procedure, public :: get_field_group

end type xios_file_type

!-------------------------------------------------------------------------------
! Abstract interfaces
!-------------------------------------------------------------------------------
abstract interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Open an existing file, or create a new file.
  !!
  !! @param[in] self               The file strategy object.
  !! @param[in] file_name          Filename
  !-----------------------------------------------------------------------------

  subroutine new_open_interface(self, file_name)
    import :: file_type

    !Arguments
    class(file_type),       intent(inout) :: self
    character(len=*),       intent(in)    :: file_name

  end subroutine new_open_interface

  !-----------------------------------------------------------------------------
  !> @brief  Interface: Close a file
  !!
  !! @param[in] self               The file strategy object.
  !-----------------------------------------------------------------------------

  subroutine close_interface(self)
    import :: file_type

    !Arguments
    class(file_type), intent(inout) :: self

  end subroutine close_interface

end interface

contains
!-------------------------------------------------------------------------------
! XIOS file type procedures
!-------------------------------------------------------------------------------

!> Initialises a file object for use with XIOS
!> @param[in] input_xios_id The XIOS name for the file
!> @param[in, optional] path The path to the file
!> @param[in, optional] freq The file output frequency
!> @param[in, optional] field_group_id The associated field group id
subroutine init_xios_file(self, input_xios_id, path, freq, field_group_id)

  implicit none

  !Arguments
  class(xios_file_type),      intent(inout) :: self
  character(len=*),           intent(in)    :: input_xios_id
  character(len=*), optional, intent(in)    :: path
  integer(i_def),   optional, intent(in)    :: freq
  character(len=*), optional, intent(in)    :: field_group_id

  self%xios_id = input_xios_id

  ! If path is not present then default to use xios_id as file name in working
  ! directory
  if (present(path)) then
    self%path = path
  else
    self%path = input_xios_id
  end if

  if (present(freq)) then
    self%output_freq = freq
  else
    ! -999 used as flag for frequency not to be set
    self%output_freq = -999
  end if

  if (present(field_group_id)) self%field_group = field_group_id

end subroutine init_xios_file

!> Getter for XIOS file ID
!> @param[out] output_xios_id The XIOS ID for the file
function get_xios_id(self) result(output_xios_id)

  implicit none
  !Arguments
  class(xios_file_type),   intent(inout) :: self

  character(str_def) :: output_xios_id

  output_xios_id = self%xios_id

end function get_xios_id

!> Getter for file path
!> @param[out] output_path The path to the file
function get_path(self) result(output_path)

  implicit none
  !Arguments
  class(xios_file_type),            intent(inout) :: self

  character(str_max_filename) :: output_path

  output_path = self%path

end function get_path

!> Getter for file output frequency
!> @param[out] file_freq The file output frequency
function get_output_freq(self) result(file_freq)

  implicit none
  !Arguments
  class(xios_file_type), intent(inout) :: self

  integer(i_def) :: file_freq

  file_freq = self%output_freq

end function get_output_freq

!> Getter for associated field group ID
!> @param[out] field_group_id The associated field group id
function get_field_group(self) result(field_group_id)

  implicit none
  !Arguments
  class(xios_file_type), intent(inout) :: self

  character(str_def) :: field_group_id

  field_group_id = self%field_group

end function get_field_group

end module file_mod
