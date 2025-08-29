










!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief    Handler object for NetCDF dimensions.
!> @details  Module containing an object which holds the relevant data for a
!!           NetCDF dimension, along with pointers to the corresponding
!!           lfric_ncdf_file_type object.
module lfric_ncdf_dims_mod

  use constants_mod,       only: i_def, str_max_filename
  use lfric_ncdf_file_mod, only: lfric_ncdf_file_type
  use log_mod,             only: log_event, log_scratch_space, LOG_LEVEL_ERROR
  use netcdf,              only: nf90_def_dim, nf90_inq_dimid,          &
                                 nf90_inquire_dimension, nf90_strerror, &
                                 nf90_noerr, nf90_nowrite

  implicit none

  private

  !> @brief  Container object for NetCDF dimensions.
  type, public :: lfric_ncdf_dims_type

    private

    integer(kind=i_def)                 :: dimid
    character(len=str_max_filename)     :: name
    integer(kind=i_def)                 :: size
    type(lfric_ncdf_file_type), pointer :: file

  contains

    procedure, public :: get_id
    procedure, public :: get_size

  end type

  interface lfric_ncdf_dims_type
    module procedure lfric_ncdf_dims_constructor
  end interface

contains

  !> @brief    Constructor for a NetCDF dimension object.
  !> @details  This routine will query the associated NetCDF file for a dimension
  !!           with the given name and if it does not exist, it will be created.
  !!           The generated ID is passed back and held in the object.
  !>
  !> @param[in]  name  The name of the dimension in the NetCDF file
  !> @param[in]  file  The associated NetCDF file
  !> @param[in]  size  The size of the dimension in the NetCDF file (optional)
  function lfric_ncdf_dims_constructor(name, file, size) result(self)

    implicit none

    type(lfric_ncdf_dims_type)                     :: self
    character(len=*),                   intent(in) :: name
    integer(kind=i_def), optional,      intent(in) :: size
    type(lfric_ncdf_file_type), target, intent(in) :: file

    integer(kind=i_def) :: ierr
    character(len=*), parameter :: routine = 'lfric_ncdf_dims_constructor'

    self%name = trim(name)
    self%file => file
    if ( present(size) ) self%size = size

    ierr = nf90_inq_dimid(self%file%get_id(), name, self%dimid)

    if (ierr /= nf90_noerr .and. file%get_io_mode() /= nf90_nowrite) then
      ierr = nf90_def_dim(self%file%get_id(), name, size, self%dimid)
    else
      ierr = nf90_inquire_dimension(self%file%get_id(), self%dimid, self%name, &
                                    self%size)
    end if

    call check_err(ierr, routine)

    return

  end function lfric_ncdf_dims_constructor

  !> @brief  Returns the NetCDF ID of the dimensions.
  !>
  !> @return  The dimension's NetCDF ID
  function get_id(self) result(id)

    implicit none

    class(lfric_ncdf_dims_type), intent(inout) :: self

    integer(kind=i_def) :: id

    id = self%dimid

    return

  end function get_id

  !> @brief  Returns the NetCDF ID of the dimensions.
  !>
  !> @return  The dimension's size
  function get_size(self) result(size)

    implicit none

    class(lfric_ncdf_dims_type), intent(inout) :: self

    integer(kind=i_def) :: size

    size = self%size

    return

  end function get_size

  !> @brief    Calls logger on error.
  !> @details  Checks the error code returned by the NetCDF file. If an error is
  !!           detected, the relevant error message is passed to the logger.
  !>
  !> @param[in]  ierr     The error code to check
  !> @param[in]  routine  The routine name that call the error check
  subroutine check_err(ierr, routine)

    implicit none

    integer(kind=i_def), intent(in) :: ierr
    character(len=*),    intent(in) :: routine

    if ( ierr /= nf90_noerr ) then
      write(log_scratch_space,*) "Error in lfric_ncdf_dims ['"//routine//"']: "//&
        trim(nf90_strerror(ierr))
      call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR )
    end if

    return

  end subroutine check_err

end module lfric_ncdf_dims_mod
