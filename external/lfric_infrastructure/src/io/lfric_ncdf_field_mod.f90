










!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief  Handler object for NetCDF field data.
module lfric_ncdf_field_mod

  use constants_mod,       only: r_def, dp_native, i_def, str_def, str_long
  use lfric_ncdf_file_mod, only: lfric_ncdf_file_type
  use lfric_ncdf_dims_mod, only: lfric_ncdf_dims_type
  use log_mod,             only: log_event, log_scratch_space, LOG_LEVEL_ERROR
  use netcdf,              only: nf90_strerror, nf90_noerr, nf90_double,     &
                                 nf90_def_var, nf90_put_var, nf90_get_var,   &
                                 nf90_inq_varid, nf90_put_att, nf90_get_att, &
                                 nf90_rename_var, nf90_nowrite

  implicit none

  private

  !> @brief  Container object for a NetCDF field variable.
  type, public :: lfric_ncdf_field_type

    private

    integer(kind=i_def)                 :: varid
    character(len=str_def)              :: name
    type(lfric_ncdf_file_type), pointer :: file
    type(lfric_ncdf_dims_type), pointer :: dimensions
    logical                             :: is_scalar = .false.
    integer(kind=i_def)                 :: data_type = nf90_double

  contains

    procedure, public :: read_data
    procedure, public :: write_data
    procedure, public :: set_char_attribute
    procedure, public :: get_char_attribute
    procedure, public :: set_real_attribute
    procedure, public :: get_real_attribute

  end type

  interface lfric_ncdf_field_type
    module procedure lfric_ncdf_field_constructor
  end interface

contains

  !> @brief    Constructor for a NetCDF field object.
  !> @details  This routine will query the associated NetCDF file for a variable
  !!           with the given name and if it does not exist (and the file has
  !!           write permissions), it will be created. The generated ID is
  !!           passed back and held in the object.
  !>
  !> @param[in]  name       The name of the field in the NetCDF file
  !> @param[in]  file       The associated NetCDF file
  !> @param[in]  dims       The associated NetCDF dimensions
function lfric_ncdf_field_constructor(name, file, dims) result(self)

    implicit none

    type(lfric_ncdf_field_type)                              :: self
    character(len=*),                             intent(in) :: name
    type(lfric_ncdf_file_type), target,           intent(in) :: file
    type(lfric_ncdf_dims_type), target, optional, intent(in) :: dims

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'lfric_ncdf_field_constructor'
    character(len=str_long)     :: cmess

    self%name = trim(name)
    self%file => file

    if (present(dims)) then
      self%dimensions => dims
    else
      self%is_scalar = .true.
    end if

    ierr = nf90_inq_varid(self%file%get_id(), name, self%varid)

    if (ierr /= nf90_noerr .and. file%get_io_mode() /= nf90_nowrite) then
      if (present(dims)) then
        ierr = nf90_def_var(self%file%get_id(), name, self%data_type, &
                          self%dimensions%get_id(), self%varid)
      else
        ierr = nf90_def_var(self%file%get_id(), name, self%data_type, self%varid)
      end if
      cmess = "Defining variable with ID: " // name

    else
      cmess = "Inquiring for variable with ID: " // name

    end if

    call check_err(ierr, routine, cmess)

    return

  end function lfric_ncdf_field_constructor


  !> @brief  Reads a variable's data from the NetCDF file.
  !>
  !> @param[out]  field_data  Field data read from the file
  subroutine read_data(self, field_data)

    implicit none

    class(lfric_ncdf_field_type), intent(in)  :: self
    real(kind=r_def),             intent(out) :: field_data(:)

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'read_data'
    character(len=str_long)     :: cmess

    ierr = nf90_get_var(self%file%get_id(), self%varid, field_data(:))

    cmess = "Getting NetCDF variable with ID: " // trim(self%name)
    call check_err(ierr, routine, cmess)

    return

  end subroutine read_data

  !> @brief  Writes data to the NetCDF field.
  !>
  !> @param[in]  field_data  The field data to be written
  subroutine write_data(self, field_data)

    implicit none

    class(lfric_ncdf_field_type), intent(inout) :: self
    real(kind=r_def),             intent(in)    :: field_data(:)

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'write_data'
    character(len=str_long)     :: cmess

    if (self%is_scalar) then
      ierr = nf90_put_var(self%file%get_id(), self%varid, field_data(1))
    else
      ierr = nf90_put_var(self%file%get_id(), self%varid, field_data)
    end if

    cmess = "Writing NetCDF variable with ID: " // trim(self%name)
    call check_err(ierr, routine, cmess)

    return

  end subroutine write_data

  !> @brief    Assigns character attribute to the NetCDF variable.
  !> @details  Adds additional information to NetCDF variable in character
  !!           form.
  !>
  !> @param[in]  attr_name   The name of attribute to be set
  !> @param[in]  attr_value  The value of the attribute
  subroutine set_char_attribute(self, attr_name, attr_value)

    implicit none

    class(lfric_ncdf_field_type), intent(in) :: self
    character(len=*),             intent(in) :: attr_name
    character(len=*),             intent(in) :: attr_value

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'set_char_attribute'
    character(len=str_long)     :: cmess

    ierr = nf90_put_att(self%file%get_id(), self%varid, attr_name, &
                        trim(attr_value))

    cmess = "Setting NetCDF attribute '" // attr_name // &
            "' for variable with ID: " // trim(self%name)
    call check_err(ierr, routine, cmess)

    return

  end subroutine set_char_attribute

  !> @brief  Reads a field attribute in character form from the NetCDF file.
  !>
  !> @param[in]  attr_name   The name of attribute to be read
  !> @return                 The return value of the attribute
  function get_char_attribute(self, attr_name) result(attr_value)

    implicit none

    class(lfric_ncdf_field_type), intent(in) :: self
    character(len=*),             intent(in) :: attr_name

    character(len=str_long) :: attr_value

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'get_char_attribute'
    character(len=str_long)     :: cmess

    ierr = nf90_get_att(self%file%get_id(), self%varid, attr_name, attr_value)
    attr_value = trim(attr_value)

    cmess = "Getting NetCDF attribute '" // attr_name // &
            "' for variable with ID: " // trim(self%name)
    call check_err(ierr, routine, cmess)

    return

  end function get_char_attribute

  !> @brief    Assigns real number attribute to the NetCDF variable.
  !> @details  Adds additional information to NetCDF variables in real number
  !!           form.
  !>
  !> @param[in]  attr_name   The name of attribute to be set
  !> @param[in]  attr_value  The value of the attribute
  subroutine set_real_attribute(self, attr_name, attr_value)

    implicit none

    class(lfric_ncdf_field_type), intent(in) :: self
    character(len=*),             intent(in) :: attr_name
    real(kind=dp_native),         intent(in) :: attr_value

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'set_real_attribute'
    character(len=str_long)     :: cmess

    ierr = nf90_put_att(self%file%get_id(), self%varid, attr_name, attr_value)

    cmess = "Setting NetCDF attribute '" // attr_name // &
            "' for variable with ID: " // trim(self%name)
    call check_err(ierr, routine, cmess)

    return

  end subroutine set_real_attribute

  !> @brief  Reads a field attribute in real number form from the NetCDF file.
  !>
  !> @param[in]  attr_name   The name of attribute to be read
  !> @return                 The return value of the attribute
  function get_real_attribute(self, attr_name) result(attr_value)

    implicit none

    class(lfric_ncdf_field_type), intent(in) :: self
    character(len=*),             intent(in) :: attr_name

    real(kind=dp_native) :: attr_value

    integer(kind=i_def)         :: ierr
    character(len=*), parameter :: routine = 'read_attribute'
    character(len=str_long)     :: cmess

    ierr = nf90_get_att(self%file%get_id(), self%varid, attr_name, attr_value)

    cmess = "Getting NetCDF attribute '" // attr_name // &
            "' for variable with ID: " // trim(self%name)
    call check_err(ierr, routine, cmess)

    return

  end function get_real_attribute

  !> @brief    Calls logger on error.
  !> @details  Checks the error code returned by the NetCDF file. If an error is
  !!           detected, the relevant error message is passed to the logger.
  !>
  !> @param[in] ierr    The error code to check
  !> @param[in] routine The routine name that call the error check
  !> @param[in] cmess   Comment message for the error report
  subroutine check_err(ierr, routine, cmess)

    implicit none

    integer(kind=i_def),     intent(in) :: ierr
    character(len=*),        intent(in) :: routine
    character(len=str_long), intent(in) :: cmess

    if ( ierr /= nf90_noerr ) then
      write(log_scratch_space,*) "Error in lfric_ncdf_field ['"//routine//"']: "//&
        trim(cmess) // " " // trim(nf90_strerror(ierr))
      call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR )
    end if

    return

  end subroutine check_err

end module lfric_ncdf_field_mod
