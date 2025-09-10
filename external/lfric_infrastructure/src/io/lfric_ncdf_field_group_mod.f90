










!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief  Handler object for NetCDF groups of NetCDF fields.
module lfric_ncdf_field_group_mod

  use constants_mod,       only: i_def, str_def
  use lfric_ncdf_dims_mod, only: lfric_ncdf_dims_type
  use lfric_ncdf_file_mod, only: lfric_ncdf_file_type
  use log_mod,             only: log_event, log_scratch_space, LOG_LEVEL_ERROR
  use netcdf,              only: nf90_inquire_variable, nf90_get_att, nf90_put_att

  implicit none

  private

  !> @brief  Container object for a NetCDF field variable.
  type, public :: lfric_ncdf_field_group_type

    private

    type(lfric_ncdf_file_type), pointer :: file
    integer(kind=i_def), allocatable    :: varids(:)

  contains

    procedure, public :: get_group_subset
    procedure, public :: add_coordinate

  end type

  interface lfric_ncdf_field_group_type
    module procedure lfric_ncdf_field_group_constructor
  end interface

contains

  !> @brief  Constructor for a NetCDF field group object.
  !>
  !> @param[in]  file        The associated NetCDF file
  !> @param[in]  varid_list  The list of field netCDF IDs within the group
  function lfric_ncdf_field_group_constructor(file, varid_list) result(self)

    implicit none

    type(lfric_ncdf_field_group_type)              :: self
    type(lfric_ncdf_file_type), target, intent(in) :: file
    integer(kind=i_def),                intent(in) :: varid_list(:)

    self%file => file

    allocate(self%varids(size(varid_list)))
    self%varids = varid_list

    return

  end function lfric_ncdf_field_group_constructor

  !> @brief  Returns a new lfric_ncdf_field_group object containing a subset of
  !!         the list of variable netCDF IDs which satisfy the input criteria

  function get_group_subset(self, dimension) result(new_group)

    implicit none

    class(lfric_ncdf_field_group_type),   intent(inout) :: self
    type(lfric_ncdf_dims_type), optional, intent(inout) :: dimension

    type(lfric_ncdf_field_group_type) :: new_group
    integer(kind=i_def), allocatable :: varid_list(:), dimids(:)
    integer(kind=i_def) :: ierr, ndims, id_id

    do id_id = 1, size(self%varids)
      ierr = nf90_inquire_variable(self%file%get_id(), self%varids(id_id), ndims=ndims)
      allocate(dimids(ndims))
      ierr = nf90_inquire_variable(self%file%get_id(), self%varids(id_id), dimids=dimids)

      if (any(dimids == dimension%get_id())) then
        varid_list = append(varid_list, self%varids(id_id))
      end if
      deallocate(dimids)
    end do

    new_group = lfric_ncdf_field_group_type(self%file, varid_list)

    return

  end function get_group_subset


  !> @brief    Assigns attributes to the NetCDF variables.
  !> @details  Adds additional information to NetCDF variables such as
  !!           variable names and descriptions.
  !>
  !> @param[in]  coord_name  The name of coordinate to be added to group's fields
  subroutine add_coordinate(self, coord_name)

    implicit none

    class(lfric_ncdf_field_group_type), intent(in) :: self
    character(len=*),             intent(in) :: coord_name

    character(len=str_def) :: coord_string, total_coord_string
    integer(kind=i_def)    :: ierr, var_id, i

    do i = 1, size(self%varids)
      var_id = self%varids(i)
      ierr = nf90_get_att(self%file%get_id(), var_id, "coordinates", &
                          coord_string)

      total_coord_string = trim(coord_string) // " " // trim(coord_name)

      ierr = nf90_put_att(self%file%get_id(), var_id, "coordinates", &
                          trim(total_coord_string))
    end do

    return

  end subroutine add_coordinate


  !> @brief  Appends an integer to the end of a list of integers.
  !>
  !> @param[in]  list    A list of integers
  !> @param[in]  new_id  The integer ID to appended to the list of integers
  function append(list, new_id) result(new_list)

    implicit none

    integer(kind=i_def), allocatable, intent(in) :: list(:)
    integer(kind=i_def),              intent(in) :: new_id

    integer(kind=i_def), allocatable :: new_list(:)

    if (.not. allocated(list)) then
      allocate(new_list(1))
      new_list(1) = new_id
    else
      allocate(new_list(size(list)+1))
      new_list(1:size(list)) = list
      new_list(size(list)+1) = new_id
    end if

  end function append

end module lfric_ncdf_field_group_mod
