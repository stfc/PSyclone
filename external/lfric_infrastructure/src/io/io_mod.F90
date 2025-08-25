!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!>  @brief    Module for IO subroutines.
!>  @details  Holds miscellaneous reading/writing and utility routines used by
!!            the infrastructure's simple IO system.
!>
module io_mod

  use constants_mod,        only: i_def, str_max_filename
  use field_mod,            only: field_type, field_proxy_type
  use field_parent_mod,     only: field_parent_proxy_type
  use file_mod,             only: FILE_MODE_READ, FILE_MODE_WRITE, &
                                  FILE_OP_CREATE, FILE_OP_OPEN
  use lfric_mpi_mod,        only: global_mpi
  use lfric_ncdf_dims_mod,  only: lfric_ncdf_dims_type
  use lfric_ncdf_field_mod, only: lfric_ncdf_field_type
  use lfric_ncdf_file_mod,  only: lfric_ncdf_file_type

  implicit none

  private

  public :: nodal_write_field,       &
            checkpoint_write_netcdf, &
            checkpoint_read_netcdf,  &
            ts_fname

contains

!> @brief   Output a field in nodal format to text file.
!>
!> @param[in]  nodal_coordinates  Field holding coordinate information
!> @param[in]  level              Field holding level information
!> @param[in]  nodal_output       Field holding diagnostic data
!> @param[in]  fspace_dimension   Dimension of the field's function space
!> @param[in]  output_unit        File unit to write to
!> @param[in]  fname              File name to write to
subroutine nodal_write_field(nodal_coordinates, level, nodal_output, &
                             fspace_dimension, output_unit, fname)

  implicit none

  type(field_type),                intent(in) :: nodal_coordinates(3)
  type(field_type),                intent(in) :: level
  type(field_type),                intent(in) :: nodal_output(3)
  integer(kind=i_def),             intent(in) :: fspace_dimension
  integer(kind=i_def),             intent(in) :: output_unit
  character(len=str_max_filename), intent(in) :: fname

  type(field_proxy_type) :: x_p(3), l_p, n_p(3)
  integer(kind=i_def)    :: df, undf, i

  do i = 1, 3
    x_p(i) = nodal_coordinates(i)%get_proxy()
    n_p(i) = nodal_output(i)%get_proxy()
  end do

  l_p = level%get_proxy()
  undf = n_p(1)%vspace%get_last_dof_owned()

  open(output_unit, file = trim(fname), status = "replace")
  write(output_unit,'(A)') 'x = ['

  if ( fspace_dimension  == 1 ) then
    do df = 1, undf
      write(output_unit, '(5e25.15e3)') x_p(1)%data(df), x_p(2)%data(df), &
                                        x_p(3)%data(df), l_p%data(df),    &
                                        n_p(1)%data(df)
    end do
  else
    do df = 1, undf
      write(output_unit, '(7e25.15e3)') x_p(1)%data(df), x_p(2)%data(df), &
                                        x_p(3)%data(df), l_p%data(df),    &
                                        n_p(1)%data(df), n_p(2)%data(df), &
                                        n_p(3)%data(df)
    end do
  end if

  write(output_unit,'(A)') '];'
  close(output_unit)

end subroutine nodal_write_field

!> @brief    I/O handler for writing a NetCDF checkpoint.
!> @details  Legacy method for writing checkpoints - Note this routine accepts
!!           a field name but does not use it - this is to keep the interface
!!           the same for all methods.
!>
!> @param[in]  field_name   Name of the field to write
!> @param[in]  file_name    Name of the file to write to
!> @param[in]  field_proxy  The proxy of the field to write
subroutine checkpoint_write_netcdf(field_name, file_name, field_proxy)

  implicit none

  character(len=*),               intent(in) :: field_name
  character(len=*),               intent(in) :: file_name
  class(field_parent_proxy_type), intent(in) :: field_proxy

  type(lfric_ncdf_file_type)  :: ncdf_file
  type(lfric_ncdf_dims_type)  :: ncdf_dims
  type(lfric_ncdf_field_type) :: ncdf_field

  ncdf_file = lfric_ncdf_file_type(file_name, open_mode=FILE_OP_CREATE, &
                                   io_mode=FILE_MODE_WRITE)

  select type(field_proxy)

    type is (field_proxy_type)
      ncdf_dims = lfric_ncdf_dims_type("field_size", ncdf_file, size(field_proxy%data(:)))
      ncdf_field = lfric_ncdf_field_type(field_name, ncdf_file, ncdf_dims)

      call ncdf_file%close_definition()

      call ncdf_field%write_data(field_proxy%data(:))

  end select

  call ncdf_file%close_file()

end subroutine checkpoint_write_netcdf

!> @brief  I/O handler for reading a NetCDF checkpoint.
!>
!> @param[in]      field_name   Name of the field to read
!> @param[in]      file_name    Name of the file to read from
!> @param[in,out]  field_proxy  The proxy of the field to be read into
subroutine checkpoint_read_netcdf(field_name, file_name, field_proxy)

  implicit none

  character(len=*),               intent(in)    :: field_name
  character(len=*),               intent(in)    :: file_name
  class(field_parent_proxy_type), intent(inout) :: field_proxy

  type(lfric_ncdf_file_type)  :: ncdf_file
  type(lfric_ncdf_dims_type)  :: ncdf_dims
  type(lfric_ncdf_field_type) :: ncdf_field

  ncdf_file = lfric_ncdf_file_type(file_name, open_mode=FILE_OP_OPEN, &
                                   io_mode=FILE_MODE_READ)

  select type(field_proxy)

    type is (field_proxy_type)
      ncdf_dims = lfric_ncdf_dims_type("field_size", ncdf_file)
      ncdf_field = lfric_ncdf_field_type(field_name, ncdf_file, ncdf_dims)

      call ncdf_field%read_data(field_proxy%data(:))

  end select

  call ncdf_file%close_file()

end subroutine checkpoint_read_netcdf

!> @brief   Function to determine output filename at a given timestep.
!>
!> @param[in]  stem_name   String file stem
!> @param[in]  file_type   String used to identify file type (e.g. 'nodal')
!> @param[in]  field_name  Name of the field
!> @param[in]  ts          Time step
!> @param[in]  ext         File extension
!> @return  The timestep-dependent file name
function ts_fname(stem_name, file_type, field_name, ts, ext)

  implicit none

  character(len=*),    intent(in) :: field_name, stem_name, &
                                     file_type, ext
  integer(kind=i_def), intent(in) :: ts
  character(len=str_max_filename) :: rank_name
  character(len=str_max_filename) :: ts_fname
  integer(kind=i_def)             :: total_ranks
  integer(kind=i_def)             :: local_rank

  total_ranks = global_mpi%get_comm_size()
  local_rank = global_mpi%get_comm_rank()

  if( total_ranks == 1 )then
      rank_name=ext
    else
      write(rank_name, "("".Rank"",I6.6)") local_rank
  end if

  write(ts_fname, '(A,A,A,A,A,I6.6,A)') trim(stem_name), "_", &
        trim(file_type), trim(field_name), "_T", ts, trim(rank_name)//ext

end function ts_fname

end module io_mod
