!-------------------------------------------------------------------------------
!(c) Crown copyright 2020 Met Office. All rights reserved.
!The file LICENCE, distributed with this code, contains details of the terms
!under which the code may be used.
!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------

!> @brief Module for field writing routines
module write_methods_mod

  use constants_mod,        only: i_def, str_def, &
                                  str_max_filename
  use field_mod,            only: field_type, field_proxy_type
  use log_mod,              only: log_event,         &
                                  log_scratch_space, &
                                  LOG_LEVEL_INFO,LOG_LEVEL_ERROR

  implicit none

  private
  public :: nodal_write_field

contains

!> @brief   Output a field in nodal format to text file
!>@param[in] nodal_coordinates Field holding coordinate information
!>@param[in] nodal_output Field holding diagnostic data
!>@param[in] fspace_dimension Dimension of the field's function space
!>@param[in] output_unit File unit to write to
!>@param[in] fname File name to write to
!-------------------------------------------------------------------------------
subroutine nodal_write_field(nodal_coordinates, nodal_output, &
                             fspace_dimension, output_unit, fname)

  implicit none

  type(field_type),            intent(in) :: nodal_coordinates(3)
  type(field_type),            intent(in) :: nodal_output(3)
  integer(i_def),              intent(in) :: fspace_dimension
  integer(i_def),              intent(in) :: output_unit
  character(str_max_filename), intent(in) :: fname

  type(field_proxy_type) :: x_p(3), n_p(3)
  integer(i_def) :: df, undf, i

  do i = 1, 3
    x_p(i) = nodal_coordinates(i)%get_proxy()
    n_p(i) = nodal_output(i)%get_proxy()
  end do

  undf = n_p(1)%vspace%get_last_dof_owned()

  open(output_unit, file = trim(fname), status = "replace")

  if ( fspace_dimension  == 1 ) then
    do df = 1, undf
      write(output_unit,'(5e25.15e3)') x_p(1)%data(df), x_p(2)%data(df), &
                                       x_p(3)%data(df), n_p(1)%data(df)
    end do
  else
    do df = 1, undf
      write(output_unit,'(7e25.15e3)') x_p(1)%data(df), x_p(2)%data(df), &
                                       x_p(3)%data(df),                  &
                                       n_p(1)%data(df), n_p(2)%data(df), &
                                       n_p(3)%data(df)
    end do
  end if

  close(output_unit)

end subroutine nodal_write_field

end module write_methods_mod
