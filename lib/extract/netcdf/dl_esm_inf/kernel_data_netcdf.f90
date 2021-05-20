! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
! Author J. Henrichs, Bureau of Meteorology
! Modified I. Kavcic, Met Office

!> This module implements a simple NetCDF writer using the PSyData
!! interface. It is specific to the dl_esm_inf library used with
!! the GOcean API.
!! A Fortran code instrumented with corresponding calls
!! to the PSyData API and linked in with this library will nf90_create
!! a NetCDF file that contains the dimensions for each
!! field.
!!

module extract_psy_data_mod

    use extract_netcdf_base_mod, only : ExtractNetcdfBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(ExtractNetcdfBaseType), public :: extract_PsyDataType

    contains

        ! The various procedures used
        procedure :: DeclareFieldDouble
        procedure :: WriteFieldDouble

        !> The generic interface for declaring a variable:
        generic, public :: PreDeclareVariable => DeclareFieldDouble

        !> The generic interface for providing the value of variables,
        !! which in case of the NetCDF interface is written:
        generic, public :: ProvideVariable => WriteFieldDouble

    end type extract_PSyDataType

contains

    ! -------------------------------------------------------------------------
    !> Checks if the return value from a NetCDF call indicates an error.
    !! If so, print the corresponding error message and aborts the program.
    !! It is typically used as a wrapper around NetCDF calls:
    !! retval = CheckError(nf90_close(ncid))
    !! @param[in] retval The return value from a NetCDF operation.
    !! Returns the return value.
    function CheckError(retval)

        use netcdf, only : nf90_noerr, nf90_strerror

        implicit none

        integer :: CheckError
        integer, intent(in) :: retval

        if (retval /= nf90_noerr) then
            print *,"NetCDF Error:"
            print *,trim(nf90_strerror(retval))
            stop
        endif

        CheckError = retval

    end function CheckError

    ! -------------------------------------------------------------------------
    !> This is a one-time init function. It is not required for the kernel
    !! extraction and is therefore empty.
    subroutine extract_PSyDataInit()
        implicit none
    end subroutine extract_PSyDataInit

    ! -------------------------------------------------------------------------
    !> This is a one-time shutdown function. It is not required for the kernel
    !! extraction and is therefore empty.
    subroutine extract_PSyDataShutdown()
        implicit none
    end subroutine extract_PSyDataShutdown

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field as defined in
    !! dl_esm_info (r2d_field). A corresponding variable definition is added
    !! to the NetCDF file, and the variable id is stored in the var_id field.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldDouble(this, name, value)

        use netcdf
        use field_mod, only : r2d_field

        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer :: x_dimid, y_dimid, retval
        integer, dimension(2) :: dimids

        ! Map to a simple 2d-array:
        call this%DeclareArray2dDouble(name, value%data)

    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a dl_esm_field (r2d_field)
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteFieldDouble(this, name, value)

        use netcdf
        use field_mod, only : r2d_field

        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer :: retval

        ! Map the field to a simple 2d-array
        call this%WriteArray2dDouble(name, value%data)

    end subroutine WriteFieldDouble

end module extract_psy_data_mod
