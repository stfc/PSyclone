! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020, Science and Technology Facilities Council.
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
! Authors J. Henrichs, Bureau of Meteorology

!> This module implements a simple netcdf writer using the PSyData 
!! interface. It is specific to the LFRic infrastructure library.
!! A Fortran code instrumented with corresponding calls
!! to the PSyData API and linked in with this library will write
!! the requested input- and output-parameters to a netcdfile file.
!! 

module extract_psy_data_mod

    use, intrinsic :: iso_fortran_env, only : int64, int32
    use extract_netcdf_base_mod, only: ExtractNetcdfBaseType, CheckError

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(ExtractNetcdfBaseType), public:: extract_PsyDataType
 
    contains
        ! The various procedures defined here
        procedure :: DeclareFieldDouble,       WriteFieldDouble
        procedure :: DeclareFieldVectorDouble, WriteFieldVectorDouble

        !> The generic interface for declaring a variable:
        generic, public :: PreDeclareVariable => DeclareScalarInt,         &
                                                 DeclareArray1dInt,        &
                                                 DeclareArray2dInt,        &
                                                 DeclareScalarReal,        &
                                                 DeclareScalarDouble,      &
                                                 DeclareArray1dDouble,     &
                                                 DeclareArray3dDouble,     &
                                                 DeclareArray4dDouble,     &
                                                 DeclareFieldVectorDouble, &
                                                 DeclareFieldDouble
        !> The generic interface for providing the value of variables,
        !! which in case of the NetCDF interface is written:                                               
        generic, public :: ProvideVariable => WriteScalarInt,              &
                                              WriteArray1dInt,             &
                                              WriteArray2dInt,             &
                                              WriteScalarReal,             &
                                              WriteScalarDouble,           &
                                              WriteArray1dDouble,          &
                                              WriteArray3dDouble,          &
                                              WriteArray4dDouble,          &
                                              WriteFieldVectorDouble,      &
                                              WriteFieldDouble

        !> The generic interface for reading in variables previously
        !! written. Used in a driver that e.g. read previously written
        !! files.
        generic, public :: ReadVariable => ReadScalarInt,         &
                                           ReadArray1dInt,        &
                                           ReadArray2dInt,        &
                                           ReadScalarReal,        &
                                           ReadScalarDouble,      &
                                           ReadArray3dDouble,     &
                                           ReadArray4dDouble
                                  
    end type extract_PSyDataType

Contains

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field as defined in
    !! dl_esm_info (r2d_field). A corresponding variable definition is added
    !! to the NetCDF file, and the variable id is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldDouble(this, name, value)
        use field_mod, only : field_type, field_proxy_type
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), intent(in)                      :: value

        type(field_proxy_type) :: value_proxy

        value_proxy = value%get_proxy()
        call this%ExtractNetcdfBaseType%DeclareArray1dDouble(name, value_proxy%data)
    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a dl_esm_field (r2d_field)
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteFieldDouble(this, name, value)
        use field_mod, only: field_type, field_proxy_type
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), intent(in)                      :: value

        type(field_proxy_type) :: value_proxy

        value_proxy = value%get_proxy()
        call this%ExtractNetcdfBaseType%WriteArray1dDouble(name, value_proxy%data)
    end subroutine WriteFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a field vector.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldVectorDouble(this, name, value)
        use netcdf, only: nf90_def_dim, nf90_def_var, NF90_REAL8
        use field_mod, only : field_type, field_proxy_type
        implicit none

        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), dimension(:), intent(in)        :: value

        integer                :: i
        type(field_proxy_type) :: value_proxy
        character(9)           :: number


        ! Provide each dimension of the vector as an individual 1d array.
        ! The base class will re-allocate internal array sizes if required
        do i = 1, size(value)
            value_proxy = value(1)%get_proxy()
            ! We add a '%' here to avoid a potential name clash if
            ! the user should have a vector field 'a' (which is now stored
            ! as a%1, a%2, ...), and a field 'a1'
            write(number, '("%",i0)') i
            call this%ExtractNetcdfBaseType%  &
                 DeclareArray1dDouble(name//trim(number), value_proxy%data)
        enddo

    end subroutine DeclareFieldVectorDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes a field vector to the netcdf file. It needs a
    !! temporary field to store the data, if the memory for this array cannot
    !! be allocated,the application will abort.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteFieldVectorDouble(this, name, value)
        use netcdf,    only: nf90_put_var
        use field_mod, only: field_type, field_proxy_type
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), dimension(:), intent(in)        :: value

        integer                :: i
        type(field_proxy_type) :: value_proxy
        character(9)           :: number

        ! Provide each dimension of the vector as an individual 1d array.
        value_proxy = value(1)%get_proxy()
        do i=1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(number, '("%",i0)') i
            call this%ExtractNetcdfBaseType% &
                 WriteArray1dDouble(name//trim(number), value_proxy%data)
        enddo

    end subroutine WriteFieldVectorDouble


end module extract_psy_data_mod
