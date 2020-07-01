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

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, public:: extract_PsyDataType
        !> The NetCDF id used for this file.
        integer, private                   :: ncid

        !> Each variable id. This is required to associate data
        !! with the declared variables: the variables are declared
        !! in the same order in which their value is provided
        integer, dimension(:), pointer :: var_id

        !> Number of variables that are written before the
        !! instrumented region.
        integer                            :: num_pre_vars

        !> Number of variables that are written after the
        !! instrumented region.
        integer                            :: num_post_vars

        !> The index of the variables as they are being declared
        !! and as they are being written. This index is used
        !! to get the variable id when writing data (which depends
        !! on the fact that declaration is done in the same order
        !! in which the values are provided).
        integer                            :: next_var_index
 
    contains
        ! The various procedures used
        procedure :: DeclareScalarInt,     WriteScalarInt,     ReadScalarInt
        procedure :: DeclareArray1dInt,    WriteArray1dInt,    ReadArray1dInt
        procedure :: DeclareArray2dInt,    WriteArray2dInt,    ReadArray2dInt
        procedure :: DeclareScalarReal,    WriteScalarReal,    ReadScalarReal
        procedure :: DeclareScalarDouble,  WriteScalarDouble,  ReadScalarDouble
        procedure :: DeclareArray1dDouble, WriteArray1dDouble, ReadArray1dDouble
        procedure :: DeclareArray3dDouble, WriteArray3dDouble, ReadArray3dDouble
        procedure :: DeclareArray4dDouble, WriteArray4dDouble, ReadArray4dDouble
        procedure :: DeclareFieldDouble,   WriteFieldDouble,   ReadFieldDouble
        procedure :: DeclareFieldVectorDouble, WriteFieldVectorDouble
        !, ReadFieldVectorDouble
        procedure :: PreStart,  PreEndDeclaration, PreEnd
        procedure :: PostStart, PostEnd
        procedure :: OpenRead

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
                                           ReadArray4dDouble,     &
                                           ReadFieldDouble 
    end type extract_PSyDataType

Contains

    ! -------------------------------------------------------------------------
    !> Checks if the return value from a netcdf call indicates an error.
    !! If so, print the corresponding error message and aborts the program.
    !! It is typically used as a wrapper around NetCDF calls:
    !! retval = CheckError(nf90_close(ncid))
    !! @param[in] retval The return value from a NetCDF operation.
    !! Returns the return value.
    function CheckError(retval) 
        use netcdf, only: nf90_noerr, nf90_strerror
        implicit none
        integer :: n=0
        integer :: CheckError
        integer, intent(in) :: retval
        if (retval /= nf90_noerr) then
            print *,"NetCDF Error:"
            print *,trim(nf90_strerror(retval))
            print *,1/n
            stop
        endif
        CheckError = retval
    end function CheckError

    ! -------------------------------------------------------------------------
    !> This is a one-time init function. It is not required for the kernel
    !! extraction and is therefore empty.
    subroutine extract_PSyDataInit()
    end subroutine extract_PSyDataInit

    ! -------------------------------------------------------------------------
    !> This is a one-time shutdown function. It is not required for the kernel
    !! extraction and is therefore empty.
    subroutine extract_PSyDataShutdown()
    end subroutine extract_PSyDataShutdown

    ! -------------------------------------------------------------------------
    !> This subroutine is the first function called when data is written out
    !! before an instrumented region of code.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] kernel_name The name of the instrumented region.
    !! @param[in] num_pre_vars The number of variables that are declared and
    !!            written before the instrumented region.
    !! @param[in] num_post_vars The number of variables that are also declared
    !!            before an instrumented region of code, but are written after
    !!            this region.
    subroutine PreStart(this, module_name, kernel_name, num_pre_vars, &
                        num_post_vars)
        use netcdf, only : nf90_create, NF90_CLOBBER
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: module_name, kernel_name
        integer, intent(in)      :: num_pre_vars, num_post_vars
        integer :: retval

        print *,"BOMJH", module_name,"{}", kernel_name, num_pre_vars, num_post_vars
        ! Open the NetCDF file
        retval = CheckError(nf90_create(module_name//"-"//kernel_name//".nc", &
                                        NF90_CLOBBER, this%ncid))
        ! Allocate the array that will store the variable IDs of all
        ! variables that are going to be declared:
        allocate(this%var_id(num_pre_vars+num_post_vars))
        ! Store number of vars, and initialise the variable index:
        this%num_pre_vars = num_pre_vars
        this%num_post_vars = num_post_vars
        this%next_var_index = 1
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called to open a NetCDF file for reading. The
    !! filename is based on the module and kernel name. This is used by a
    !! driver program that will read a NetCDF file previously created by the
    !! PSyData API.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] kernel_name The name of the instrumented region.
    subroutine OpenRead(this, module_name, kernel_name)
        use netcdf, only : nf90_open, NF90_NOWRITE
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: module_name, kernel_name
        integer :: retval

        retval = CheckError(nf90_open(module_name//"-"//kernel_name//".nc", &
                                        NF90_NOWRITE, this%ncid))
    end subroutine OpenRead

    ! -------------------------------------------------------------------------
    !> This subroutine is called once all variables are declared (this includes
    !! variables that will be written before as well as variables that are
    !! written after the instrumented region). It is used to switch the NetCDF
    !! file from declaration to writing state, and reset the next_var_index
    !! back to 1.
    !! @param[inout] this The instance of the extract_PsyDataType.
    subroutine PreEndDeclaration(this)
        use netcdf, only : nf90_enddef
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        integer :: retval
        retval = CheckError(nf90_enddef(this%ncid))
        this%next_var_index = 1
    end subroutine PreEndDeclaration

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the value of all variables has been
    !! provided (and declared). After this call the instrumented region will
    !! be executed. For this NetCDF interface this function is not required,
    !! so it is empty.
    !! @param[inout] this The instance of the extract_PsyDataType.
    subroutine PreEnd(this)
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
    end subroutine PreEnd

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed. After this call the value of variables after the instrumented
    !! region will be provided. For this NetCDF interface this function is
    !! not required, so it is empty.
    !! @param[inout] this The instance of the extract_PsyDataType.
    subroutine PostStart(this)
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
    end subroutine PostStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed and all values of variables after the instrumented
    !! region have been provided. This will close the NetCDF file.
    !! @param[inout] this The instance of the extract_PsyDataType.
    subroutine PostEnd(this)
        use netcdf, only : nf90_close
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        integer :: retval
        retval = CheckError(nf90_close(this%ncid))
    end subroutine PostEnd

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar integer value. A corresponding
    !! variable definition is added to the NetCDF file, and the variable
    !! id is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarInt(this, name, value)
        use netcdf
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
        integer :: retval

        print *,"BOMJH dsi ", name, this%next_var_index
        retval = CheckError(nf90_def_var(this%ncid, name, NF90_INT,     &
                                         this%var_id(this%next_var_index)))
        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a scalar integer variable to the
    !! NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarInt(this, name, value)
        use netcdf
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
        integer :: retval
        print *,"BOMJH wsi ", name, this%next_var_index
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine reads the value of a scalar integer variable from
    !! the NetCDF file and returns it to the user.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarInt(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(out) :: value
        integer :: retval, varid
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 1d-integer array.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dInt(this, name, value)
        use netcdf, only: nf90_def_dim, nf90_def_var, NF90_INT
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)          :: name
        integer, dimension(:), intent(in) :: value

        integer                           :: x_dimid, retval
        integer, dimension(1)             :: dimids

        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value,1), x_dimid))
        dimids =  (/ x_dimid/)
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_INT,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareArray1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine writes an 1d-integer array to the netcdf file.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dInt(this, name, value)
        use netcdf, only: nf90_put_var
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: retval

        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index), &
                                         value))
        this%next_var_index = this%next_var_index + 1

    end subroutine WriteArray1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine reads a 1d-integer array from the netcdf file.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ReadArray1dInt(this, name, value)
        use netcdf, only : nf90_inq_dimid, nf90_inquire_dimension, nf90_get_var, &
                           nf90_inq_varid
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        integer, dimension(:), allocatable, intent(out)   :: value

        integer                                           :: dim_id1, dim1
        integer                                           :: varid, retval, ierr

        ! First query the dimensions of the original array from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim_id1))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id1,  &
                                                   len=dim1))
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1), stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1," in ReadArray1dInt."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 2d-integer array.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray2dInt(this, name, value)
        use netcdf, only: nf90_def_dim, nf90_def_var, NF90_INT
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)            :: name
        integer, dimension(:,:), intent(in) :: value

        integer                             :: retval
        integer, dimension(2)               :: dimids

        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value,1), dimids(1)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim2",  &
                                         size(value,2), dimids(2)))
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_INT,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine writes a 2d-integer array to the netcdf file.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray2dInt(this, name, value)
        use netcdf, only: nf90_put_var
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        integer, dimension(:,:), intent(in)               :: value

        integer                                           :: retval

        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index), &
                                         value))
        this%next_var_index = this%next_var_index + 1

    end subroutine WriteArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine reads a 2d-integer array from the netcdf file.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ReadArray2dInt(this, name, value)
        use netcdf, only : nf90_inq_dimid, nf90_inquire_dimension, nf90_get_var, &
                           nf90_inq_varid
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        integer, dimension(:,:), allocatable, intent(out) :: value

        integer                                           :: dim_id1, dim1
        integer                                           :: dim_id2, dim2
        integer                                           :: varid, retval, ierr
        ! First query the dimensions of the original array from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim_id1))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id1,  &
                                                   len=dim1))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim2", dim_id2))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id2,  &
                                                   len=dim2))
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1, dim2), stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1,"x",dim2," in ReadArray2dInt."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar single precision value. A
    !! corresponding variable definition is added to the NetCDF file, and the
    !! variable id is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarReal(this, name, value)
        use netcdf
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL,     &
                                         this%var_id(this%next_var_index)))
        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a scalar single precision variable
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarReal(this, name, value)
        use netcdf, only: nf90_put_var
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine reads the value of a scalar single precision variable
    !! from the NetCDF file and returns it to the user
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarReal(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(out) :: value
        integer :: retval, varid
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar single precision value. A
    !! corresponding variable definition is added to the NetCDF file, and the
    !! variable id is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarDouble(this, name, value)
        use netcdf
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_DOUBLE,     &
                                         this%var_id(this%next_var_index)))
        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a scalar double precision variable
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarDouble(this, name, value)
        use netcdf, only: nf90_put_var
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteScalarDouble
    
    ! -------------------------------------------------------------------------
    !> This subroutine reads the value of a scalar double precision variable
    !! from the NetCDF file and returns it to the user
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarDouble(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(out) :: value
        integer :: retval, varid
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 1d double precision array. A corresponding
    !! variable definition is added to the NetCDF file, and the variable id
    !! is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dDouble(this, name, value)
        use netcdf, only : nf90_def_dim, nf90_def_var, NF90_REAL8
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        double precision, dimension(:), intent(in)        :: value

        integer                                           :: retval
        integer, dimension(1)                             :: dimids
        print *,"BOMJH d1dd ", name, this%next_var_index

        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value,1), dimids(1)))
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL8,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareArray1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a 1d doublre precision array
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dDouble(this, name, value)
        use netcdf
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        double precision, dimension(:), intent(in)        :: value

        integer                                           :: retval

        print *,"BOMJH w1dd ", name, this%next_var_index
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteArray1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine reads the values of a 1d double precision array.
    !! It allocates the 1d-double precision array to store the read values
    !! which is then returned to the caller. If the memory for the array can
    !! not be allocated, the application will be stopped.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dDouble(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:), allocatable, intent(out) :: value
        integer :: retval, varid, ierr
        integer :: dim1_id, dim1
        integer :: dim2_id, dim2
        integer :: dim3_id, dim3
        integer :: dim4_id, dim4
        character(100) :: x

        ! First query the dimensions of the original r2d_field from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim1_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim1_id, len=dim1))
        
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1), stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1, " in ReadArray1dDouble."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0.0d0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadArray1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 3d double precision array. A corresponding
    !! variable definition is added to the NetCDF file, and the variable id
    !! is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray3dDouble(this, name, value)
        use netcdf, only : nf90_def_dim, nf90_def_var, NF90_REAL8
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        double precision, dimension(:,:,:), intent(in)    :: value

        integer, dimension(3) :: dimids
        integer               :: retval

        print *,"BOMJH d3dd ", name, this%next_var_index

        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value,1), dimids(1)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim2",  &
                                         size(value,2), dimids(2)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim3",  &
                                         size(value,3), dimids(3)))
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL8,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareArray3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a 3d doublre precision array
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray3dDouble(this, name, value)
        use netcdf
        use field_mod, only : field_type
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:), intent(in) :: value
        integer :: retval

        print *,"BOMJH w3dd ", name, this%next_var_index
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteArray3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine reads the values of a 3d double precision array.
    !! It allocates the 4d-double precision array to store the read values
    !! which is then returned to the caller. If the memory for the array can
    !! not be allocated, the application will be stopped.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dDouble(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:), allocatable, intent(out) :: value
        integer :: retval, varid, ierr
        integer :: dim1_id, dim1
        integer :: dim2_id, dim2
        integer :: dim3_id, dim3
        character(100) :: x

        ! First query the dimensions of the original r2d_field from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim1_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim1_id,  &
                                                   len=dim1))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim2", dim2_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim2_id,  &
                                                   len=dim2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim3"), dim3_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim3_id,  &
                                                   len=dim3))
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1, dim2, dim3), stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1,"x",dim2,"x",dim3, " in ReadFieldDouble."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0.0d0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadArray3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 4d double precision array. A corresponding
    !! variable definition is added to the NetCDF file, and the variable id
    !! is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray4dDouble(this, name, value)
        use netcdf, only : nf90_def_dim, nf90_def_var, NF90_REAL8
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:,:), allocatable, intent(in) :: value
        integer :: n1_dimid, y_dimid, retval
        integer, dimension(4) :: dimids

        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value,1), dimids(1)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim2",  &
                                         size(value,2), dimids(2)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim3",  &
                                         size(value,3), dimids(3)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim4",  &
                                         size(value,4), dimids(4)))
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL8,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareArray4dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a 4d doublre precision array
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray4dDouble(this, name, value)
        use netcdf
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:,:), intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteArray4dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine reads the values of a 4d double precision array.
    !! It allocates the 4d-double precision array to store the read values
    !! which is then returned to the caller. If the memory for the array can
    !! not be allocated, the application will be stopped.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dDouble(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:,:), allocatable, intent(out) :: value
        integer :: retval, varid, ierr
        integer :: dim1_id, dim1
        integer :: dim2_id, dim2
        integer :: dim3_id, dim3
        integer :: dim4_id, dim4
        character(100) :: x

        ! First query the dimensions of the original r2d_field from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim1_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim1_id,  &
                                                   len=dim1))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim2", dim2_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim2_id,  &
                                                   len=dim2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim3"), dim3_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim3_id,  &
                                                   len=dim3))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim4", dim4_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim4_id,  &
                                                   len=dim4))
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1, dim2, dim3, dim4), stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1,"x",dim2,"x",dim3,"x",dim4," in ReadFieldDouble."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0.0d0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadArray4dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field as defined in
    !! dl_esm_info (r2d_field). A corresponding variable definition is added
    !! to the NetCDF file, and the variable id is stored in the var_id field.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldDouble(this, name, value)
        use netcdf, only : nf90_def_var, nf90_def_dim, NF90_REAL8
        use field_mod, only : field_type, field_proxy_type
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), intent(in)                      :: value

        integer                :: x_dimid, y_dimid, retval
        type(field_proxy_type) :: value_proxy
        integer, dimension(1)  :: dimids

        print *,"BOMJH dfd ", name, this%next_var_index
        value_proxy = value%get_proxy()
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value_proxy%data,1), dimids(1)))
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL8,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a dl_esm_field (r2d_field)
    !! to the NetCDF file. It takes the variable id from the corresponding
    !! declaration.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteFieldDouble(this, name, value)
        use netcdf
        use field_mod, only : field_type, field_proxy_type
        implicit none
        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_proxy_type)                            :: value_proxy
        type(field_type), intent(in)                      :: value

        integer :: retval
        print *,"BOMJH wfd ", name, this%next_var_index

        value_proxy = value%get_proxy()
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value_proxy%data(:)))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine reads the values of a dl_esm_inf field (r2d_field).
    !! It allocates a 2d-double precision array to store the read values
    !! which is then returned to the caller. If the memory for the array can
    !! not be allocated, the application will be stopped.
    !! @param[inout] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadFieldDouble(this, name, value)
        use netcdf
        implicit none

        class(extract_PsyDataType), intent(inout), target          :: this
        character(*), intent(in)                                   :: name
        double precision, dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim1_id, dim1
        integer        :: dim2_id, dim2, ierr
        character(100) :: x

        ! First query the dimensions of the original r2d_field from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim1_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim1_id,  &
                                                   len=dim1))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim2", dim2_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim2_id,  &
                                                   len=dim2))
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1, dim2), Stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1,"x",dim2," in ReadFieldDouble."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0.0d0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a field vector.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldVectorDouble(this, name, value)
        use netcdf, only: nf90_def_dim, nf90_def_var, NF90_INT
        use field_mod, only : field_type, field_proxy_type
        implicit none
        class(extract_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), dimension(:), intent(in)        :: value

        integer                             :: retval
        integer, dimension(2)               :: dimids
        type(field_proxy_type)              :: value_proxy

        print *,"BOMJH dfvd ", name, this%next_var_index

        ! All fields in a vector have the same size, so just pick the first one
        value_proxy = value(1)%get_proxy()
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         size(value,1), dimids(1)))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim2",  &
                                         size(value_proxy%data,1), dimids(2)))
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_INT,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
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

        integer                                       :: i, j, retval
        type(field_proxy_type)                        :: value_proxy
        double precision, dimension(:,:), allocatable :: tmp

        print *,"BOMJH wfvd ", name, this%next_var_index
        ! The data of the vector fields need to be copied into a 
        ! standard Fortan array that is consecutive in memory
        value_proxy = value(1)%get_proxy()
        allocate( tmp(size(value, 1), size(value_proxy%data, 1)), stat=retval )
        if(retval /= 0) then
            print *,"Cannot allocate temporary array for ", name,            &
                    " of size ", size(value,1),"x",size(value_proxy%data,1), &
                    " in WriteFieldVectorDouble."
        endif
        do i=1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            tmp(i,:) = value_proxy%data
        enddo
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index), &
                                         tmp))
        deallocate(tmp)
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteFieldVectorDouble

    ! -------------------------------------------------------------------------
    !> This subroutine reads a field vector from the netcdf file.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ReadFieldVectorDouXXble(this, name, value)
        use netcdf, only : nf90_inq_dimid, nf90_inquire_dimension, nf90_get_var, &
                           nf90_inq_varid
        implicit none
        class(extract_PSyDataType), intent(inout), target          :: this
        character(*), intent(in)                                   :: name
        double precision, dimension(:,:), allocatable, intent(out) :: value

        integer :: dim_id1, dim1
        integer :: dim_id2, dim2
        integer :: varid, retval, ierr

        ! First query the dimensions of the original array from the
        ! netcdf file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim_id1))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id1,  &
                                                   len=dim1))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim2", dim_id2))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id2,  &
                                                   len=dim2))
        ! Allocate enough space to store the values to be read:
        allocate(value(dim1, dim2), stat=ierr)
        if (ierr /= 0) then
            print *,"Cannot allocate array for ", name, &
                    " of size ", dim1,"x",dim2," in ReadFieldVectorDouble."
            stop
        endif
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadFieldVectorDouXXble

    ! -------------------------------------------------------------------------

end module extract_psy_data_mod
