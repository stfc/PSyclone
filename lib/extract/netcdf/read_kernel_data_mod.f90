! ================================================== !
! THIS FILE IS CREATED FROM THE JINJA TEMPLATE FILE. !
! DO NOT MODIFY DIRECTLY!                            !
! ================================================== !



! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
! Author: J. Henrichs, Bureau of Meteorology

!> This module implements a simple NetCDF reader. It provides the functions:
!! OpenRead:      opens a file for reading
!! ReadScalar...:           reads the specified scalar value
!! ReadArray1dDouble, ... : allocates and reads the specified array type.

module read_kernel_data_mod

    use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                              real32, real64, &
                                              stderr => Error_Unit

    implicit none

    !> This is the data type that manages the information required
    !! to read data from a NetCDF file.

    type, public :: ReadKernelDataType

        !> The NetCDF ID used for this file.
        integer                            :: ncid

    contains

        ! The various procedures used
        procedure :: OpenRead

        procedure :: ReadScalarChar
        procedure :: ReadArray1dChar
        procedure :: ReadArray2dChar
        procedure :: ReadArray3dChar
        procedure :: ReadArray4dChar
        procedure :: ReadScalarInt
        procedure :: ReadArray1dInt
        procedure :: ReadArray2dInt
        procedure :: ReadArray3dInt
        procedure :: ReadArray4dInt
        procedure :: ReadScalarLong
        procedure :: ReadArray1dLong
        procedure :: ReadArray2dLong
        procedure :: ReadArray3dLong
        procedure :: ReadArray4dLong
        procedure :: ReadScalarLogical
        procedure :: ReadArray1dLogical
        procedure :: ReadArray2dLogical
        procedure :: ReadArray3dLogical
        procedure :: ReadArray4dLogical
        procedure :: ReadScalarReal
        procedure :: ReadArray1dReal
        procedure :: ReadArray2dReal
        procedure :: ReadArray3dReal
        procedure :: ReadArray4dReal
        procedure :: ReadScalarDouble
        procedure :: ReadArray1dDouble
        procedure :: ReadArray2dDouble
        procedure :: ReadArray3dDouble
        procedure :: ReadArray4dDouble

        !> The generic interface for reading the value of variables.
        !! This is not part of the official PSyData API, but is used in
        !! the drivers created by PSyclone.
        generic, public :: ReadVariable => &
            ReadScalarChar, &
            ReadArray1dChar, &
            ReadArray2dChar, &
            ReadArray3dChar, &
            ReadArray4dChar, &
            ReadScalarInt, &
            ReadArray1dInt, &
            ReadArray2dInt, &
            ReadArray3dInt, &
            ReadArray4dInt, &
            ReadScalarLong, &
            ReadArray1dLong, &
            ReadArray2dLong, &
            ReadArray3dLong, &
            ReadArray4dLong, &
            ReadScalarLogical, &
            ReadArray1dLogical, &
            ReadArray2dLogical, &
            ReadArray3dLogical, &
            ReadArray4dLogical, &
            ReadScalarReal, &
            ReadArray1dReal, &
            ReadArray2dReal, &
            ReadArray3dReal, &
            ReadArray4dReal, &
            ReadScalarDouble, &
            ReadArray1dDouble, &
            ReadArray2dDouble, &
            ReadArray3dDouble, &
            ReadArray4dDouble

    end type ReadKernelDataType

contains

    ! -------------------------------------------------------------------------
    !> @brief Checks if the return value from a NetCDF call indicates an error.
    !! If so, print the corresponding error message and aborts the program.
    !! It is typically used as a wrapper around NetCDF calls:
    !! retval = CheckError(nf90_close(ncid))
    !! @param[in] retval The return value from a NetCDF operation.
    !! Returns the return value.
    function CheckError(retval)

        use netcdf, only : nf90_noerr, nf90_strerror

        implicit none

        integer, intent(in) :: retval
        integer             :: CheckError

        if (retval /= nf90_noerr) then
            write(stderr, *) "NetCDF Error:"
            write(stderr, *) trim(nf90_strerror(retval))
            stop
        endif
        CheckError = retval

    end function CheckError

    ! -------------------------------------------------------------------------
    !> @brief This subroutine is called to open a NetCDF file for reading. The
    !! filename is based on the module and kernel name. This is used by a
    !! driver program that will read a NetCDF file previously created by the
    !! PSyData API.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] region_name The name of the instrumented region.
    subroutine OpenRead(this, module_name, region_name)

        use netcdf, only : nf90_open, NF90_NOWRITE

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: module_name, &
                                                            region_name
        integer :: retval

        retval = CheckError(nf90_open(module_name//"-"//region_name//".nc", &
                                        NF90_NOWRITE, this%ncid))

    end subroutine OpenRead


    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the value of a scalar character(*)
    !! variable from the NetCDF file and returns it to the user. Note that
    !! this function is not part of the PSyData API, but it is convenient to
    !! have these functions together here. The driver can then be linked with
    !! this  PSyData library and will be able to read the files.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarChar(this, name, value)

        use netcdf, only : nf90_inq_varid, nf90_get_var

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: name
        character(*), intent(out)                            :: value

        integer                                          :: retval, varid

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadScalarChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 1D array of character(*)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dChar(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        character(*), dimension(:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dChar."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise the whole array with "".
        value = ""
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray1dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 2D array of character(*)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray2dChar(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        character(*), dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dChar."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise the whole array with "".
        value = ""
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray2dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 3D array of character(*)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dChar(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        character(*), dimension(:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dChar."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise the whole array with "".
        value = ""
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray3dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 4D array of character(*)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dChar(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        character(*), dimension(:,:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3,dim_size4
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%4"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size4))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dChar."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise the whole array with "".
        value = ""
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray4dChar


    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the value of a scalar integer(kind=int32)
    !! variable from the NetCDF file and returns it to the user. Note that
    !! this function is not part of the PSyData API, but it is convenient to
    !! have these functions together here. The driver can then be linked with
    !! this  PSyData library and will be able to read the files.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarInt(this, name, value)

        use netcdf, only : nf90_inq_varid, nf90_get_var

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: name
        integer(kind=int32), intent(out)                            :: value

        integer                                          :: retval, varid

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadScalarInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 1D array of integer(kind=int32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dInt(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int32), dimension(:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dInt."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray1dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 2D array of integer(kind=int32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray2dInt(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int32), dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dInt."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray2dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 3D array of integer(kind=int32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dInt(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int32), dimension(:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dInt."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray3dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 4D array of integer(kind=int32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dInt(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int32), dimension(:,:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3,dim_size4
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%4"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size4))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dInt."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray4dInt


    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the value of a scalar integer(kind=int64)
    !! variable from the NetCDF file and returns it to the user. Note that
    !! this function is not part of the PSyData API, but it is convenient to
    !! have these functions together here. The driver can then be linked with
    !! this  PSyData library and will be able to read the files.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarLong(this, name, value)

        use netcdf, only : nf90_inq_varid, nf90_get_var

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: name
        integer(kind=int64), intent(out)                            :: value

        integer                                          :: retval, varid

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadScalarLong



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 1D array of integer(kind=int64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dLong(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int64), dimension(:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dLong."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray1dLong



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 2D array of integer(kind=int64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray2dLong(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int64), dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dLong."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray2dLong



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 3D array of integer(kind=int64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dLong(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int64), dimension(:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dLong."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray3dLong



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 4D array of integer(kind=int64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dLong(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        integer(kind=int64), dimension(:,:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3,dim_size4
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%4"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size4))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dLong."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray4dLong


    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the value of a scalar Logical(kind=4)
    !! variable from the NetCDF file and returns it to the user. Note that
    !! this function is not part of the PSyData API, but it is convenient to
    !! have these functions together here. The driver can then be linked with
    !! this  PSyData library and will be able to read the files.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarLogical(this, name, value)

        use netcdf, only : nf90_inq_varid, nf90_get_var

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: name
        Logical(kind=4), intent(out)                            :: value

        integer                                          :: retval, varid
        integer                                          :: tmp

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, tmp))
        value = tmp .eq. 1

    end subroutine ReadScalarLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 1D array of Logical(kind=4)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dLogical(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        Logical(kind=4), dimension(:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1
        integer        :: ierr
        integer, dimension(:), allocatable :: tmp

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dLogical."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! We cannot read logical directly, so read an int array.
        ! Allocate enough space to store the values to be read:
        allocate(tmp(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate int-array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dLogical."
            stop
        endif
        retval = CheckError(nf90_get_var(this%ncid, varid, tmp))
        ! Then convert each '1' in this array to .true., everything else
        ! to .false.
        value = tmp == 1
        deallocate(tmp)

    end subroutine ReadArray1dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 2D array of Logical(kind=4)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray2dLogical(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        Logical(kind=4), dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2
        integer        :: ierr
        integer, dimension(:,:), allocatable :: tmp

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dLogical."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! We cannot read logical directly, so read an int array.
        ! Allocate enough space to store the values to be read:
        allocate(tmp(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate int-array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dLogical."
            stop
        endif
        retval = CheckError(nf90_get_var(this%ncid, varid, tmp))
        ! Then convert each '1' in this array to .true., everything else
        ! to .false.
        value = tmp == 1
        deallocate(tmp)

    end subroutine ReadArray2dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 3D array of Logical(kind=4)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dLogical(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        Logical(kind=4), dimension(:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3
        integer        :: ierr
        integer, dimension(:,:,:), allocatable :: tmp

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dLogical."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! We cannot read logical directly, so read an int array.
        ! Allocate enough space to store the values to be read:
        allocate(tmp(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate int-array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dLogical."
            stop
        endif
        retval = CheckError(nf90_get_var(this%ncid, varid, tmp))
        ! Then convert each '1' in this array to .true., everything else
        ! to .false.
        value = tmp == 1
        deallocate(tmp)

    end subroutine ReadArray3dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 4D array of Logical(kind=4)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dLogical(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        Logical(kind=4), dimension(:,:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3,dim_size4
        integer        :: ierr
        integer, dimension(:,:,:,:), allocatable :: tmp

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%4"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size4))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dLogical."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! We cannot read logical directly, so read an int array.
        ! Allocate enough space to store the values to be read:
        allocate(tmp(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate int-array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dLogical."
            stop
        endif
        retval = CheckError(nf90_get_var(this%ncid, varid, tmp))
        ! Then convert each '1' in this array to .true., everything else
        ! to .false.
        value = tmp == 1
        deallocate(tmp)

    end subroutine ReadArray4dLogical


    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the value of a scalar real(kind=real32)
    !! variable from the NetCDF file and returns it to the user. Note that
    !! this function is not part of the PSyData API, but it is convenient to
    !! have these functions together here. The driver can then be linked with
    !! this  PSyData library and will be able to read the files.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarReal(this, name, value)

        use netcdf, only : nf90_inq_varid, nf90_get_var

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: name
        real(kind=real32), intent(out)                            :: value

        integer                                          :: retval, varid

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadScalarReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 1D array of real(kind=real32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dReal(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real32), dimension(:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dReal."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray1dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 2D array of real(kind=real32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray2dReal(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real32), dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dReal."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray2dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 3D array of real(kind=real32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dReal(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real32), dimension(:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dReal."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray3dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 4D array of real(kind=real32)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dReal(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real32), dimension(:,:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3,dim_size4
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%4"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size4))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dReal."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray4dReal


    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the value of a scalar real(kind=real64)
    !! variable from the NetCDF file and returns it to the user. Note that
    !! this function is not part of the PSyData API, but it is convenient to
    !! have these functions together here. The driver can then be linked with
    !! this  PSyData library and will be able to read the files.
    !! @param[in,out] this The instance of the ReadKernelDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value The read value is stored here.
    subroutine ReadScalarDouble(this, name, value)

        use netcdf, only : nf90_inq_varid, nf90_get_var

        implicit none

        class(ReadKernelDataType), intent(inout), target :: this
        character(*), intent(in)                         :: name
        real(kind=real64), intent(out)                            :: value

        integer                                          :: retval, varid

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadScalarDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 1D array of real(kind=real64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray1dDouble(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real64), dimension(:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1, &
                            " in ReadArray1dDouble."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray1dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 2D array of real(kind=real64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray2dDouble(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real64), dimension(:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2, &
                            " in ReadArray2dDouble."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray2dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 3D array of real(kind=real64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray3dDouble(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real64), dimension(:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3, &
                            " in ReadArray3dDouble."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray3dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine reads the values of a 4D array of real(kind=real64)
    !! It allocates memory for the allocatable parameter 'value' to store the
    !! read values which is then returned to the caller. If the memory for the
    !! array cannot be allocated, the application will be stopped.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[out] value An allocatable, unallocated 2d-double precision array
    !!             which is allocated here and stores the values read.
    subroutine ReadArray4dDouble(this, name, value)

        use netcdf

        implicit none

        class(ReadKernelDataType), intent(inout), target             :: this
        character(*), intent(in)                                     :: name
        real(kind=real64), dimension(:,:,:,:), allocatable, intent(out) :: value

        integer        :: retval, varid
        integer        :: dim_id
        integer        :: dim_size1,dim_size2,dim_size3,dim_size4
        integer        :: ierr

        ! First query the dimensions of the original array from the
        ! NetCDF file
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%1"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size1))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%2"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size2))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%3"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size3))
        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim%4"), &
                                           dim_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim_id, &
                                                   len=dim_size4))

        ! Allocate enough space to store the values to be read:
        allocate(value(dim_size1,dim_size2,dim_size3,dim_size4), Stat=ierr)
        if (ierr /= 0) then
            write(stderr,*) "Cannot allocate array for ", name, &
                            " of size ", dim_size1,dim_size2,dim_size3,dim_size4, &
                            " in ReadArray4dDouble."
            stop
        endif

        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        ! The compiler will convert the double precision value to the right
        ! type (e.g. int or single precision).
        value = 0.0d0
        retval = CheckError(nf90_get_var(this%ncid, varid, value))

    end subroutine ReadArray4dDouble


end module read_kernel_data_mod
