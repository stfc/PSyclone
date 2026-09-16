! ================================================== !
! THIS FILE IS CREATED FROM THE JINJA TEMPLATE FILE. !
! DO NOT MODIFY DIRECTLY!                            !
! ================================================== !



! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022-2025, Science and Technology Facilities Council.
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

!> This module implements a simple stand-alone writer using the PSyData
!! interface. A Fortran code instrumented with corresponding calls
!! to the PSyData API and linked in with this library will create
!! a binary file that contains all scalar values, and the dimensions
!! and content for each array.

module extract_standalone_base_mod

    use psy_data_base_mod, only : PSyDataBaseType, is_enabled

    use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                              real32, real64, &
                                              stderr => Error_Unit

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(PSyDataBaseType), public :: ExtractStandaloneBaseType

        !> The unit number to use for output
        integer                            :: unit_number

    contains

        ! The various procedures used
        procedure :: PreStart
        procedure :: PostEnd

        procedure :: WriteScalarChar
        procedure :: WriteArray1dChar
        procedure :: WriteArray2dChar
        procedure :: WriteArray3dChar
        procedure :: WriteArray4dChar
        procedure :: WriteScalarInt
        procedure :: WriteArray1dInt
        procedure :: WriteArray2dInt
        procedure :: WriteArray3dInt
        procedure :: WriteArray4dInt
        procedure :: WriteScalarLogical
        procedure :: WriteArray1dLogical
        procedure :: WriteArray2dLogical
        procedure :: WriteArray3dLogical
        procedure :: WriteArray4dLogical
        procedure :: WriteScalarReal
        procedure :: WriteArray1dReal
        procedure :: WriteArray2dReal
        procedure :: WriteArray3dReal
        procedure :: WriteArray4dReal
        procedure :: WriteScalarDouble
        procedure :: WriteArray1dDouble
        procedure :: WriteArray2dDouble
        procedure :: WriteArray3dDouble
        procedure :: WriteArray4dDouble

        !> The generic interface for providing the value of variables:
        generic, public :: ProvideVariable => &
            WriteScalarChar, &
            WriteArray1dChar, &
            WriteArray2dChar, &
            WriteArray3dChar, &
            WriteArray4dChar, &
            WriteScalarInt, &
            WriteArray1dInt, &
            WriteArray2dInt, &
            WriteArray3dInt, &
            WriteArray4dInt, &
            WriteScalarLogical, &
            WriteArray1dLogical, &
            WriteArray2dLogical, &
            WriteArray3dLogical, &
            WriteArray4dLogical, &
            WriteScalarReal, &
            WriteArray1dReal, &
            WriteArray2dReal, &
            WriteArray3dReal, &
            WriteArray4dReal, &
            WriteScalarDouble, &
            WriteArray1dDouble, &
            WriteArray2dDouble, &
            WriteArray3dDouble, &
            WriteArray4dDouble

    end type ExtractStandaloneBaseType

contains

    ! -------------------------------------------------------------------------
    !> @brief This is a one-time init function. It is not required for the kernel
    !! extraction and is therefore empty.
    subroutine extract_PSyDataInit()
        implicit none
    end subroutine extract_PSyDataInit

    ! -------------------------------------------------------------------------
    !> @brief This is a one-time shutdown function. It is not required for the kernel
    !! extraction and is therefore empty.
    subroutine extract_PSyDataShutdown()
        implicit none
    end subroutine extract_PSyDataShutdown

    ! -------------------------------------------------------------------------
    !> @brief This subroutine is the first function called when data is written out
    !! before an instrumented region of code.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] region_name The name of the instrumented region.
    !! @param[in] num_pre_vars The number of variables that are declared and
    !!            written before the instrumented region.
    !! @param[in] num_post_vars The number of variables that are also declared
    !!            before an instrumented region of code, but are written after
    !!            this region.
    subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                        num_post_vars)

#ifndef NO_MPI
        use mpi
#endif

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                            :: module_name, &
                                                               region_name
        integer, intent(in)                                 :: num_pre_vars, &
                                                               num_post_vars
#ifndef NO_MPI
        integer :: rank, size, ierr, num_digits
        character(7) :: rank_format, rank_string
#endif
        integer :: retval

        call this%PSyDataBaseType%PreStart(module_name, region_name, &
                                           num_pre_vars, num_post_vars)

#ifdef NO_MPI
        open(newunit=this%unit_number, access='sequential',  &
             form="unformatted", file=module_name//"-"//region_name//".binary")
#else
        ! Add the rank to the file name:
        call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
        num_digits = floor(log(real(size))/log(10.0)) + 1
        write(rank_format, "('(I',I0,'.',I0,')')") num_digits, num_digits
        write(rank_string, rank_format) rank
        open(newunit=this%unit_number, access='sequential',  &
             form="unformatted",                             &
             file=module_name//"-"//region_name//"-"//       &
                  trim(rank_string)//".binary")
#endif

    end subroutine PreStart

    ! -------------------------------------------------------------------------
    !> @brief This subroutine is called after the instrumented region has been
    !! executed and all values of variables after the instrumented
    !! region have been provided. This will close the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    subroutine PostEnd(this)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this

        close(this%unit_number)
        call this%PSyDataBaseType%PostEnd()

    end subroutine PostEnd

    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes the value of a scalar character(*)
    !! variable to the file. It takes the variable id from the
    !! corresponding declaration.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarChar(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        character(*), intent(in)                                    :: value
        integer                                                 :: retval

        write(this%unit_number) value
        call this%PSyDataBaseType%ProvideScalarChar(name, value)

    end subroutine WriteScalarChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 1D array of character(*)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dChar(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        character(*), dimension(:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray1dChar(name, value)

    end subroutine WriteArray1dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 2D array of character(*)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray2dChar(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        character(*), dimension(:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray2dChar(name, value)

    end subroutine WriteArray2dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 3D array of character(*)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray3dChar(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        character(*), dimension(:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray3dChar(name, value)

    end subroutine WriteArray3dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 4D array of character(*)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray4dChar(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        character(*), dimension(:,:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) size(value, 4)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray4dChar(name, value)

    end subroutine WriteArray4dChar

    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes the value of a scalar integer(kind=int32)
    !! variable to the file. It takes the variable id from the
    !! corresponding declaration.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarInt(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        integer(kind=int32), intent(in)                                    :: value
        integer                                                 :: retval

        write(this%unit_number) value
        call this%PSyDataBaseType%ProvideScalarInt(name, value)

    end subroutine WriteScalarInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 1D array of integer(kind=int32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dInt(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        integer(kind=int32), dimension(:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray1dInt(name, value)

    end subroutine WriteArray1dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 2D array of integer(kind=int32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray2dInt(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        integer(kind=int32), dimension(:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray2dInt(name, value)

    end subroutine WriteArray2dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 3D array of integer(kind=int32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray3dInt(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        integer(kind=int32), dimension(:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray3dInt(name, value)

    end subroutine WriteArray3dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 4D array of integer(kind=int32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray4dInt(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        integer(kind=int32), dimension(:,:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) size(value, 4)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray4dInt(name, value)

    end subroutine WriteArray4dInt

    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes the value of a scalar Logical(kind=4)
    !! variable to the file. It takes the variable id from the
    !! corresponding declaration.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarLogical(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        Logical(kind=4), intent(in)                                    :: value
        integer                                                 :: retval

        write(this%unit_number) value
        call this%PSyDataBaseType%ProvideScalarLogical(name, value)

    end subroutine WriteScalarLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 1D array of Logical(kind=4)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dLogical(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        Logical(kind=4), dimension(:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray1dLogical(name, value)

    end subroutine WriteArray1dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 2D array of Logical(kind=4)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray2dLogical(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        Logical(kind=4), dimension(:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray2dLogical(name, value)

    end subroutine WriteArray2dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 3D array of Logical(kind=4)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray3dLogical(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        Logical(kind=4), dimension(:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray3dLogical(name, value)

    end subroutine WriteArray3dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 4D array of Logical(kind=4)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray4dLogical(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        Logical(kind=4), dimension(:,:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) size(value, 4)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray4dLogical(name, value)

    end subroutine WriteArray4dLogical

    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes the value of a scalar real(kind=real32)
    !! variable to the file. It takes the variable id from the
    !! corresponding declaration.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarReal(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real32), intent(in)                                    :: value
        integer                                                 :: retval

        write(this%unit_number) value
        call this%PSyDataBaseType%ProvideScalarReal(name, value)

    end subroutine WriteScalarReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 1D array of real(kind=real32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dReal(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real32), dimension(:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray1dReal(name, value)

    end subroutine WriteArray1dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 2D array of real(kind=real32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray2dReal(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real32), dimension(:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray2dReal(name, value)

    end subroutine WriteArray2dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 3D array of real(kind=real32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray3dReal(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real32), dimension(:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray3dReal(name, value)

    end subroutine WriteArray3dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 4D array of real(kind=real32)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray4dReal(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real32), dimension(:,:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) size(value, 4)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray4dReal(name, value)

    end subroutine WriteArray4dReal

    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes the value of a scalar real(kind=real64)
    !! variable to the file. It takes the variable id from the
    !! corresponding declaration.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteScalarDouble(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real64), intent(in)                                    :: value
        integer                                                 :: retval

        write(this%unit_number) value
        call this%PSyDataBaseType%ProvideScalarDouble(name, value)

    end subroutine WriteScalarDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 1D array of real(kind=real64)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray1dDouble(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real64), dimension(:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray1dDouble(name, value)

    end subroutine WriteArray1dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 2D array of real(kind=real64)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray2dDouble(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real64), dimension(:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray2dDouble(name, value)

    end subroutine WriteArray2dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 3D array of real(kind=real64)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray3dDouble(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real64), dimension(:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray3dDouble(name, value)

    end subroutine WriteArray3dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine writes a 4D array of real(kind=real64)
    !! to the file.
    !! @param[in,out] this The instance of the ExtractStandaloneBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteArray4dDouble(this, name, value)

        implicit none

        class(ExtractStandaloneBaseType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        real(kind=real64), dimension(:,:,:,:), intent(in)          :: value

        integer :: retval

        if (.not. is_enabled) return
        write(this%unit_number) size(value, 1)
        write(this%unit_number) size(value, 2)
        write(this%unit_number) size(value, 3)
        write(this%unit_number) size(value, 4)
        write(this%unit_number) value

        call this%PSyDataBaseType%ProvideArray4dDouble(name, value)

    end subroutine WriteArray4dDouble


end module extract_standalone_base_mod
