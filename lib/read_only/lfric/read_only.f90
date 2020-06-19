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

!> This module implements a verification that read-only fields are
!! not overwritten (due to memory overwrites etc)
!! 

module read_only_verify_psy_data_mod
    use, intrinsic :: iso_fortran_env, only : int64, int32
        use field_mod, only : field_type

    implicit none

    !> Maximum string length for module- and region-names
    integer, parameter :: MAX_STRING_LENGTH = 512

    !> This is the data type that stores a checksum for each read-only
    !! variable. A static instance of this type is created for each
    !! instrumented region with PSyclone.

    type, public:: read_only_verify_PSyDataType
        !> This field stores a 64-bit integer checksum for each
        !! variable.
        integer(kind=int64), dimension(:), allocatable :: checksums

        !> The index of the variables as they are being declared
        !! and as they are being written. This index is used
        !! to get the variable id when writing data (which depends
        !! on the fact that declaration is done in the same order
        !! in which the values are provided).
        integer :: next_var_index

        !> During the declaration of the variable, this variable is
        !! used to count the number of checksums to store. Reason for
        !! this are vector fields, which are declared as one variable,
        !! but we store the checksum for each member individually.
        integer :: count_checksums

        !> This boolean flag switches from 'computing and storing checksum'
        !! to 'verify checksum'.
        logical :: verify_checksums

        !> Store the name of the module and region
        character(MAX_STRING_LENGTH) :: module_name, region_name

    contains
        ! The various procedures used
        procedure :: DeclareScalarInt,    ChecksumScalarInt
        procedure :: DeclareArray1dInt,   ChecksumArray1dInt
        procedure :: DeclareArray2dInt,   ChecksumArray2dInt
        procedure :: DeclareArray3dInt,   ChecksumArray3dInt
        procedure :: DeclareScalarReal,   ChecksumScalarReal
        procedure :: DeclareArray1dDouble,ChecksumArray1dDouble
        procedure :: DeclareArray3dDouble,ChecksumArray3dDouble
        procedure :: DeclareArray4dDouble,ChecksumArray4dDouble
        procedure :: DeclareScalarDouble, ChecksumScalarDouble
        procedure :: DeclareFieldDouble,  ChecksumFieldDouble
        procedure :: DeclareFieldVectorDouble,  ChecksumFieldVectorDouble
        procedure :: PreStart, PreEndDeclaration, PreEnd
        procedure :: PostStart, PostEnd

        !> The generic interface for declaring a variable:
        generic, public :: PreDeclareVariable => DeclareScalarInt,    &
                                                 DeclareArray1dInt,   &
                                                 DeclareArray2dInt,   &
                                                 DeclareArray3dInt,   &
                                                 DeclareScalarReal,   &
                                                 DeclareArray1dDouble,&
                                                 DeclareArray3dDouble,&
                                                 DeclareArray4dDouble,&
                                                 DeclareScalarDouble, &
                                                 DeclareFieldDouble,  &
                                                 DeclareFieldVectorDouble

        !> The generic interface for providing the value of variables,
        !! which in case of the NetCDF interface is written:                                               
        generic, public :: ProvideVariable => ChecksumScalarInt,    &
                                              ChecksumArray1dInt,   &
                                              ChecksumArray2dInt,   &
                                              ChecksumArray3dInt,   &
                                              ChecksumScalarReal,   &
                                              ChecksumArray1dDouble,&
                                              ChecksumArray3dDouble,&
                                              ChecksumArray4dDouble,&
                                              ChecksumScalarDouble, &
                                              ChecksumFieldDouble,  &
                                              ChecksumFieldVectorDouble

    end type read_only_verify_PSyDataType

Contains

    ! -------------------------------------------------------------------------
    !> This subroutine is the first function called when an instrumented region
    !! is entered. It initialises this object, and stores module and regin
    !! names. 
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] kernel_name The name of the instrumented region.
    !! @param[in] num_pre_vars The number of variables that are declared and
    !!            written before the instrumented region.
    !! @param[in] num_post_vars The number of variables that are also declared
    !!            before an instrumented region of code, but are written after
    !!            this region.
    subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                        num_post_vars)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: module_name, region_name
        integer, intent(in)      :: num_pre_vars, num_post_vars

        if (num_pre_vars /= num_post_vars) then
            print *,"The same number of variables must be provided before and"
            print *,"after the instrumented region. But the values are:"
            print *,"Before: ", num_pre_vars, " after: ", num_post_vars
            stop
        endif
        print *,"PreStart ", module_name, " ", region_name
        this%count_checksums = 0
        this%verify_checksums = .false.
        this%module_name = module_name
        this%region_name = region_name
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called once all variables are declared. It makes
    !! sure that the next variable index is starting at 1 again.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PreEndDeclaration(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        print *,"PreEndDecl, #vars ", this%count_checksums
        flush(6)
        ! During the declaration the number of checksums to be
        ! stored was counted, so allocate the array now:
        if (.not. allocated(this%checksums)) then
            allocate(this%checksums(this%count_checksums))
        endif
        print *,"PreEndDecl2, #vars ", this%count_checksums, shape(this%checksums)
        flush(6)

        this%next_var_index = 1
    end subroutine PreEndDeclaration
    ! -------------------------------------------------------------------------
    !> This subroutine is called after the value of all variables has been
    !! provided (and declared). After this call the instrumented region will
    !! be executed. Nothing is required to be done here.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PreEnd(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
    end subroutine PreEnd
    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed. After this call the value of variables after the instrumented
    !! region will be provided. This subroutine sets the 'verify_checksum'
    !! flag to true, causing all further checksum calls to verify that the
    !! checksum has not changed. It also resets the next variable index to 1
    !! again.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PostStart(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        this%verify_checksums = .true.
        this%next_var_index = 1
    end subroutine PostStart
    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed and all values of variables after the instrumented
    !! region have been provided. No special functionality required here.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PostEnd(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
    end subroutine PostEnd

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar integer value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a single integer.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumScalarInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value

        if (this%verify_checksums) then
            if (value /= this%checksums(this%next_var_index)) then
                print *,"--------------------------------------"
                print *,"Integer variable ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                ! In case of integer variables, we can use the checksum as the
                ! original value:
                print *,"Original value: ", this%checksums(this%next_var_index)
                print *,"New value:      ", value
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = value
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares an array of integer value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:), intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareArray1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for an array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumArray1dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i

        checksum = 0
        do i=1, size(value, 1)
            checksum = checksum + value(i)
        enddo
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                print *,"--------------------------------------"
                print *,"Integer array ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", checksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumArray1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 2d-array of integer value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray2dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:,:), intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 2d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumArray2dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = 0
        do j=1, size(value, 2)
            do i=1, size(value, 1)
                checksum = checksum + value(i, j)
            enddo
        enddo
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                print *,"--------------------------------------"
                print *,"2d Integer array ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", checksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares an array of integer value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray3dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:,:,:), intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareArray3dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for an array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumArray3dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer, dimension(:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j, k

        checksum = 0
        do k=1, size(value, 3)
            do j=1, size(value, 2)
                do i=1, size(value, 1)
                    checksum = checksum + value(i, j, k)
                enddo
            enddo
        enddo
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                print *,"--------------------------------------"
                print *,"3d Integer array ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", checksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumArray3dInt

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar single precision value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums)
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumScalarReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        real :: orig_value
        integer(kind=int64) :: chksum64
        integer(kind=int32) :: chksum32

        ! Type-case from real to 32-bit int (same size, so
        ! no undefined bits in result)
        chksum32 = transfer(value, chksum32)
        ! Now assign to 64 bit, so we have a 64 bit checksum
        ! without potentiall undefined bits.
        chksum64 = chksum32

        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= chksum64) then
                ! Convert back to 32 bit, and then cast to real:
                chksum32 = this%checksums(this%next_var_index)
                orig_value = transfer(chksum32, orig_value)
                print *,"--------------------------------------"
                print *,"Real variable ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                print *,"Original value: ", orig_value
                print *,"New value:      ", value
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = chksum64
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 1d array of double precision values.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:), intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareArray1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 1d double precision array.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumArray1dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:), intent(in) :: value
        integer(kind=int64) :: checksum

        checksum = ComputeChecksum1d(value)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= checksum) then
                print *,"--------------------------------------"
                print *,"1d double array ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", checksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumArray1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 3d array of double precision values.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray3dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:), intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareArray3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 3d double precision array.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumArray3dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum

        checksum = ComputeChecksum3d(value)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= checksum) then
                print *,"--------------------------------------"
                print *,"3d double array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", checksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", trim(name), checksum
    end subroutine ChecksumArray3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a 4d array of double precision values.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray4dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:,:), intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareArray4dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 4d double precision array.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumArray4dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum

        checksum = ComputeChecksum4d(value)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= checksum) then
                print *,"--------------------------------------"
                print *,"3d double array ", name, " has been modified in ", &
                    this%module_name," : ", this%region_name
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", checksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumArray4dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar single precision value.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareScalarDouble
    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums)
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumScalarDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        double precision             :: orig_value
        integer(kind=int64):: cksum

        ! We can use the 'cast'ed 64 bit integer values directly as checksum
        cksum = transfer(value, cksum)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= cksum) then
                ! Convert 64 bit integer back to 64 bit double precision:
                orig_value = transfer(this%checksums(this%next_var_index), orig_value)
                print *,"--------------------------------------"
                print *,"Double precision variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                print *,"Original value: ", orig_value
                print *,"New value:      ", value
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field based on the LFRIc
    !! infrastructure type field_type.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine DeclareFieldDouble(this, name, value)
!        use field_mod, only : field_type
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), intent(in) :: value
        print *,"Declare field ", name
        this%count_checksums = this%count_checksums + 1
    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksum for a field based on the
    !! LFRic infrastructure type field_type. Depending on state
    !! (this%verify_checksum) it either stores the checksum, or compares it
    !! with a previously computed checksum.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ChecksumFieldDouble(this, name, value)
        use field_mod, only : field_type, field_proxy_type
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), intent(in) :: value
        type(field_proxy_type) :: value_proxy
        integer(kind=int64):: cksum

        value_proxy = value%get_proxy()
        cksum = ComputeChecksum1d(value_proxy%data)
        print *,"Provide field ", name, value_proxy%data(1), cksum, size(value_proxy%data, 1), shape(value_proxy%data)
        print *,"LOC", loc(value_proxy%data(1))
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= cksum) then
                print *,"--------------------------------------"
                print *,"Double precision field ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                print *,"Original checksum: ", this%checksums(this%next_var_index)
                print *,"New checksum:      ", cksum
                print *,"--------------------------------------"
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif
        this%next_var_index = this%next_var_index + 1
        print *,"Checked: ", name
    end subroutine ChecksumFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a vector of double precision field based on
    !! the LFRIc infrastructure type field_type.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine DeclareFieldVectorDouble(this, name, value)
!        use field_mod, only : field_type
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), dimension(3), intent(in) :: value
        print *,"Declare", name, " size ", size(value, 1)
        this%count_checksums = this%count_checksums + size(value, 1)
    end subroutine DeclareFieldVectorDouble

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksums for a vector of fields based on
    !! the LFRic infrastructure type field_type. Depending on state
    !! (this%verify_checksum) it either stores the checksums (one for each
    !! field of the vector), or compares them with previously computed
    !! checksums.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The vector of fields.
    subroutine ChecksumFieldVectorDouble(this, name, value)
        use field_mod, only : field_type, field_proxy_type
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), dimension(:), intent(in) :: value
        type(field_proxy_type) :: value_proxy
        integer(kind=int64):: cksum
        integer :: i
        character(512) :: new_name
        print *,"checksumfieldvectordouble", size(value, 1)
        return

        do i=1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            print *,"Provide vector", name, value_proxy%data(1), size(value_proxy%data, 1), shape(value_proxy%data)
            new_name = name
            call this%ChecksumFieldDouble(name, value(i))
            this%next_var_index = this%next_var_index + 1
        enddo
        print *,"Checked: ", name
    end subroutine ChecksumFieldVectorDouble

    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 3d double
    !! precision Fortran array.
    function ComputeChecksum1d(field) result(checksum)
        implicit none
        integer(kind=int64) :: checksum
        double precision, dimension(:) :: field
        integer :: j
        print *,"computechecksum1d", field(1), size(field, 1), shape(field), transfer(field(1), checksum)
        checksum = 0
        do j=1, size(field, 1)
            checksum = checksum + transfer(field(j), checksum)
        enddo
    end function ComputeChecksum1d

    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 4d double
    !! precision Fortran array.
    function ComputeChecksum3d(field) result(checksum)
        implicit none
        integer(kind=int64) :: checksum
        double precision, dimension(:,:,:) :: field
        integer :: i, j, k
        print *,"computechecksum3d", size(field, 1), size(field, 2), size(field, 3), shape(field)
        checksum = 0
        do k=1, size(field, 3)
            do j=1, size(field, 2)
                do i=1, size(field, 1)
                    checksum = checksum + transfer(field(i,j,k), checksum)
                enddo
            enddo
        enddo
    end function ComputeChecksum3d

    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 4d double
    !! precision Fortran array.
    function ComputeChecksum4d(field) result(checksum)
        implicit none
        integer(kind=int64) :: checksum
        double precision, dimension(:,:,:,:) :: field
        integer :: i, j, k, l
        print *,"computechecksum4d", size(field, 1), size(field, 2), size(field, 3), shape(field)
        checksum = 0
        do l=1, size(field, 4)
            do k=1, size(field, 3)
                do j=1, size(field, 2)
                    do i=1, size(field, 1)
                        checksum = checksum + transfer(field(i,j,k, l), checksum)
                    enddo
                enddo
            enddo
        enddo
    end function ComputeChecksum4d

    ! -------------------------------------------------------------------------
    
subroutine test(my_field)
!    use field_mod, only : field_type
    !use read_only_verify_psy_data_mod

    type(field_type), intent(in) :: my_field(3)
    type(read_only_verify_PSyDataType) :: psy_data_type

    call psy_data_type%PreDeclareVariable("abc", my_field)
    call psy_data_type%ProvideVariable("abc", my_field)
end subroutine test

end module read_only_verify_psy_data_mod

