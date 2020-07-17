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
    use, intrinsic :: iso_fortran_env, only : int64, int32, &
                                              stderr=>Error_Unit
    use field_mod, only : field_type
    use psy_data_base_mod, only : PSyDataBaseType

    implicit none

    !> This is the data type that stores a checksum for each read-only
    !! variable. A static instance of this type is created for each
    !! instrumented region with PSyclone.

    type, extends(PSyDataBaseType), public:: read_only_verify_PSyDataType
        !> This field stores a 64-bit integer checksum for each
        !! variable.
        integer(kind=int64), dimension(:), allocatable :: checksums

        !> This boolean flag switches from 'computing and storing checksum'
        !! to 'verify checksum'.
        logical :: verify_checksums

    contains
        ! The various procedures used
        procedure :: ChecksumScalarInt
        procedure :: ChecksumArray1dInt
        procedure :: ChecksumArray2dInt
        procedure :: ChecksumArray3dInt
        procedure :: ChecksumScalarReal
        procedure :: ChecksumArray1dDouble
        procedure :: ChecksumArray3dDouble
        procedure :: ChecksumArray4dDouble
        procedure :: ChecksumScalarDouble
        procedure :: DeclareFieldDouble,  ChecksumFieldDouble
        procedure :: DeclareFieldVectorDouble,  ChecksumFieldVectorDouble
        procedure :: PreStart, PreEndDeclaration
        procedure :: PostStart

        !> The generic interface for declaring a variable
        !! (these functions are all empty since this is not
        !! not required for read-only verification).
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
        !! which in case of the read-only verification either computes
        !! the checksum (before a kernel), or compares a checksum (after
        !! a kernel call).
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
    !!            checksum'ed before the instrumented region.
    !! @param[in] num_post_vars The number of variables that are also declared
    !!            before an instrumented region of code, but are checksum'ed
    !!            after this region.
    subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                        num_post_vars)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: module_name, region_name
        integer, intent(in)      :: num_pre_vars, num_post_vars

        character(1) :: verbose
        integer :: status
        call this%PSyDataBaseType%PreStart(module_name, region_name, &
                                           num_pre_vars, num_post_vars)
        if (num_pre_vars /= num_post_vars) then
            write(stderr, *) "PSYDATA: The same number of variables must be provided before"
            write(stderr, *) "and after the instrumented region. But the values are:"
            write(stderr, *) "Before: ", num_pre_vars, " after: ", num_post_vars
            stop
        endif

        this%verify_checksums = .false.
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called once all variables are declared. It makes
    !! sure that the next variable index is starting at 1 again.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine PreEndDeclaration(this)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        integer :: err

        ! During the declaration the number of checksums to be
        ! stored was counted in next_var_index, so allocate the array
        ! now (if it has not been allocated already in a previous call):
        if (.not. allocated(this%checksums)) then
            allocate(this%checksums(this%next_var_index-1), stat=err)
            if(err/=0) then
                write(stderr, *) "PSyData: Could not allocate ", &
                                 this%next_var_index-1,          &
                                 " integers, aborting."
                stop
            endif
        endif
        call this%PSyDataBaseType%PreEndDeclaration()
    end subroutine PreEndDeclaration

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

        call this%PSyDataBaseType%PostStart()
        this%verify_checksums = .true.
    end subroutine PostStart

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "Integer variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                ! In case of integer variables, we can use the checksum as the
                ! original value:
                write(stderr,*) "Original value: ", this%checksums(this%next_var_index)
                write(stderr,*) "New value:      ", value
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr,*) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = value
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumScalarInt

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "Integer array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", checksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumArray1dInt

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "2d Integer array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr,*) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr,*) "New checksum:      ", checksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr,*) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 3d-array of integer
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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "3d Integer array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr,*) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr,*) "New checksum:      ", checksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr,*) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumArray3dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a scalar single precision value.
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

        ! Type-cast from real to 32-bit int (same size, so
        ! no undefined bits in result)
        chksum32 = transfer(value, chksum32)
        ! Now assign to 64 bit, so we have a 64 bit checksum
        ! without potentially undefined bits.
        chksum64 = chksum32

        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= chksum64) then
                ! Convert back to 32 bit, and then cast to real:
                chksum32 = this%checksums(this%next_var_index)
                orig_value = transfer(chksum32, orig_value)
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "Real variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original value: ", orig_value
                write(stderr, *) "New value:      ", value
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = chksum64
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumScalarReal

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "1d double array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", checksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumArray1dDouble

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "3d double array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", checksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumArray3dDouble

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "4d double array ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", checksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumArray4dDouble

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
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "Double precision variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original value: ", orig_value
                write(stderr, *) "New value:      ", value
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field based on the LFRIc
    !! infrastructure type field_type.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine DeclareFieldDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
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
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= cksum) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "Double precision field ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", cksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif
        this%next_var_index = this%next_var_index + 1
    end subroutine ChecksumFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine declares a vector of double precision fields based on
    !! the LFRIc infrastructure type field_type.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    subroutine DeclareFieldVectorDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + size(value, 1)
    end subroutine DeclareFieldVectorDouble

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksums for a vector of fields based on
    !! the LFRic infrastructure type field_type. It uses ChecksumFieldDouble
    !! to handle each individual field of the vector (i.e. storing or 
    !! comparing the checksum). This way the read-only verification is done
    !! separately for each field member of the vector.
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
        ! Enough for a 6 digit number plus '()'
        character(8) :: index_string

        ! Provide each member of the vector as a normal field. This way
        ! the checksum will be computed for each member individually.
        do i=1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(index_string, '("(",i0,")")') i
            call this%ChecksumFieldDouble(name//trim(index_string), value(i))
            this%next_var_index = this%next_var_index + 1
        enddo
    end subroutine ChecksumFieldVectorDouble

    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 1d double
    !! precision Fortran array.
    function ComputeChecksum1d(field) result(checksum)
        implicit none
        integer(kind=int64) :: checksum
        double precision, dimension(:) :: field
        integer :: i
        checksum = 0
        do i=1, size(field, 1)
            checksum = checksum + transfer(field(i), checksum)
        enddo
    end function ComputeChecksum1d

    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 3d double
    !! precision Fortran array.
    function ComputeChecksum3d(field) result(checksum)
        implicit none
        integer(kind=int64) :: checksum
        double precision, dimension(:,:,:) :: field
        integer :: i, j, k
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
        checksum = 0
        do l=1, size(field, 4)
            do k=1, size(field, 3)
                do j=1, size(field, 2)
                    do i=1, size(field, 1)
                        checksum = checksum + transfer(field(i,j,k,l), checksum)
                    enddo
                enddo
            enddo
        enddo
    end function ComputeChecksum4d

    ! -------------------------------------------------------------------------
    
end module read_only_verify_psy_data_mod

