! ==================================================
! THIS FILE IS CREATED FROM THE JINJA TEMPLATE FILE 
! DO NOT MODIFY DIRECTLY                            
! ==================================================




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
        procedure :: DeclareFieldDouble,  ProvideFieldDouble
        procedure :: DeclareFieldVectorDouble,  ProvideFieldVectorDouble
        procedure :: PreStart, PreEndDeclaration
        procedure :: PostStart

        procedure :: ProvideScalarDouble
        procedure :: ProvideArray1dDouble
        procedure :: ProvideArray2dDouble
        procedure :: ProvideArray3dDouble
        procedure :: ProvideArray4dDouble
        procedure :: ProvideScalarReal
        procedure :: ProvideArray1dReal
        procedure :: ProvideArray2dReal
        procedure :: ProvideArray3dReal
        procedure :: ProvideArray4dReal
        procedure :: ProvideScalarInt
        procedure :: ProvideArray1dInt
        procedure :: ProvideArray2dInt
        procedure :: ProvideArray3dInt
        procedure :: ProvideArray4dInt

                                                                                                                                                                              
        ! Declare generic interface for PreDeclareVariable:
        generic, public :: PreDeclareVariable => &
            DeclareFieldDouble, &
            DeclareFieldVectorDouble, &
            DeclareScalarDouble, &
            DeclareArray1dDouble, &
            DeclareArray2dDouble, &
            DeclareArray3dDouble, &
            DeclareArray4dDouble, &
            DeclareScalarReal, &
            DeclareArray1dReal, &
            DeclareArray2dReal, &
            DeclareArray3dReal, &
            DeclareArray4dReal, &
            DeclareScalarInt, &
            DeclareArray1dInt, &
            DeclareArray2dInt, &
            DeclareArray3dInt, &
            DeclareArray4dInt

        !> The generic interface for providing the value of variables,
        !! which in case of the read-only verification either computes
        !! the checksum (before a kernel), or compares a checksum (after
        !! a kernel call).
        generic, public :: ProvideVariable => &
            ProvideFieldDouble,       &
            ProvideFieldVectorDouble, &
            ProvideScalarDouble, &
            ProvideArray1dDouble, &
            ProvideArray2dDouble, &
            ProvideArray3dDouble, &
            ProvideArray4dDouble, &
            ProvideScalarReal, &
            ProvideArray1dReal, &
            ProvideArray2dReal, &
            ProvideArray3dReal, &
            ProvideArray4dReal, &
            ProvideScalarInt, &
            ProvideArray1dInt, &
            ProvideArray2dInt, &
            ProvideArray3dInt, &
            ProvideArray4dInt
                                              
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
    subroutine ProvideFieldDouble(this, name, value)
        use field_mod, only : field_type, field_proxy_type
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), intent(in) :: value
        type(field_proxy_type) :: value_proxy
        integer(kind=int64):: cksum

        value_proxy = value%get_proxy()
        cksum = ComputeChecksum1dDouble(value_proxy%data)
        ! We could call ProvideArray1DDouble here, but would get a
        ! confusing error message
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
    end subroutine ProvideFieldDouble

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
    !! the LFRic infrastructure type field_type. It uses ProvideFieldDouble
    !! to handle each individual field of the vector (i.e. storing or 
    !! comparing the checksum). This way the read-only verification is done
    !! separately for each field member of the vector.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The vector of fields.
    subroutine ProvideFieldVectorDouble(this, name, value)
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
            call this%ProvideFieldDouble(name//trim(index_string), value(i))
            this%next_var_index = this%next_var_index + 1
        enddo
    end subroutine ProvideFieldVectorDouble

    ! =========================================================================
    ! Jinja created code.
    ! =========================================================================



    ! =========================================================================
    ! Implementation for all double precision types
    ! =========================================================================
    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a single Double.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideScalarDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, intent(in)     :: value

        double precision            :: orig_value
        integer(kind=int64) :: checksum, int_64


        checksum = transfer(value, int_64)

        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "double precision variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                ! We can recreate the original value which is stored as
                ! 64-bit integer as the checksum:
                orig_value = transfer(this%checksums(this%next_var_index), orig_value)
                write(stderr,*) "Original value: ", orig_value
                write(stderr,*) "New value:      ", value
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr,*) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        call this%PSyDataBaseType%ProvideScalarDouble(name, value)
    end subroutine ProvideScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 1d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray1dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum1dDouble(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "1d Double array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray1dDouble(name, value)
    end subroutine ProvideArray1dDouble


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 1d
    !! double precision Fortran array.
    function ComputeChecksum1dDouble(field) result(checksum)
        implicit none
        double precision, dimension(:) :: field

        integer :: i1
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i1=1, size(field, 1)
           int_64 = transfer(field(i1), checksum)
           checksum = checksum + int_64
        enddo
    end function ComputeChecksum1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 2d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray2dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum2dDouble(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "2d Double array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray2dDouble(name, value)
    end subroutine ProvideArray2dDouble


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 2d
    !! double precision Fortran array.
    function ComputeChecksum2dDouble(field) result(checksum)
        implicit none
        double precision, dimension(:,:) :: field

        integer :: i1,i2
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i2=1, size(field, 2)
           do i1=1, size(field, 1)
              int_64 = transfer(field(i1,i2), checksum)
              checksum = checksum + int_64
           enddo
        enddo
    end function ComputeChecksum2dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 3d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray3dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum3dDouble(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "3d Double array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray3dDouble(name, value)
    end subroutine ProvideArray3dDouble


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 3d
    !! double precision Fortran array.
    function ComputeChecksum3dDouble(field) result(checksum)
        implicit none
        double precision, dimension(:,:,:) :: field

        integer :: i1,i2,i3
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i3=1, size(field, 3)
           do i2=1, size(field, 2)
              do i1=1, size(field, 1)
                 int_64 = transfer(field(i1,i2,i3), checksum)
                 checksum = checksum + int_64
              enddo
           enddo
        enddo
    end function ComputeChecksum3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 4d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray4dDouble(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum4dDouble(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "4d Double array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray4dDouble(name, value)
    end subroutine ProvideArray4dDouble


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 4d
    !! double precision Fortran array.
    function ComputeChecksum4dDouble(field) result(checksum)
        implicit none
        double precision, dimension(:,:,:,:) :: field

        integer :: i1,i2,i3,i4
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i4=1, size(field, 4)
           do i3=1, size(field, 3)
              do i2=1, size(field, 2)
                 do i1=1, size(field, 1)
                    int_64 = transfer(field(i1,i2,i3,i4), checksum)
                    checksum = checksum + int_64
                 enddo
              enddo
           enddo
        enddo
    end function ComputeChecksum4dDouble

   
    ! =========================================================================
    ! Implementation for all real types
    ! =========================================================================
    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a single Real.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideScalarReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, intent(in)     :: value

        real            :: orig_value
        integer(kind=int64) :: checksum, int_64

        integer(kind=int32) :: int_32

        ! `transfer` leaves undefined bits in a 64-bit value
        ! so assign to 32-bit, then assign to 64-bit to have all bits defined
        int_32 = transfer(value, int_32)
        checksum = int_32

        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "real variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                ! We can recreate the original value which is stored as
                ! 64-bit integer as the checksum:
                int_32 = this%checksums(this%next_var_index)
                orig_value = transfer(int_32, orig_value)
                write(stderr,*) "Original value: ", orig_value
                write(stderr,*) "New value:      ", value
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr,*) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        call this%PSyDataBaseType%ProvideScalarReal(name, value)
    end subroutine ProvideScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 1d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray1dReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, dimension(:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum1dReal(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "1d Real array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray1dReal(name, value)
    end subroutine ProvideArray1dReal


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 1d
    !! real Fortran array.
    function ComputeChecksum1dReal(field) result(checksum)
        implicit none
        real, dimension(:) :: field

        integer :: i1
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i1=1, size(field, 1)
           ! transfer leaves undefined bits in a 64-bit target
           ! so we transfer to 32-bits and then assign to 64-bit
           int_32 = transfer(field(i1), int_32)
           int_64 = int_32
           checksum = checksum + int_64
        enddo
    end function ComputeChecksum1dReal

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 2d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray2dReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, dimension(:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum2dReal(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "2d Real array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray2dReal(name, value)
    end subroutine ProvideArray2dReal


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 2d
    !! real Fortran array.
    function ComputeChecksum2dReal(field) result(checksum)
        implicit none
        real, dimension(:,:) :: field

        integer :: i1,i2
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i2=1, size(field, 2)
           do i1=1, size(field, 1)
              ! transfer leaves undefined bits in a 64-bit target
              ! so we transfer to 32-bits and then assign to 64-bit
              int_32 = transfer(field(i1,i2), int_32)
              int_64 = int_32
              checksum = checksum + int_64
           enddo
        enddo
    end function ComputeChecksum2dReal

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 3d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray3dReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, dimension(:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum3dReal(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "3d Real array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray3dReal(name, value)
    end subroutine ProvideArray3dReal


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 3d
    !! real Fortran array.
    function ComputeChecksum3dReal(field) result(checksum)
        implicit none
        real, dimension(:,:,:) :: field

        integer :: i1,i2,i3
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i3=1, size(field, 3)
           do i2=1, size(field, 2)
              do i1=1, size(field, 1)
                 ! transfer leaves undefined bits in a 64-bit target
                 ! so we transfer to 32-bits and then assign to 64-bit
                 int_32 = transfer(field(i1,i2,i3), int_32)
                 int_64 = int_32
                 checksum = checksum + int_64
              enddo
           enddo
        enddo
    end function ComputeChecksum3dReal

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 4d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray4dReal(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        real, dimension(:,:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum4dReal(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "4d Real array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray4dReal(name, value)
    end subroutine ProvideArray4dReal


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 4d
    !! real Fortran array.
    function ComputeChecksum4dReal(field) result(checksum)
        implicit none
        real, dimension(:,:,:,:) :: field

        integer :: i1,i2,i3,i4
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i4=1, size(field, 4)
           do i3=1, size(field, 3)
              do i2=1, size(field, 2)
                 do i1=1, size(field, 1)
                    ! transfer leaves undefined bits in a 64-bit target
                    ! so we transfer to 32-bits and then assign to 64-bit
                    int_32 = transfer(field(i1,i2,i3,i4), int_32)
                    int_64 = int_32
                    checksum = checksum + int_64
                 enddo
              enddo
           enddo
        enddo
    end function ComputeChecksum4dReal

   
    ! =========================================================================
    ! Implementation for all integer(kind=int32) types
    ! =========================================================================
    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a single Int.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideScalarInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), intent(in)     :: value

        integer(kind=int32)            :: orig_value
        integer(kind=int64) :: checksum, int_64

        integer(kind=int32) :: int_32

        ! `transfer` leaves undefined bits in a 64-bit value
        ! so assign to 32-bit, then assign to 64-bit to have all bits defined
        int_32 = transfer(value, int_32)
        checksum = int_32

        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "integer(kind=int32) variable ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                ! We can recreate the original value which is stored as
                ! 64-bit integer as the checksum:
                int_32 = this%checksums(this%next_var_index)
                orig_value = transfer(int_32, orig_value)
                write(stderr,*) "Original value: ", orig_value
                write(stderr,*) "New value:      ", value
                write(stderr,*) "------------- PSyData -------------------------"
            else if(this%verbosity>1) then
                write(stderr,*) "PSYDATA: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = checksum
        endif
        call this%PSyDataBaseType%ProvideScalarInt(name, value)
    end subroutine ProvideScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 1d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray1dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum1dInt(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "1d Int array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray1dInt(name, value)
    end subroutine ProvideArray1dInt


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 1d
    !! integer(kind=int32) Fortran array.
    function ComputeChecksum1dInt(field) result(checksum)
        implicit none
        integer(kind=int32), dimension(:) :: field

        integer :: i1
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i1=1, size(field, 1)
           ! transfer leaves undefined bits in a 64-bit target
           ! so we transfer to 32-bits and then assign to 64-bit
           int_32 = transfer(field(i1), int_32)
           int_64 = int_32
           checksum = checksum + int_64
        enddo
    end function ComputeChecksum1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 2d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray2dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum2dInt(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "2d Int array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray2dInt(name, value)
    end subroutine ProvideArray2dInt


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 2d
    !! integer(kind=int32) Fortran array.
    function ComputeChecksum2dInt(field) result(checksum)
        implicit none
        integer(kind=int32), dimension(:,:) :: field

        integer :: i1,i2
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i2=1, size(field, 2)
           do i1=1, size(field, 1)
              ! transfer leaves undefined bits in a 64-bit target
              ! so we transfer to 32-bits and then assign to 64-bit
              int_32 = transfer(field(i1,i2), int_32)
              int_64 = int_32
              checksum = checksum + int_64
           enddo
        enddo
    end function ComputeChecksum2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 3d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray3dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum3dInt(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "3d Int array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray3dInt(name, value)
    end subroutine ProvideArray3dInt


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 3d
    !! integer(kind=int32) Fortran array.
    function ComputeChecksum3dInt(field) result(checksum)
        implicit none
        integer(kind=int32), dimension(:,:,:) :: field

        integer :: i1,i2,i3
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i3=1, size(field, 3)
           do i2=1, size(field, 2)
              do i1=1, size(field, 1)
                 ! transfer leaves undefined bits in a 64-bit target
                 ! so we transfer to 32-bits and then assign to 64-bit
                 int_32 = transfer(field(i1,i2,i3), int_32)
                 int_64 = int_32
                 checksum = checksum + int_64
              enddo
           enddo
        enddo
    end function ComputeChecksum3dInt

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) for a 4d-array of integer
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray4dInt(this, name, value)
        implicit none
        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:,:,:), intent(in) :: value
        integer(kind=int64) :: checksum
        integer :: i, j

        checksum = ComputeChecksum4dInt(value)
        if (this%verify_checksums) then
            if (checksum /= this%checksums(this%next_var_index)) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr,*) "4d Int array ", name, " has been modified in ", &
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
        call this%PSyDataBaseType%ProvideArray4dInt(name, value)
    end subroutine ProvideArray4dInt


    ! -------------------------------------------------------------------------
    !> This function computes a 64-bit integer checksum for a 4d
    !! integer(kind=int32) Fortran array.
    function ComputeChecksum4dInt(field) result(checksum)
        implicit none
        integer(kind=int32), dimension(:,:,:,:) :: field

        integer :: i1,i2,i3,i4
        integer(kind=int32) :: int_32
        integer(kind=int64) :: checksum, int_64

        checksum = 0
        do i4=1, size(field, 4)
           do i3=1, size(field, 3)
              do i2=1, size(field, 2)
                 do i1=1, size(field, 1)
                    ! transfer leaves undefined bits in a 64-bit target
                    ! so we transfer to 32-bits and then assign to 64-bit
                    int_32 = transfer(field(i1,i2,i3,i4), int_32)
                    int_64 = int_32
                    checksum = checksum + int_64
                 enddo
              enddo
           enddo
        enddo
    end function ComputeChecksum4dInt

      
    ! -------------------------------------------------------------------------
    
end module read_only_verify_psy_data_mod

