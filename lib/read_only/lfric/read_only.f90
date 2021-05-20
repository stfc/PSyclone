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

!> This module implements a verification that read-only fields are
!! not overwritten (due to memory overwrites etc)
!!

module read_only_verify_psy_data_mod

    use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                              real32, real64, &
                                              stderr => Error_Unit
    use field_mod, only : field_type
    use integer_field_mod, only : integer_field_type
    use read_only_base_mod, only : ReadOnlyBaseType, is_enabled

    implicit none

    !> This is the data type that stores a checksum for each read-only
    !! variable. A static instance of this type is created for each
    !! instrumented region with PSyclone.

    type, extends(ReadOnlyBaseType), public :: read_only_verify_PSyDataType

    contains

        ! The LFRic-specific procedures defined here
        procedure :: DeclareField
        procedure :: ProvideField
        procedure :: DeclareFieldVector
        procedure :: ProvideFieldVector
        procedure :: DeclareIntField
        procedure :: ProvideIntField
        procedure :: DeclareIntFieldVector
        procedure :: ProvideIntFieldVector

        ! Declare generic interface for PreDeclareVariable:
        generic, public :: PreDeclareVariable => &
                           DeclareField,         &
                           DeclareFieldVector,   &
                           DeclareIntField,      &
                           DeclareIntFieldVector

        !> The generic interface for providing the value of variables,
        !! which in case of the read-only verification either computes
        !! the checksum (before a kernel), or compares a checksum (after
        !! a kernel call).
        generic, public :: ProvideVariable =>  &
                           ProvideField,       &
                           ProvideFieldVector, &
                           ProvideIntField,    &
                           ProvideIntFieldVector

    end type read_only_verify_PSyDataType

contains

    ! -------------------------------------------------------------------------
    !> This subroutine declares a real-valued (double precision) field based on
    !! the LFRic infrastructure type field_type.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareField(this, name, value)

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), intent(in) :: value

        this%next_var_index = this%next_var_index + 1

    end subroutine DeclareField

    ! -------------------------------------------------------------------------
    !> This subroutine declares an integer field based on the LFRic
    !! infrastructure type integer_field_type.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareIntField(this, name, value)

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(integer_field_type), intent(in) :: value

        this%next_var_index = this%next_var_index + 1

    end subroutine DeclareIntField

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksum for a real-valued field based on
    !! the LFRic infrastructure type field_type. Depending on state
    !! (this%verify_checksum) it either stores the checksum, or compares it
    !! with a previously computed checksum.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideField(this, name, value)

        use field_mod, only : field_type, field_proxy_type
        use read_only_base_mod, only : ComputeChecksum

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), intent(in) :: value
        type(field_proxy_type) :: value_proxy
        integer(kind=int64):: cksum

        if (.not. is_enabled) return

        value_proxy = value%get_proxy()
        cksum = ComputeChecksum(value_proxy%data)
        ! We could call ProvideArray1DDouble here, but would get a
        ! confusing error message
        if (this%verify_checksums) then
            if (this%checksums(this%next_var_index) /= cksum) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "Double precision field ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", cksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if (this%verbosity>1) then
                write(stderr, *) "PSyData: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif

        this%next_var_index = this%next_var_index + 1

    end subroutine ProvideField

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksum for an integer-valued field based
    !! on the LFRic infrastructure type integer_field_type. Depending on state
    !! (this%verify_checksum) it either stores the checksum, or compares it
    !! with a previously computed checksum.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideIntField(this, name, value)

        use integer_field_mod,  only : integer_field_type, &
                                       integer_field_proxy_type
        use read_only_base_mod, only : ComputeChecksum

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(integer_field_type), intent(in) :: value
        type(integer_field_proxy_type) :: value_proxy
        integer(kind=int64):: cksum

        if (.not. is_enabled) return

        value_proxy = value%get_proxy()
        cksum = ComputeChecksum(value_proxy%data)
        ! We could call ProvideArray1DInt here, but would get a
        ! confusing error message
        if (this%verify_checksums) then
            if (this%checksums(this%next_var_index) /= cksum) then
                write(stderr,*) "------------- PSyData -------------------------"
                write(stderr, *) "Integer-valued field ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", cksum
                write(stderr,*) "------------- PSyData -------------------------"
            else if (this%verbosity>1) then
                write(stderr, *) "PSyData: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif

        this%next_var_index = this%next_var_index + 1

    end subroutine ProvideIntField

    ! -------------------------------------------------------------------------
    !> This subroutine declares a vector of real-valued (double precision)
    !! fields based on the LFRic infrastructure type field_type.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldVector(this, name, value)

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), dimension(:), intent(in) :: value

        this%next_var_index = this%next_var_index + size(value, 1)

    end subroutine DeclareFieldVector

    ! -------------------------------------------------------------------------
    !> This subroutine declares a vector of integer fields based on
    !! the LFRic infrastructure type integer_field_type.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareIntFieldVector(this, name, value)

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(integer_field_type), dimension(:), intent(in) :: value

        this%next_var_index = this%next_var_index + size(value, 1)

    end subroutine DeclareIntFieldVector

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksums for a vector of real-valued
    !! fields based on the LFRic infrastructure type field_type. It uses
    !! ProvideField to handle each individual field of the vector (i.e.
    !! storing or comparing the checksum). This way the read-only verification
    !! is done separately for each field member of the vector.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The vector of fields.
    subroutine ProvideFieldVector(this, name, value)

        use field_mod, only : field_type, field_proxy_type

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(field_type), dimension(:), intent(in) :: value
        type(field_proxy_type) :: value_proxy
        integer(kind=int64) :: cksum
        integer :: i
        ! Enough for a 6 digit number plus '()'
        character(8) :: index_string

        if (.not. is_enabled) return

        ! Provide each member of the vector as a normal field. This way
        ! the checksum will be computed for each member individually.
        do i = 1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(index_string, '("(",i0,")")') i
            call this%ProvideField(name//trim(index_string), value(i))
        enddo

    end subroutine ProvideFieldVector

    ! -------------------------------------------------------------------------
    !> This subroutine computes the checksums for a vector of integer-valued
    !! fields based on the LFRic infrastructure type integer_field_type. It
    !! uses ProvideField to handle each individual field of the vector
    !! (i.e. storing or comparing the checksum). This way the read-only
    !! verification is done separately for each field member of the vector.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The vector of fields.
    subroutine ProvideIntFieldVector(this, name, value)

        use integer_field_mod, only : integer_field_type, &
                                      integer_field_proxy_type

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(integer_field_type), dimension(:), intent(in) :: value
        type(integer_field_proxy_type) :: value_proxy
        integer(kind=int64) :: cksum
        integer :: i
        ! Enough for a 6 digit number plus '()'
        character(8) :: index_string

        if (.not. is_enabled) return

        ! Provide each member of the vector as a normal field. This way
        ! the checksum will be computed for each member individually.
        do i = 1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(index_string, '("(",i0,")")') i
            call this%ProvideIntField(name//trim(index_string), value(i))
        enddo

    end subroutine ProvideIntFieldVector

    ! -------------------------------------------------------------------------

end module read_only_verify_psy_data_mod
