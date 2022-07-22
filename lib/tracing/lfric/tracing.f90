! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2022, Science and Technology Facilities Council.
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

!> This module implements a simple tracing library that will
!! trace read- and write-accesses to fields in LFRic. This can
!! be used to trace where invalid values in a field come from.
!!

module tracing_psy_data_mod

    use, intrinsic :: iso_fortran_env, only : int64, int32
    use tracing_base_mod,       only : TracingBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(TracingBaseType), public :: tracing_PsyDataType

    contains

        ! The various procedures defined here
        procedure :: DeclareField
        procedure :: ProvideField
        procedure :: DeclareFieldVector
        procedure :: ProvideFieldVector
        procedure :: DeclareIntField
        procedure :: ProvideIntField
        procedure :: DeclareIntFieldVector
        procedure :: ProvideIntFieldVector

        !> Declare generic interface for PreDeclareVariable:
        generic, public :: PreDeclareVariable => &
                           DeclareField,         &
                           DeclareFieldVector,   &
                           DeclareIntField,      &
                           DeclareIntFieldVector

        !> The generic interface for providing the value of variables,
        !! which in case of the kernel extraction writes the data to
        !! the NetCDF file.
        generic, public :: ProvideVariable => &
                           ProvideField,        &
                           ProvideFieldVector,  &
                           ProvideIntField,     &
                           ProvideIntFieldVector

    end type tracing_PSyDataType

contains

    ! -------------------------------------------------------------------------
    !> This subroutine declares an LFRic field with real-valued data.
    !! It calls the PreDeclareVariable function provided by the base class
    !! (depending on the type of the argument, e.g. it might call
    !! DeclareArray1dDouble).
    !! @param[in,out] this The instance of the tracing_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareField(this, name, value)

        use field_mod, only : field_type, field_proxy_type

        implicit none

        class(tracing_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), intent(in)                      :: value

        type(field_proxy_type) :: value_proxy

        value_proxy = value%get_proxy()
        call this%PreDeclareVariable(name, value_proxy%data)

    end subroutine DeclareField

    ! -------------------------------------------------------------------------
    !> This subroutine declares an LFRic field with integer-valued data.
    !! It calls the PreDeclareVariable function provided by the base class
    !! (depending on the type of the argument, e.g. it might call
    !! DeclareArray1dInt).
    !! @param[in,out] this The instance of the tracing_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareIntField(this, name, value)

        use integer_field_mod, only : integer_field_type, &
                                      integer_field_proxy_type

        implicit none

        class(tracing_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(integer_field_type), intent(in)              :: value

        type(integer_field_proxy_type) :: value_proxy

        value_proxy = value%get_proxy()
        call this%PreDeclareVariable(name, value_proxy%data)

    end subroutine DeclareIntField

    ! -------------------------------------------------------------------------
    !> This subroutine traces an LFRic real-valued field. It uses the
    !! corresponding function provided by the base class.
    !! @param[in,out] this The instance of the tracing_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideField(this, name, value)

        use field_mod, only : field_type, field_proxy_type

        implicit none

        class(tracing_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), intent(in)                      :: value

        type(field_proxy_type) :: value_proxy

        value_proxy = value%get_proxy()
        call this%ProvideVariable(name, value_proxy%data)

    end subroutine ProvideField

    ! -------------------------------------------------------------------------
    !> This subroutine traces an LFRic integer-valued field. It uses the
    !! corresponding function provided by the base class.
    !! @param[in,out] this The instance of the tracing_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideIntField(this, name, value)

        use integer_field_mod, only : integer_field_type, &
                                      integer_field_proxy_type

        implicit none

        class(tracing_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(integer_field_type), intent(in)              :: value

        type(integer_field_proxy_type) :: value_proxy

        value_proxy = value%get_proxy()
        call this%ProvideVariable(name, value_proxy%data)

    end subroutine ProvideIntField

    ! -------------------------------------------------------------------------
    !> This subroutine declares an LFric real-valued field vector. Each
    !! component of the vector is stored as a separate variable, using the
    !! corresponding array function of the base class.
    !! @param[in,out] this The instance of the tracing_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldVector(this, name, value)

        use field_mod, only : field_type, field_proxy_type

        implicit none

        class(tracing_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), dimension(:), intent(in)        :: value

        integer                :: i
        type(field_proxy_type) :: value_proxy
        character(9)           :: number

        ! Provide each component of the vector as an individual 1d array.
        ! The base class will re-allocate internal array sizes if required
        do i = 1, size(value)
            value_proxy = value(i)%get_proxy()
            ! We add a '%' here to avoid a potential name clash if
            ! the user should have a vector field 'a' (which is now stored
            ! as a%1, a%2, ...), and a field 'a1'
            write(number, '("%",i0)') i
            call this%PreDeclareVariable(name//trim(number), value_proxy%data)
        enddo

    end subroutine DeclareFieldVector

    ! -------------------------------------------------------------------------
    !> This subroutine declares an LFric integer-valued field vector. Each
    !! component of the vector is stored as a separate variable, using the
    !! corresponding array function of the base class.
    !! @param[in,out] this The instance of the tracing_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareIntFieldVector(this, name, value)

        use integer_field_mod, only : integer_field_type, &
                                      integer_field_proxy_type

        implicit none

        class(tracing_PSyDataType), intent(inout), target  :: this
        character(*), intent(in)                           :: name
        type(integer_field_type), dimension(:), intent(in) :: value

        integer                        :: i
        type(integer_field_proxy_type) :: value_proxy
        character(9)                   :: number

        ! Provide each component of the vector as an individual 1d array.
        ! The base class will re-allocate internal array sizes if required
        do i = 1, size(value)
            value_proxy = value(i)%get_proxy()
            ! We add a '%' here to avoid a potential name clash if
            ! the user should have a vector field 'a' (which is now stored
            ! as a%1, a%2, ...), and a field 'a1'
            write(number, '("%",i0)') i
            call this%PreDeclareVariable(name//trim(number), value_proxy%data)
        enddo

    end subroutine DeclareIntFieldVector

    ! -------------------------------------------------------------------------
    !> This subroutine writes an LFRic real-valued field vector to the NetCDF
    !! file. Each component is stored as an individual variable using the
    !! corresponding array function of the base class.
    !! @param[in,out] this The instance of the tracing_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideFieldVector(this, name, value)

        use field_mod, only : field_type, field_proxy_type

        implicit none

        class(tracing_PSyDataType), intent(inout), target :: this
        character(*), intent(in)                          :: name
        type(field_type), dimension(:), intent(in)        :: value

        integer                :: i
        type(field_proxy_type) :: value_proxy
        character(9)           :: number

        ! Provide each dimension of the vector as an individual 1d array.
        do i = 1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(number, '("%",i0)') i
            call this%ProvideVariable(name//trim(number), value_proxy%data)
        enddo

    end subroutine ProvideFieldVector

! -------------------------------------------------------------------------
    !> This subroutine writes an LFRic integer-valued field vector to the
    !! NetCDF file. Each component is stored as an individual variable
    !! using the corresponding array function of the base class.
    !! @param[in,out] this The instance of the tracing_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideIntFieldVector(this, name, value)

        use integer_field_mod, only : integer_field_type, &
                                      integer_field_proxy_type

        implicit none

        class(tracing_PSyDataType), intent(inout), target  :: this
        character(*), intent(in)                           :: name
        type(integer_field_type), dimension(:), intent(in) :: value

        integer                        :: i
        type(integer_field_proxy_type) :: value_proxy
        character(9)                   :: number

        ! Provide each dimension of the vector as an individual 1d array.
        do i = 1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(number, '("%",i0)') i
            call this%ProvideVariable(name//trim(number), value_proxy%data)
        enddo

    end subroutine ProvideIntFieldVector

end module tracing_psy_data_mod
