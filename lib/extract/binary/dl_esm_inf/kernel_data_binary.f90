! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> This module implements a simple binary-file writer using the PSyData
!! interface. It is specific to the dl_esm_inf library used with
!! the GOcean API.
!! A Fortran code instrumented with corresponding calls
!! to the PSyData API and linked in with this library will write
!! the requested input and output parameters to a Fortran binary file.
!!

module extract_psy_data_mod

    use extract_binary_base_mod, only : ExtractBinaryBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a binary file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(ExtractBinaryBaseType), public :: extract_PsyDataType

    contains

        ! The various procedures used
        procedure :: DeclareFieldDouble
        procedure :: WriteFieldDouble

        !> The generic interface for declaring a variable:
        generic, public :: PreDeclareVariable => DeclareFieldDouble

        !> The generic interface for providing the value of variables,
        !! which in case of the extract interface is written:
        generic, public :: ProvideVariable => WriteFieldDouble

    end type extract_PSyDataType

contains

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
    !! to the Fortran binary file.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldDouble(this, name, value)

        use field_mod, only : r2d_field

        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value

        ! Map to a simple 2d-array:
        call this%DeclareArray2dDouble(name, value%data)

    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of a dl_esm_field (r2d_field)
    !! to the Fortran binary file.
    !! @param[in,out] this The instance of the extract_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine WriteFieldDouble(this, name, value)

        use field_mod, only : r2d_field

        implicit none

        class(extract_PsyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value

        ! Map the field to a simple 2d-array
        call this%WriteArray2dDouble(name, value%data)

    end subroutine WriteFieldDouble

end module extract_psy_data_mod
