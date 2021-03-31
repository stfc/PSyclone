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

!> This module implements a verification that read-only fields (in the
!! dl_esm_inf infrastructure) are not overwritten (due to memory overwrites etc).
!! It is based on the ReadOnlyBaseType (from which it inherits the handling
!! of the basic Fortran data types and 2d-arrays, as specified in the Makefile).
!! It adds the support for the dl_esm_inf-specific field type.

module read_only_verify_psy_data_mod

    use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                              real32, real64, &
                                              stderr => Error_Unit

    use read_only_base_mod, only : ReadOnlyBaseType, read_only_verify_PSyDataInit, &
                 read_only_verify_PSyDataShutdown, is_enabled, &
                 read_only_verify_PSyDataStart, read_only_verify_PSyDataStop

    implicit none

    !> This is the data type that stores a checksum for each read-only
    !! variable. A static instance of this type is created for each
    !! instrumented region with PSyclone.
    type, extends(ReadOnlyBaseType), public :: read_only_verify_PSyDataType

    contains

        ! The various procedures used from this class
        procedure :: DeclareFieldDouble
        procedure :: ProvideFieldDouble
        procedure :: Abort

        !> Add the `DeclareFieldDouble` subroutine to the generic
        !! PreDeclareVariable interface. Note that the other generic
        !! `declareXXX` subroutines are declared in the PsyData base class.
        generic, public :: PreDeclareVariable => DeclareFieldDouble

        !> Add the function to handle fields to the generic interface.
        !! The ReadOnly base class provides additional methods for
        !! the generic interface like ProvideArray2dInt, ...
        generic, public :: ProvideVariable => ProvideFieldDouble

    end type read_only_verify_PSyDataType

contains

    ! -------------------------------------------------------------------------
    !> Displays the message and aborts execution. Use dl_esm_inf'subroutine
    !! `abort` function.
    !! @param[in] message Error message to be displayed (string).
    subroutine Abort(this, message)

        use gocean_mod, only : gocean_stop

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*) :: message

        call gocean_stop(message)

    end subroutine Abort

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field as defined in
    !! dl_esm_inf (r2d_field). It does nothing for the read-only verification.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    subroutine DeclareFieldDouble(this, name, value)

        use field_mod, only : r2d_field

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value

        this%next_var_index = this%next_var_index + 1

    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) of a dl_esm_field (r2d_field)
    !! @param[in,out] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideFieldDouble(this, name, value)

        use field_mod, only : r2d_field
        use read_onlY_base_mod, only : ComputeChecksum

        implicit none

        class(read_only_verify_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer(kind=int64) :: cksum

        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        cksum = ComputeChecksum(value%data)
        if(this%verify_checksums) then
            if(this%checksums(this%next_var_index) /= cksum) then
                write(stderr, *) "------------------- PSyData -------------------"
                write(stderr, *) "Double precision field ", name, " has been modified in ", &
                    trim(this%module_name)," : ", trim(this%region_name)
                write(stderr, *) "Original checksum: ", this%checksums(this%next_var_index)
                write(stderr, *) "New checksum:      ", cksum
                write(stderr, *) "------------------- PSyData -------------------"
            else if(this%verbosity>1) then
                write(stderr, *) "PSyData: checked variable ", trim(name)
            endif
        else
            this%checksums(this%next_var_index) = cksum
        endif

        this%next_var_index = this%next_var_index + 1

    end subroutine ProvideFieldDouble

end module read_only_verify_psy_data_mod
