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

!> This module implements a PSyData-based verification that floating point
!! input and output parameters are not NAN and not infinite for the
!! dl_esm_inf library. It is based on the NANTestBaseType (from which it
!! inherits the handling of the basic Fortran data types and 2d-arrays,
!! as specified in the Makefile). It adds the support for the
!! dl_esm_inf-specific field type.

module nan_test_psy_data_mod
    use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                              real32, real64, &
                                              stderr=>Error_Unit

    use nan_test_base_mod, only : NANTestBaseType, nan_test_PSyDataInit, &
                 nan_test_PSyDataShutdown, is_enabled, &
                 nan_test_PSyDataStart, nan_test_PSyDataStop
    implicit none

    type, extends(NANTestBaseType), public:: nan_test_PSyDataType

    contains
        ! The various procedures used from this class
        procedure :: DeclareFieldDouble, ProvideFieldDouble

        !> The generic interface for declaring a variable. The 'Declare'
        !! functions are actually not used at all, but they must be provided
        !! as empty functions (the base class creates other functions, e.g.
        !! DeclareArray2dInt, but since they are not used by
        !! GOcean, there is no need to add them here).
        generic, public :: PreDeclareVariable => DeclareScalarInt,    &
                                                 DeclareScalarReal,   &
                                                 DeclareScalarDouble, &
                                                 DeclareArray2dDouble,&
                                                 DeclareFieldDouble

        !> The generic interface for providing the value of variables,
        !! which in this case verifies that all floating point values
        !! are not NAN and not infinite.
        generic, public :: ProvideVariable => ProvideScalarInt,    &
                                              ProvideScalarReal,   &
                                              ProvideScalarDouble, &
                                              ProvideArray2dDouble,&
                                              ProvideFieldDouble

    end type nan_test_PSyDataType

Contains

    ! -------------------------------------------------------------------------
    !> This subroutine declares a double precision field as defined in
    !! dl_esm_inf (r2d_field). It does nothing in the NAN checking library.
    !! @param[inout] this The instance of the nan_test_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[inout] this The instance of the nan_test_PSyDataType.
    subroutine DeclareFieldDouble(this, name, value)
        use field_mod, only : r2d_field
        implicit none
        class(nan_test_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    !> This subroutine either computes a checksum or compares a checksum
    !! (depending on this%verify_checksums) of a dl_esm_field (r2d_field)
    !! @param[inout] this The instance of the nan_test_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideFieldDouble(this, name, value)
        use field_mod, only : r2d_field
        implicit none
        class(nan_test_PSyDataType), intent(inout), target :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer(kind=int64):: cksum

        call this%ProvideArray2dDouble(name, value%data)
    end subroutine ProvideFieldDouble
    
end module nan_test_psy_data_mod
