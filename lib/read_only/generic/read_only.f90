! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> This module implements a verification that read-only variables in generic
!! Fortran code are indeed not modified (e.g. because of memory overwrite)
!! This very slim module is implemented since PSyclone expects
!! certain module and type names, which the base class ReadOnlyBaseType
!! does not provide.

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
    !! instrumented region with PSyclone. It is empty, this class is
    !! only here to get the right name for PSyclone in case of generic
    !! transformation usage.
    type, extends(ReadOnlyBaseType), public :: read_only_verify_PSyDataType

    end type read_only_verify_PSyDataType

end module read_only_verify_psy_data_mod
