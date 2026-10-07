! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> This module implements a simple NetCDF writer using the PSyData
!! interface. It is for handling generic Fortran code, i.e. it does 
!! not actually provide any API-specific types like fields, only standard
!! Fortran data types. Therefore, it is just an empty wrapper around
!! the ExtractNetcdfBaseType, which provides the standard module name
!! and class name expected by the extraction scripts.
!!

module extract_psy_data_mod

    use extract_netcdf_base_mod, only : ExtractNetcdfBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(ExtractNetcdfBaseType), public :: extract_PsyDataType

    end type extract_PSyDataType

end module extract_psy_data_mod
