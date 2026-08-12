! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> This module implements a PSyData-based verification that checks if
!! variable values are within a certain range. It is based on the
!! ValueRangeCheckBaseType (from which it inherits the handling of the
!! basic Fortran data types and 2d-arrays, as specified in the Makefile),
!! and just extends it with a dummy implementation to provide the same
!! module name as the domain-specific implementations.

module value_range_check_psy_data_mod

    use value_range_check_base_mod, only : ValueRangeCheckBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a binary file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(ValueRangeCheckBaseType), public :: value_range_check_psydatatype

    end type value_range_check_psydatatype

end module value_range_check_psy_data_mod
