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

!> This module implements a simple in situ visualisation module using
!! Python's pyplot library. So the Fortran data is passed to a Python
!! script at runtime using a C-interface. 
!! 

module visualisation_psy_data_mod

    use, intrinsic :: iso_fortran_env, only : real64, &
                                              stderr=>Error_Unit
    use, intrinsic :: iso_c_binding, only: c_int, c_double

    use psy_data_base_mod, only: PSyDataBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(PSyDataBaseType), public:: visualisation_PsyDataType
 
    logical :: grid_defined = .False.

    contains
        ! The various procedures defined here
        procedure :: DeclareField,       ProvideField
        procedure :: DeclareFieldVector, ProvideFieldVector

        !> Declare generic interface for PreDeclareVariable:
        generic, public :: PreDeclareVariable => &
            DeclareField,       &
            DeclareFieldVector

        !> The generic interface for providing the value of variables,
        generic, public :: ProvideVariable => &
            ProvideField,       &
            ProvideFieldVector

    end type visualisation_PsyDataType

    ! Interface to C-wrapper that calls python
    interface
      function initialise_python() bind(C, name="initialise_python")
        ! Interface blocks don't know about their context,
        ! so we need to use iso_c_binding to get c_int definition
        import :: c_int, c_double
        ! Return value
        integer(c_int) :: initialise_python
      end function initialise_python
    end interface

    interface
      function set_grid(n_chi, chi1, chi2, chi3) bind(C, name="set_grid")
        ! Interface blocks don't know about their context,
        ! so we need to use iso_c_binding to get c_int definition
        import :: c_int, c_double
        ! Return value
        integer(c_int) :: set_grid
        integer(c_int) :: n_chi
        real(kind=c_double), dimension(*), intent(inout) :: chi1
        real(kind=c_double), dimension(*), intent(inout) :: chi2
        real(kind=c_double), dimension(*), intent(inout) :: chi3
      end function set_grid
    end interface

    interface
      function update_plot_data(n_field, field) bind(C, name="update_plot_data")
        ! Interface blocks don't know about their context,
        ! so we need to use iso_c_binding to get c_int definition
        import :: c_int, c_double
        ! Return value
        integer(c_int) :: update_plot_data
        integer(c_int) :: n_field
        real(kind=c_double), dimension(*), intent(inout) :: field
      end function update_plot_data
    end interface

Contains

    ! -------------------------------------------------------------------------
    !> This subroutine declares an LFRic field.
    !! It calls the PreDeclareVariable function provided by the base class
    !! (depending on the type of the argument, e.g. it might call
    !! DeclareArray1dDouble).
    !! @param[inout] this The instance of the visualisation_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareField(this, name, value)
        use field_mod, only : field_type, field_proxy_type
        implicit none

        class(visualisation_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        type(field_type), intent(in)                            :: value

        type(field_proxy_type) :: value_proxy
        integer(c_int) :: cc, result
        cc = 123

        value_proxy = value%get_proxy()
        call this%PreDeclareVariable(name, value_proxy%data)
    end subroutine DeclareField

    ! -------------------------------------------------------------------------
    !> This subroutine writes the value of an LFRic field
    !! to the NetCDF file. It uses the corresponding function
    !! provided by the base class.
    !! @param[inout] this The instance of the visualisation_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideField(this, name, value)
        use, intrinsic :: iso_c_binding
        use field_mod, only: field_type, field_proxy_type
        implicit none

        class(visualisation_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        type(field_type), intent(in)                            :: value

        type(field_proxy_type) :: value_proxy
        integer(c_int) :: result, n_field, n_chi

        value_proxy = value%get_proxy()
        call this%ProvideVariable(name, value_proxy%data)
        if (this%verbosity>1) &
            write(stderr,*) "PSyData: ProvideField: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name

        n_field = size(value_proxy%data)
        value_proxy%data(1) = 1.234
        value_proxy%data(size(value_proxy%data)) = 9.876
        result = update_plot_data(n_field, value_proxy%data)
    end subroutine ProvideField

    ! -------------------------------------------------------------------------
    !> This subroutine declares an LFric field vector. This represents the
    !! grid on which the field to be visualised is. Since the grid does not
    !! change, we only store the values the first time it is called.
    !! @param[inout] this The instance of the visualisation_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareFieldVector(this, name, value)
        use field_mod, only : field_type, field_proxy_type
        implicit none

        class(visualisation_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        type(field_type), dimension(:), intent(in)              :: value

        integer                :: i
        type(field_proxy_type) :: value_proxy1, value_proxy2, value_proxy3
        character(9)           :: number
        integer(c_int)         :: result, n_chi

        if (size(value, 1) .ne. 3) then
            write(stderr, *) "PSyData: Vector field '", name,"' should have 3 dimensions,"
            write(stderr, *) "         but has", size(value, 1), " - aborting."
            call this%Abort("Invalid dimension")
        endif

        if( .not. this%grid_defined) then
            result = initialise_python()

            value_proxy1 = value(1)%get_proxy()
            value_proxy2 = value(2)%get_proxy()
            value_proxy3 = value(3)%get_proxy()
            n_chi = size(value_proxy1%data)
            ! First time, store the chi field vector
            result = set_grid(n_chi, value_proxy1%data, value_proxy2%data, value_proxy3%data)
            this%grid_defined = .true.
        endif

        if(this%verbosity==2) then
            write(stderr, *) "PSyData: DeclareFieldVector ", trim(this%module_name), &
                    " ", trim(this%region_name),": ", name
        endif

    end subroutine DeclareFieldVector

    ! -------------------------------------------------------------------------
    !> This subroutine writes an LFRic field vector to the netcdf file. Each
    !! component is stored as an individual variable using the corresponding
    !! array function of the base class.
    !! @param[inout] this The instance of the visualisation_PsyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideFieldVector(this, name, value)
        use field_mod, only: field_type, field_proxy_type
        implicit none

        class(visualisation_PsyDataType), intent(inout), target :: this
        character(*), intent(in)                                :: name
        type(field_type), dimension(:), intent(in)              :: value

        integer                :: i
        type(field_proxy_type) :: value_proxy
        character(9)           :: number

        ! Provide each dimension of the vector as an individual 1d array.
        do i=1, size(value, 1)
            value_proxy = value(i)%get_proxy()
            write(number, '("%",i0)') i
            call this%ProvideVariable(name//trim(number), value_proxy%data)
        enddo

    end subroutine ProvideFieldVector


end module visualisation_psy_data_mod
