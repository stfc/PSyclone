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
    use, intrinsic :: iso_c_binding, only: c_int

    use psy_data_base_mod, only: PSyDataBaseType

    implicit none

    !> This is the data type that manages the information required
    !! to write data to a NetCDF file using the PSyData API. A
    !! static instance of this type is created for each instrumented
    !! region with PSyclone (and each region will write a separate
    !! file).
    type, extends(PSyDataBaseType), public:: visualisation_PsyDataType
 
        real, allocatable, dimension(:,:) :: chi

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
      function python_caller(flock_num) bind(C, name="python_caller")
        ! Interface blocks don't know about their context,
        ! so we need to use iso_c_binding to get c_int definition
        !use, intrinsic::iso_c_binding, only : c_int
        import :: c_int
        integer(c_int) :: flock_num
        integer(c_int) :: python_caller
      end function python_caller
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
        result = python_caller(cc)
        print *,"from c", result
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

        value_proxy = value%get_proxy()
        call this%ProvideVariable(name, value_proxy%data)
        if (this%verbosity>1) &
            write(stderr,*) "PSyData: ProvideField: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name

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
        type(field_proxy_type) :: value_proxy
        character(9)           :: number

        if (size(value, 1) .ne. 3) then
            write(stderr, *) "PSyData: Vector field '", name,"' should have 3 dimensions,"
            write(stderr, *) "         but has", size(value, 1), " - aborting."
            call this%Abort("Invalid dimension")
        endif

        if( .not. allocated(this%chi)) then
            ! First time, store the chi field vector
            value_proxy = value(1)%get_proxy()
            print *,"Copying ",size(value_proxy%data)
            allocate(this%chi(size(value_proxy%data), 3))
            do i=1, 3
                value_proxy = value(i)%get_proxy()
                this%chi(:,i) = value_proxy%data(:)
            enddo
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
