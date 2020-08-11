! ==================================================
! THIS FILE IS CREATED FROM THE JINJA TEMPLATE FILE 
! DO NOT MODIFY DIRECTLY                            
! ==================================================




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

module psy_data_base_mod
    use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                              real32, real64, &
                                              stderr=>Error_Unit
    implicit none

    !> Maximum string length for module- and region-names
    integer, parameter :: MAX_STRING_LENGTH = 512

    !> If this section is enabled or not. Controlled by the two
    !! static functions PSyDataStart and PSyDataStop. Note that
    !! none of the functions here use this value at all, it is up
    !! to the derived classes to implement the start/stop functionality.
    logical :: is_enabled = .true.

    !> This is a useful base class for PSyData wrapper libraries.
    !! It is not required to use this as a base, but it provides
    !! useful functionality.

    type, public:: PSyDataBaseType

        !> The index of the variables as they are being declared
        !! and as they are being provided. It is useful to provided
        !! a variable-dependent index.
        integer :: next_var_index

        !> Verbosity level for output at runtime. This is taken from the
        !! PSYDATA_VERBOSE environment variable.
        !! 0: Only errors will be written (PSYDATA_VERBOSE undefined)
        !! 1: Additionally write the name of the confirmed kernel_name
        !! 2: Also write the name of each tested variable
        integer :: verbosity

        !> Store the name of the module and region
        character(MAX_STRING_LENGTH) :: module_name, region_name

        contains
            procedure :: PreStart, PreEndDeclaration, PreEnd
            procedure :: PostStart, PostEnd, Abort
            procedure :: DeclareScalarInt
            procedure :: ProvideScalarInt
                procedure :: DeclareArray1dInt
                procedure :: ProvideArray1dInt
                procedure :: DeclareArray2dInt
                procedure :: ProvideArray2dInt
                procedure :: DeclareArray3dInt
                procedure :: ProvideArray3dInt
                procedure :: DeclareArray4dInt
                procedure :: ProvideArray4dInt
            procedure :: DeclareScalarReal
            procedure :: ProvideScalarReal
                procedure :: DeclareArray1dReal
                procedure :: ProvideArray1dReal
                procedure :: DeclareArray2dReal
                procedure :: ProvideArray2dReal
                procedure :: DeclareArray3dReal
                procedure :: ProvideArray3dReal
                procedure :: DeclareArray4dReal
                procedure :: ProvideArray4dReal
            procedure :: DeclareScalarDouble
            procedure :: ProvideScalarDouble
                procedure :: DeclareArray1dDouble
                procedure :: ProvideArray1dDouble
                procedure :: DeclareArray2dDouble
                procedure :: ProvideArray2dDouble
                procedure :: DeclareArray3dDouble
                procedure :: ProvideArray3dDouble
                procedure :: DeclareArray4dDouble
                procedure :: ProvideArray4dDouble

    end type PSyDataBaseType

contains

    ! -------------------------------------------------------------------------
    !> This subroutine is the first function called when an instrumented region
    !! is entered. It initialises this object, and stores module and regin
    !! names. 
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] module_name The name of the module of the instrumented
    !!            region.
    !! @param[in] region_name The name of the instrumented region.
    !! @param[in] num_pre_vars The number of variables that are declared and
    !!            written before the instrumented region (unused atm).
    !! @param[in] num_post_vars The number of variables that are also declared
    !!            before an instrumented region of code, but are written after
    !!            this region (unused atm).
    subroutine PreStart(this, module_name, region_name, num_pre_vars, &
                        num_post_vars)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: module_name, region_name
        integer, intent(in)      :: num_pre_vars, num_post_vars

        character(1) :: verbose
        integer      :: status

        call get_environment_variable("PSYDATA_VERBOSE", verbose, status=status)
        this%verbosity=0
        if (status==0) then
            if(verbose=="0") then
                this%verbosity = 0
            else if (verbose=="1") then
                this%verbosity = 1
            else if (verbose=="2") then
                this%verbosity = 2
            else
                write(stderr,*) "PSyData: invalid setting of PSYDATA_VERBOSE."
                write(stderr,*) "It must be '0', 1' or '2', but it is '", verbose,"'."
                this%verbosity = 0
            endif
        endif

        this%next_var_index = 1
        this%module_name = module_name
        this%region_name = region_name

        if (.not. is_enabled) return

        if(this%verbosity>0) &
            write(stderr,*) "PSyData: PreStart ", module_name, " ", region_name
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called once all variables are declared. It makes
    !! sure that the next variable index is starting at 1 again.
    !! @param[inout] this The instance of the PSyDataBaseType.
    subroutine PreEndDeclaration(this)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        this%next_var_index = 1
    end subroutine PreEndDeclaration

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the value of all variables has been
    !! provided (and declared). After this call the instrumented region will
    !! be executed. Nothing is done here.
    !! @param[inout] this The instance of the PSyDataBaseType.
    subroutine PreEnd(this)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
    end subroutine PreEnd

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed. After this call the value of variables after the instrumented
    !! region will be provided. This subroutine resets the next variable index
    !! to 1  again.
    !! @param[inout] this The instance of the PSyDataBaseType.
    subroutine PostStart(this)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        this%next_var_index = 1
    end subroutine PostStart

    ! -------------------------------------------------------------------------
    !> This subroutine is called after the instrumented region has been
    !! executed and all values of variables after the instrumented
    !! region have been provided. No special functionality required here.
    !! @param[inout] this The instance of the PSyDataBaseType.
    subroutine PostEnd(this)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this

        if (.not. is_enabled) return

        if(this%verbosity>0) &
            write(stderr, *) "PSyData: PostEnd ", trim(this%module_name), &
                    " ", trim(this%region_name)
    end subroutine PostEnd

    ! -------------------------------------------------------------------------
    !> Displays the message and aborts execution. This base implementation
    !! just uses `stop`.
    !! @param[in] message Error message to be displayed (string).
    subroutine Abort(this, message)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*) :: message

        stop message
    end subroutine Abort

    ! -------------------------------------------------------------------------
    !> An optional initialisation subroutine.
    subroutine read_only_verify_PSyDataInit()
      implicit none
      return
    end subroutine read_only_verify_PSyDataInit

    ! -------------------------------------------------------------------------
    !> An optional initialisation subroutine.
    subroutine read_only_verify_PSyDataShutdown()
      implicit none
      return
    end subroutine read_only_verify_PSyDataShutdown

    ! -------------------------------------------------------------------------
    !> Enables PSyData handling (if it is not already enabled).
    subroutine read_only_verify_PSyDataStart()
      implicit none
      is_enabled = .true.
    end subroutine read_only_verify_PSyDataStart

    ! -------------------------------------------------------------------------
    !> Disables PSyData handling.
    subroutine read_only_verify_PSyDataStop()
      implicit none
      is_enabled = .false.
    end subroutine read_only_verify_PSyDataStop

    ! =========================================================================
    ! Jinja created code:
    ! =========================================================================
    ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar integer(kind=int32) value. This
    !! implementation only increases the next index, and prints
    !! the name of the variable if verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), intent(in) :: value

        this%next_var_index = this%next_var_index+1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareScalarInt: ", &
                            trim(this%module_name), " ",                &
                            trim(this%region_name), ": ", name
    end subroutine DeclareScalarInt

    ! -------------------------------------------------------------------------
    !> This subroutine provides a scalar integer(kind=int32) value. This
    !! implementation only increases the next index.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideScalarInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), intent(in) :: value

        this%next_var_index = this%next_var_index+1
    end subroutine ProvideScalarInt

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 1d-array of
    !! integer(kind=int32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray1dInt: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray1dInt

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 1d integer(kind=int32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray1dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray1dInt

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 2d-array of
    !! integer(kind=int32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray2dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray2dInt: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray2dInt

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 2d integer(kind=int32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray2dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray2dInt

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 3d-array of
    !! integer(kind=int32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray3dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray3dInt: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray3dInt

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 3d integer(kind=int32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray3dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray3dInt

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 4d-array of
    !! integer(kind=int32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray4dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray4dInt: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray4dInt

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 4d integer(kind=int32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray4dInt(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        integer(kind=int32), dimension(:,:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray4dInt

      ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar real(kind=real32) value. This
    !! implementation only increases the next index, and prints
    !! the name of the variable if verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), intent(in) :: value

        this%next_var_index = this%next_var_index+1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareScalarReal: ", &
                            trim(this%module_name), " ",                &
                            trim(this%region_name), ": ", name
    end subroutine DeclareScalarReal

    ! -------------------------------------------------------------------------
    !> This subroutine provides a scalar real(kind=real32) value. This
    !! implementation only increases the next index.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideScalarReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), intent(in) :: value

        this%next_var_index = this%next_var_index+1
    end subroutine ProvideScalarReal

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 1d-array of
    !! real(kind=real32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray1dReal: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray1dReal

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 1d real(kind=real32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray1dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray1dReal

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 2d-array of
    !! real(kind=real32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray2dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray2dReal: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray2dReal

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 2d real(kind=real32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray2dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray2dReal

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 3d-array of
    !! real(kind=real32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray3dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray3dReal: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray3dReal

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 3d real(kind=real32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray3dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray3dReal

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 4d-array of
    !! real(kind=real32) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray4dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:,:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray4dReal: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray4dReal

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 4d real(kind=real32) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray4dReal(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real32), dimension(:,:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray4dReal

      ! -------------------------------------------------------------------------
    !> This subroutine declares a scalar real(kind=real64) value. This
    !! implementation only increases the next index, and prints
    !! the name of the variable if verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareScalarDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), intent(in) :: value

        this%next_var_index = this%next_var_index+1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareScalarDouble: ", &
                            trim(this%module_name), " ",                &
                            trim(this%region_name), ": ", name
    end subroutine DeclareScalarDouble

    ! -------------------------------------------------------------------------
    !> This subroutine provides a scalar real(kind=real64) value. This
    !! implementation only increases the next index.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideScalarDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), intent(in) :: value

        this%next_var_index = this%next_var_index+1
    end subroutine ProvideScalarDouble

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 1d-array of
    !! real(kind=real64) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray1dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray1dDouble: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray1dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 1d real(kind=real64) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray1dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray1dDouble

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 2d-array of
    !! real(kind=real64) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray2dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray2dDouble: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray2dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 2d real(kind=real64) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray2dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray2dDouble

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 3d-array of
    !! real(kind=real64) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray3dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray3dDouble: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray3dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 3d real(kind=real64) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray3dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray3dDouble

    ! ---------------------------------------------------------------------
    !> This subroutine handles a declaration of a 4d-array of
    !! real(kind=real64) values. This base implementation only increases
    !! next_var_index and prints the name of the variable if
    !! verbose output is requested.
    !! @param[inout] this The instance of the PSyDataBaseType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine DeclareArray4dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:,:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1

        if (.not. is_enabled) return

        if (this%verbosity>1) &
            write(stderr,*) "PSyData: DeclareArray4dDouble: ", &
                            trim(this%module_name), " ",               &
                            trim(this%region_name), ": ", name
    end subroutine DeclareArray4dDouble

    ! -------------------------------------------------------------------------
    !> This subroutine handles a provide call for a 4d real(kind=real64) array.
    !! This base implementation only increases next_var_index.
    !! @param[inout] this The instance of the read_only_verify_PSyDataType.
    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    subroutine ProvideArray4dDouble(this, name, value)
        implicit none
        class(PSyDataBaseType), intent(inout), target :: this
        character(*), intent(in) :: name
        real(kind=real64), dimension(:,:,:,:), intent(in) :: value
        this%next_var_index = this%next_var_index + 1
    end subroutine ProvideArray4dDouble

     
end module psy_data_base_mod
