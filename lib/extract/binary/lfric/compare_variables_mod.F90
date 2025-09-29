! ================================================== !
! THIS FILE IS CREATED FROM THE JINJA TEMPLATE FILE. !
! DO NOT MODIFY DIRECTLY!                            !
! ================================================== !



! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
! Author: J. Henrichs, Bureau of Meteorology

module compare_variables_mod

    use, intrinsic :: iso_fortran_env, only : real64, stderr => Error_Unit

    implicit None
    integer, parameter :: COUNT_ALL      = 1
    integer, parameter :: COUNT_0        = 2    ! No error
    integer, parameter :: COUNT_NEG_9    = 3    ! 10^-9 > rel error > 0
    integer, parameter :: COUNT_NEG_6    = 4    ! 10^-6 > rel error >=10^-9
    integer, parameter :: COUNT_NEG_3    = 5    ! 10^-3 > rel error >=10^-6
    integer, parameter :: COUNT_LARGER   = 6    ! rel error >=10^-3
    integer, parameter :: MAX_ABS_ERROR  = 7
    integer, parameter :: MAX_REL_ERROR  = 8
    integer, parameter :: L2_DIFF        = 9
    integer, parameter :: L2_COS_SIMILAR = 10

    integer, parameter :: NUM_RESULTS = 10

    integer, parameter                                        :: MAX_STRING_LENGTH=512
    character(MAX_STRING_LENGTH), dimension(:),   allocatable :: all_names
    real(kind=real64),            dimension(:,:), allocatable :: all_results
    integer                                                   :: current_index = 0

    ! Declare generic interface for Compare:
    interface compare
        module procedure compare_scalar_Char
        module procedure compare_array_1dChar
        module procedure compare_array_2dChar
        module procedure compare_array_3dChar
        module procedure compare_array_4dChar
        module procedure compare_scalar_Int
        module procedure compare_array_1dInt
        module procedure compare_array_2dInt
        module procedure compare_array_3dInt
        module procedure compare_array_4dInt
        module procedure compare_scalar_Logical
        module procedure compare_array_1dLogical
        module procedure compare_array_2dLogical
        module procedure compare_array_3dLogical
        module procedure compare_array_4dLogical
        module procedure compare_scalar_Real
        module procedure compare_array_1dReal
        module procedure compare_array_2dReal
        module procedure compare_array_3dReal
        module procedure compare_array_4dReal
        module procedure compare_scalar_Double
        module procedure compare_array_1dDouble
        module procedure compare_array_2dDouble
        module procedure compare_array_3dDouble
        module procedure compare_array_4dDouble
    end interface

contains

    subroutine compare_init(num_vars)
        implicit none
        integer :: num_vars, error

        allocate(all_names(num_vars), stat=error)
        if (error /= 0) then
            write(stderr,*) "Cannot allocate array for ", num_vars, &
                            " result names."
            stop
        endif
        allocate(all_results(num_vars, NUM_RESULTS), stat=error)
        if (error /= 0) then
            write(stderr,*) "Cannot allocate array for ", num_vars, &
                            " result summaries."
            stop
        endif
        current_index = 0

    end subroutine compare_init

    ! -------------------------------------------------------------------------
    subroutine compare_summary()
        implicit none
        integer :: i, max_name_len
        character(256) :: out_format

        ! First compute the format, based on maximum name length:
        max_name_len = -1

        do i=1, current_index
            if (len(trim(all_names(i))) > max_name_len) then
                max_name_len = len(trim(all_names(i)))
            endif
        enddo

        write(out_format, "('(A',I0)" ) max_name_len
        write(*,out_format//",10A13)") "Variable", "count", "identical",  &
            "#rel<1E-9", "#rel<1E-6", "#rel<1E-3", "#rel>=1E-3", &
            "max_abs", "max_rel", "l2_diff", "l2_cos"

        out_format = trim(out_format)//",' ',6(I12, ' '),4(E12.7,' '))"

        ! Then write out the results for each variable:
        do i=1, current_index
            write(*,out_format) trim(all_names(i)), int(all_results(i,1:6)), &
                                all_results(i,7:)
        enddo

    end subroutine compare_summary

    ! -------------------------------------------------------------------------

    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a scalar character(*)
    !! variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results  will be printed when `compare_summary` is called.

    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[in] correct_value The expected value of the variable.
    subroutine compare_scalar_Char(name, value, correct_value)
        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64 
        implicit none
        character(*), intent(in)           :: value, correct_value
        character(*)                   :: name

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index,COUNT_ALL) = 1.0
        if (value == correct_value) then
            ! All other values have already been initialised with 0
            all_results(current_Index, L2_COS_SIMILAR) = 1
            all_results(current_Index, COUNT_0       ) = 1
        else   ! Results are different
            ! Set all errors to 1
            all_results(current_index, MAX_ABS_ERROR ) = 1.0
            all_results(current_index, L2_DIFF       ) = 1.0
            all_results(current_index, L2_COS_SIMILAR) = 0.0
            all_results(current_index, MAX_REL_ERROR ) = 1.0
            all_results(current_Index, COUNT_LARGER  ) = 1
        endif

    end subroutine compare_scalar_Char




    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 1D array of
    !! character(*) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_1dChar(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        character(*), dimension(:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:), allocatable :: double_values
        double precision, dimension(:), allocatable :: double_correct
        double precision, dimension(:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! We convert the correct strings to be a '1', and the computed values to
            ! be either 0 if the strings are different, or 1 otherwise.
            allocate(double_values, source=merge(0.0d0, 1.0d0, values /= correct_values))
            ! We need an array shape of booleans here
            allocate(double_correct, source=merge(1.0d0, 0.0d0, values == values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_1dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 2D array of
    !! character(*) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_2dChar(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        character(*), dimension(:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:), allocatable :: double_values
        double precision, dimension(:,:), allocatable :: double_correct
        double precision, dimension(:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! We convert the correct strings to be a '1', and the computed values to
            ! be either 0 if the strings are different, or 1 otherwise.
            allocate(double_values, source=merge(0.0d0, 1.0d0, values /= correct_values))
            ! We need an array shape of booleans here
            allocate(double_correct, source=merge(1.0d0, 0.0d0, values == values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_2dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 3D array of
    !! character(*) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_3dChar(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        character(*), dimension(:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! We convert the correct strings to be a '1', and the computed values to
            ! be either 0 if the strings are different, or 1 otherwise.
            allocate(double_values, source=merge(0.0d0, 1.0d0, values /= correct_values))
            ! We need an array shape of booleans here
            allocate(double_correct, source=merge(1.0d0, 0.0d0, values == values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_3dChar



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 4D array of
    !! character(*) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_4dChar(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        character(*), dimension(:,:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! We convert the correct strings to be a '1', and the computed values to
            ! be either 0 if the strings are different, or 1 otherwise.
            allocate(double_values, source=merge(0.0d0, 1.0d0, values /= correct_values))
            ! We need an array shape of booleans here
            allocate(double_correct, source=merge(1.0d0, 0.0d0, values == values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_4dChar

    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a scalar integer(kind=int32)
    !! variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results  will be printed when `compare_summary` is called.

    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[in] correct_value The expected value of the variable.
    subroutine compare_scalar_Int(name, value, correct_value)
        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64 
        implicit none
        integer(kind=int32), intent(in)           :: value, correct_value
        character(*)                   :: name

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index,COUNT_ALL) = 1.0
        if (value == correct_value) then
            ! All other values have already been initialised with 0
            all_results(current_Index, L2_COS_SIMILAR) = 1
            all_results(current_Index, COUNT_0       ) = 1
        else   ! Results are different
            all_results(current_index, MAX_ABS_ERROR ) = correct_value - value
            if (correct_value /= 0) then
                all_results(current_index, MAX_REL_ERROR) = &
                    abs((correct_Value-value)/real(value))
            else
                ! Division by 0
                all_results(current_index, MAX_REL_ERROR) = 1.0
            endif
            if(all_results(current_index, MAX_REL_ERROR) >= 1e-3) then
                all_results(current_Index, COUNT_NEG_3   ) = 1
            else if(all_results(current_index, MAX_REL_ERROR) >= 1e-6) then
                all_results(current_Index, COUNT_NEG_6   ) = 1
            else
                all_results(current_Index, COUNT_NEG_9   ) = 1
            endif
        endif

    end subroutine compare_scalar_Int




    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 1D array of
    !! integer(kind=int32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_1dInt(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        integer(kind=int32), dimension(:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:), allocatable :: double_values
        double precision, dimension(:), allocatable :: double_correct
        double precision, dimension(:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_1dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 2D array of
    !! integer(kind=int32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_2dInt(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        integer(kind=int32), dimension(:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:), allocatable :: double_values
        double precision, dimension(:,:), allocatable :: double_correct
        double precision, dimension(:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_2dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 3D array of
    !! integer(kind=int32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_3dInt(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        integer(kind=int32), dimension(:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_3dInt



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 4D array of
    !! integer(kind=int32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_4dInt(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        integer(kind=int32), dimension(:,:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_4dInt

    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a scalar Logical(kind=4)
    !! variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results  will be printed when `compare_summary` is called.

    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[in] correct_value The expected value of the variable.
    subroutine compare_scalar_Logical(name, value, correct_value)
        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64 
        implicit none
        Logical(kind=4), intent(in)           :: value, correct_value
        character(*)                   :: name

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index,COUNT_ALL) = 1.0
        if (value .EQV. correct_value) then
            ! All other values have already been initialised with 0
            all_results(current_Index, L2_COS_SIMILAR) = 1
            all_results(current_Index, COUNT_0       ) = 1
        else   ! Results are different
            ! Set all errors to 1
            all_results(current_index, MAX_ABS_ERROR ) = 1.0
            all_results(current_index, L2_DIFF       ) = 1.0
            all_results(current_index, L2_COS_SIMILAR) = 0.0
            all_results(current_index, MAX_REL_ERROR ) = 1.0
            all_results(current_Index, COUNT_LARGER  ) = 1
        endif

    end subroutine compare_scalar_Logical




    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 1D array of
    !! Logical(kind=4) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_1dLogical(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        Logical(kind=4), dimension(:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:), allocatable :: double_values
        double precision, dimension(:), allocatable :: double_correct
        double precision, dimension(:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values .EQV. correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! Convert the logical values to real to avoid special cases:
            allocate(double_values, source=merge(1.0d0, 0.0d0, values))
            allocate(double_correct, source=merge(1.0d0, 0.0d0, correct_values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_1dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 2D array of
    !! Logical(kind=4) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_2dLogical(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        Logical(kind=4), dimension(:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:), allocatable :: double_values
        double precision, dimension(:,:), allocatable :: double_correct
        double precision, dimension(:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values .EQV. correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! Convert the logical values to real to avoid special cases:
            allocate(double_values, source=merge(1.0d0, 0.0d0, values))
            allocate(double_correct, source=merge(1.0d0, 0.0d0, correct_values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_2dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 3D array of
    !! Logical(kind=4) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_3dLogical(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        Logical(kind=4), dimension(:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values .EQV. correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! Convert the logical values to real to avoid special cases:
            allocate(double_values, source=merge(1.0d0, 0.0d0, values))
            allocate(double_correct, source=merge(1.0d0, 0.0d0, correct_values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_3dLogical



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 4D array of
    !! Logical(kind=4) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_4dLogical(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        Logical(kind=4), dimension(:,:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values .EQV. correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
! Convert the logical values to real to avoid special cases:
            allocate(double_values, source=merge(1.0d0, 0.0d0, values))
            allocate(double_correct, source=merge(1.0d0, 0.0d0, correct_values))
            ! Now use the double precision arrays for computing the statistics

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_4dLogical

    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a scalar real(kind=real32)
    !! variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results  will be printed when `compare_summary` is called.

    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[in] correct_value The expected value of the variable.
    subroutine compare_scalar_Real(name, value, correct_value)
        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64 
        implicit none
        real(kind=real32), intent(in)           :: value, correct_value
        character(*)                   :: name

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index,COUNT_ALL) = 1.0
        if (value == correct_value) then
            ! All other values have already been initialised with 0
            all_results(current_Index, L2_COS_SIMILAR) = 1
            all_results(current_Index, COUNT_0       ) = 1
        else   ! Results are different
            all_results(current_index, MAX_ABS_ERROR ) = correct_value - value
            if (correct_value /= 0) then
                all_results(current_index, MAX_REL_ERROR) = abs((correct_Value-value)/value)
            else
                ! Division by 0
                all_results(current_index, MAX_REL_ERROR) = 1.0
            endif
            if(all_results(current_index, MAX_REL_ERROR) >= 1e-3) then
                all_results(current_Index, COUNT_NEG_3   ) = 1
            else if(all_results(current_index, MAX_REL_ERROR) >= 1e-6) then
                all_results(current_Index, COUNT_NEG_6   ) = 1
            else
                all_results(current_Index, COUNT_NEG_9   ) = 1
            endif
        endif

    end subroutine compare_scalar_Real




    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 1D array of
    !! real(kind=real32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_1dReal(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real32), dimension(:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:), allocatable :: double_values
        double precision, dimension(:), allocatable :: double_correct
        double precision, dimension(:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_1dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 2D array of
    !! real(kind=real32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_2dReal(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real32), dimension(:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:), allocatable :: double_values
        double precision, dimension(:,:), allocatable :: double_correct
        double precision, dimension(:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_2dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 3D array of
    !! real(kind=real32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_3dReal(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real32), dimension(:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_3dReal



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 4D array of
    !! real(kind=real32) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_4dReal(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real32), dimension(:,:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_4dReal

    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a scalar real(kind=real64)
    !! variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results  will be printed when `compare_summary` is called.

    !! @param[in] name The name of the variable (string).
    !! @param[in] value The value of the variable.
    !! @param[in] correct_value The expected value of the variable.
    subroutine compare_scalar_Double(name, value, correct_value)
        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64 
        implicit none
        real(kind=real64), intent(in)           :: value, correct_value
        character(*)                   :: name

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index,COUNT_ALL) = 1.0
        if (value == correct_value) then
            ! All other values have already been initialised with 0
            all_results(current_Index, L2_COS_SIMILAR) = 1
            all_results(current_Index, COUNT_0       ) = 1
        else   ! Results are different
            all_results(current_index, MAX_ABS_ERROR ) = correct_value - value
            if (correct_value /= 0) then
                all_results(current_index, MAX_REL_ERROR) = abs((correct_Value-value)/value)
            else
                ! Division by 0
                all_results(current_index, MAX_REL_ERROR) = 1.0
            endif
            if(all_results(current_index, MAX_REL_ERROR) >= 1e-3) then
                all_results(current_Index, COUNT_NEG_3   ) = 1
            else if(all_results(current_index, MAX_REL_ERROR) >= 1e-6) then
                all_results(current_Index, COUNT_NEG_6   ) = 1
            else
                all_results(current_Index, COUNT_NEG_9   ) = 1
            endif
        endif

    end subroutine compare_scalar_Double




    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 1D array of
    !! real(kind=real64) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_1dDouble(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real64), dimension(:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:), allocatable :: double_values
        double precision, dimension(:), allocatable :: double_correct
        double precision, dimension(:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_1dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 2D array of
    !! real(kind=real64) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_2dDouble(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real64), dimension(:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:), allocatable :: double_values
        double precision, dimension(:,:), allocatable :: double_correct
        double precision, dimension(:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_2dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 3D array of
    !! real(kind=real64) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_3dDouble(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real64), dimension(:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_3dDouble



    ! -------------------------------------------------------------------------
    !> @brief This subroutine compares the value of a 4D array of
    !! real(kind=real64) variable with the expected correct value and adds statistics
    !! about this comparison to the global field all_result fields. The
    !! results will be printed when `compare_summary` is called.
    !! @param[in] name The name of the variable (string).
    !! @param[in] values The values of the variable.
    !! @param[in] correct_values The expected value of the variable.
    subroutine compare_array_4dDouble(name, values, correct_values)

        use, intrinsic :: iso_fortran_env, only : int64, int32,   &
                                                  real32, real64
        implicit none

        real(kind=real64), dimension(:,:,:,:), intent(in)  :: values, correct_values
        character(*), intent(in)                        :: name

        ! Convert any type to double to be able to use the same maths:
        double precision, dimension(:,:,:,:), allocatable :: double_values
        double precision, dimension(:,:,:,:), allocatable :: double_correct
        double precision, dimension(:,:,:,:), allocatable :: tmp

        current_index = current_index + 1
        all_names(current_index) = name
        all_results(current_index,:) = 0.0
        all_results(current_index, COUNT_ALL) = size(values)
        if (all(values == correct_values)) then
            ! All values correct. Notice that all results are already initialised
            ! to 0, so only set the non-zero values here:
            all_results(current_index, L2_COS_SIMILAR) = 1
            all_results(current_index, COUNT_0       ) = size(correct_values)
        else
            ! There are errors
            allocate(double_values, source=dble(values))
            allocate(double_correct, source=dble(correct_values))

            allocate(tmp, mold=double_values)

            tmp = double_correct - double_values
            all_results(current_index, MAX_ABS_ERROR) = maxval(abs(tmp))
            all_results(current_index, L2_DIFF) = sqrt(real(sum(tmp*tmp)))
            all_results(current_index, L2_COS_SIMILAR) = &
                sum(double_values*double_correct)        &
                / sqrt(real(sum(double_values*double_values)))  &
                / sqrt(real(sum(double_correct*double_correct)))
            all_results(current_index, COUNT_0) = count(tmp == 0.0d0)

            where(double_correct /= 0)
                tmp = abs(tmp/double_correct)
            elsewhere
                tmp = -1
            endwhere
            all_results(current_index, MAX_REL_ERROR) = maxval(tmp)
            all_results(current_index, COUNT_LARGER) = count(tmp >= 1.0d-3)
            all_results(current_index, COUNT_NEG_9) = count(tmp < 1.0d-9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_6) = count(tmp < 1.0d-6) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_0)
            all_results(current_index, COUNT_NEG_3) = count(tmp < 1.0d-3) &
                - all_results(current_Index, COUNT_NEG_9) &
                - all_results(current_Index, COUNT_NEG_6) &
                - all_results(current_Index, COUNT_0)
        endif

    end subroutine Compare_array_4dDouble

end module compare_variables_mod
