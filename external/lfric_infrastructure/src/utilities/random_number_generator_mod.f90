










!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief common class for generating random numbers in LFRic
!> @description class to store in modeldb to ensure random number generation
!> uses the intended seed

module random_number_generator_mod

  use key_value_mod, only                 : abstract_value_type
  use, intrinsic :: iso_fortran_env, only : real64, real32

  implicit none

  private

  public :: random_number_generator_type, &
            random_number_generator_seeder_interface, &
            random_number_generator_method_interface

  ! Abstract interfaces for external rng methods.
  ! Allows users to select an arbitrary rng generation method;
  ! Pointers to the actual methods are passed to constructor.

  abstract interface
    subroutine random_number_generator_seeder_interface(seed)
      integer, intent(in) :: seed
    end subroutine random_number_generator_seeder_interface
  end interface

  abstract interface
    subroutine random_number_generator_method_interface(result)
      real, intent(inout) :: result
    end subroutine random_number_generator_method_interface
  end interface

  !> random number generator that can be used as a key-value pair
  type, extends(abstract_value_type) :: random_number_generator_type

    !> The seed to use for the RNG
    integer :: seed
    !> Indicates whether or not the RNG has been seeded
    logical :: seeded

    !> A pointer to the subroutine used to generate a value for the RNG
    procedure(random_number_generator_method_interface), pointer, nopass :: method
    !> A pointer to the subroutine used to seed the RNG
    procedure(random_number_generator_seeder_interface), pointer, nopass :: seeder

    contains

      procedure :: check_seed
      procedure :: next_scalar
      procedure :: next_1d
      procedure :: next_2d
      procedure :: next_3d
      generic :: next => next_scalar, next_1d, next_2d, next_3d

  end type random_number_generator_type

  interface random_number_generator_type
    ! initialiser
    module procedure init_random_number_generator_type
  end interface

  contains

    ! default methods use built-in random_seed & random_number

    !> default method to seed the RNG (if no custom method is provided)
    !> @param[in] seed the seed to use
    subroutine default_random_number_generator_seeder(seed_value)
        implicit none
        integer, intent(in) :: seed_value

        integer, allocatable, dimension(:) :: seed
        integer :: seed_size

        call random_seed(size=seed_size)

        allocate(seed(seed_size))
        seed = seed_value

        call random_seed(put=seed)

    end subroutine default_random_number_generator_seeder

    !> default method to run the RNG
    !> @param[out] result the variable to store the random number in
    subroutine default_random_number_generator_method(result)
      implicit none
      real, intent(inout) :: result

      call random_number(result)
    end subroutine default_random_number_generator_method

    !> Initialiser for a RNG object
    !> @param [in] seed The number to use to seed the RNG
    !> @param [in] seeder A pointer to the subroutine to use to seed the RNG
    !> @param [in] method A pointer to the subroutine to use to generate the next value for the RNG
    function init_random_number_generator_type( seed, method, seeder ) &
             result(rng)

      implicit none

      integer, intent(in) :: seed
      type(random_number_generator_type) :: rng
      procedure(random_number_generator_seeder_interface), pointer, optional :: seeder
      procedure(random_number_generator_method_interface), pointer, optional :: method

      rng%seed = seed
      rng%seeded = .false.

      if (present(seeder)) then
        rng%seeder => seeder
      else
        rng%seeder => default_random_number_generator_seeder
      end if

      if (present(method)) then
        rng%method => method
      else
        rng%method => default_random_number_generator_method
      end if

    end function init_random_number_generator_type

    !> Seeds the RNG if it hasn't already been seeded
    subroutine check_seed(self)
      implicit none
      class(random_number_generator_type), intent(inout) :: self
      if (.not. self%seeded) then
        call self%seeder(self%seed)
        self%seeded = .true.
      end if
    end subroutine check_seed

    !> calls the RNG with a scalar
    subroutine next_scalar(self, results)
      implicit none
      class(random_number_generator_type), intent(inout) :: self
      real(kind=real64), intent(inout) :: results
      real :: native_result
      call self%check_seed()
      call self%method(native_result)
      results = real(native_result, real64)  ! coerce whatever fn_ptr returns to appropriate kind
    end subroutine next_scalar

    !> calls the RNG with a 1d array
    subroutine next_1d(self, results)
      implicit none
      class(random_number_generator_type), intent(inout) :: self
      real(kind=real64), dimension(:), intent(inout) :: results
      real :: native_result
      integer :: i
      call self%check_seed()
      do i = lbound(results, 1), ubound(results, 1)
        call self%method(native_result)
        results(i) = real(native_result, real64)  ! coerce whatever fn_ptr returns to appropriate kind
      end do
    end subroutine next_1d

    !> calls the RNG with a 2d array
    subroutine next_2d(self, results)
      implicit none
      class(random_number_generator_type), intent(inout) :: self
      real(kind=real64), dimension(:,:), intent(inout) :: results
      real :: native_result
      integer :: i, j
      call self%check_seed()
      do j = lbound(results, 2), ubound(results, 2)
        do i = lbound(results, 1), ubound(results, 1)
          call self%method(native_result)
          results(i,j) = real(native_result, real64)  ! coerce whatever fn_ptr returns to appropriate kind
        end do
      end do
    end subroutine next_2d

    !> calls the RNG with a 3d array
    subroutine next_3d(self, results)
      implicit none
      class(random_number_generator_type), intent(inout) :: self
      real(kind=real64), dimension(:,:,:), intent(inout) :: results
      real :: native_result
      integer :: i, j, k
      call self%check_seed()
      do k = lbound(results, 3), ubound(results, 3)
        do j = lbound(results, 2), ubound(results, 2)
          do i = lbound(results, 1), ubound(results, 1)
            call self%method(native_result)
            results(i,j,k) = real(native_result, real64)  ! coerce whatever fn_ptr returns to appropriate kind
          end do
        end do
      end do
    end subroutine next_3d

end module random_number_generator_mod
