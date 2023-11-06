!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing 32-bit real scalar related classes.
!>
!> @details A representation of a 32-bit real scalar which provides both easy
!> access to the scalar data and a method by which the PSy layer can access the
!> distributed memory aspects of the scalar

module scalar_r32_mod

  use, intrinsic :: iso_fortran_env, only : real32

  use constants_mod,      only: i_def
  implicit none
  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> PSy layer representation of a scalar.
  !>
  type, public :: scalar_r32_type
    private
    !> The value of the scalar
    real(kind=real32), public :: value
  contains
    !> Perform a global sum operation on the scalar
    !> @return The global sum of the scalar values over all ranks
    procedure, public :: get_sum
    !> Calculate the global minimum of the scalar
    !> @return The minimum of the scalar values over all ranks
    procedure, public :: get_min
    !> Calculate the global maximum of the scalar
    !> @return The maximum of the scalar values over all ranks
    procedure, public :: get_max
    !> Wait (i.e. block) until all current non-blocking reductions
    !> (sum, max, min) are complete.
    !>
    !> Currently only have blocking reductions, so this
    !> subroutine currently returns without waiting.
    procedure reduction_finish
  end type scalar_r32_type

contains

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !! Start performing a global sum operation on a scalar
  !!
  function get_sum(self) result (g_sum)
    use mpi_mod, only: global_sum
    implicit none
    class(scalar_r32_type), intent(in) :: self
    real(real32) :: g_sum
    call global_sum( self%value, g_sum )
  end function get_sum

  !! Start the calculation of the global minimum of a scalar
  !!
  function get_min(self) result (g_min)
    use mpi_mod, only: global_min
    implicit none
    class(scalar_r32_type), intent(in) :: self
    real(real32) :: g_min
    call global_min( self%value, g_min )
  end function get_min

  !! Start the calculation of the global maximum of a scalar
  !!
  function get_max(self) result (g_max)
    use mpi_mod, only: global_max
    implicit none
    class(scalar_r32_type), intent(in) :: self
    real(real32) :: g_max
    call global_max( self%value, g_max )
  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, we only have blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)
    implicit none
    class(scalar_r32_type), intent(in) :: self
    real(real32)    ::  value_tmp
    value_tmp=self%value            ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.
  end subroutine reduction_finish

end module scalar_r32_mod
