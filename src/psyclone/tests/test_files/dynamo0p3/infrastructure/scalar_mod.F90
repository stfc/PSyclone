!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> @brief A module providing scalar related classes.
!>
!> @details A representation of a scalar which provides both easy access to the
!> scalar data and a method by which the PSy layer can access the distributed
!> memory aspects of the scalar


module scalar_mod

  use constants_mod,      only: r_def, i_def

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> PSy layer representation of a scalar.
  !>
  type, public :: scalar_type
    private

    !> The value of the scalar
    real(kind=r_def), public :: value

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

  end type scalar_type

  interface scalar_type
    module procedure scalar_constructor
  end interface

contains

  !> Construct a <code>scalar_type</code> object.
  !>
  !> @param [in] value The value with which to initialize the scalar
  !> @return self the field
  !>
  function scalar_constructor(value) result(self)

    implicit none

    real(kind=r_def), intent(in) :: value
    type(scalar_type), target :: self

    self%value = value

  end function scalar_constructor

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !! Start performing a global sum operation on a scalar
  !!
  function get_sum(self) result (g_sum)

    use mpi_mod, only: global_sum
    implicit none

    class(scalar_type), intent(in) :: self

    real(r_def) :: g_sum

    call global_sum( self%value, g_sum )

  end function get_sum

  !! Start the calculation of the global minimum of a scalar
  !!
  function get_min(self) result (g_min)

    use mpi_mod, only: global_min
    implicit none

    class(scalar_type), intent(in) :: self

    real(r_def) :: g_min

    call global_min( self%value, g_min )

  end function get_min

  !! Start the calculation of the global maximum of a scalar
  !!
  function get_max(self) result (g_max)

    use mpi_mod, only: global_max
    implicit none

    class(scalar_type), intent(in) :: self

    real(r_def) :: g_max

    call global_max( self%value, g_max )

  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, we only have blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)

    implicit none

    class(scalar_type), intent(in) :: self

    real(r_def)    ::  value_tmp

    value_tmp=self%value            ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.

  end subroutine reduction_finish

end module scalar_mod
