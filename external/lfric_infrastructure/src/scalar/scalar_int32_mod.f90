!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing scalar related classes for int32 data
!>
!> @details A representation of a int32 scalar which provides both
!> easy access to the scalar data and a method by which the PSy layer
!> can access the distributed memory aspects of the scalar

module scalar_int32_mod

  use, intrinsic :: iso_fortran_env, only : int32

  use constants_mod,      only: l_def
  use lfric_mpi_mod,      only: lfric_mpi_type, global_mpi
  implicit none
  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> PSy layer representation of a scalar.
  !>
  type, public :: scalar_int32_type
    private
    !> The value of the scalar
    integer(kind=int32), public :: value
    !> Object that describes what processes the scalar is represented over
    type(lfric_mpi_type)        :: mpi
    !> Logical for whether the initialiser has been called
    logical(l_def)              :: mpi_initialised = .false.
  contains
    !> initialiser to set up the scaler object
    procedure, public :: initialise
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
  end type scalar_int32_type

contains

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !> Initialise the scalar object with a value and an mpi object
  !> @param [in] value The value to be held
  !> @param [in] mpi The optional mpi object that represents the processes
  !>                 over which this scalar is held
  subroutine initialise(self, value, mpi)
    implicit none
    class(scalar_int32_type),   intent(inout) :: self
    integer(kind=int32),      intent(in)    :: value
    type(lfric_mpi_type), optional, intent(in)  :: mpi

    self%value = value
    if(present(mpi))then
      self%mpi_initialised = .true.
      self%mpi = mpi
    end if

  end subroutine initialise

  !! Start performing a global sum operation on a scalar
  !!
  function get_sum(self) result (g_sum)
    use lfric_mpi_mod, only: global_mpi
    implicit none
    class(scalar_int32_type), intent(inout) :: self
    integer(kind=int32) :: g_sum
    ! If an mpi object has been specified, use that, otherwise use global_mpi
    if(self%mpi_initialised)then
      call self%mpi%global_sum( self%value, g_sum )
    else
      call global_mpi%global_sum( self%value, g_sum )
    end if
  end function get_sum

  !! Start the calculation of the global minimum of a scalar
  !!
  function get_min(self) result (g_min)
    use lfric_mpi_mod, only: global_mpi
    implicit none
    class(scalar_int32_type), intent(inout) :: self
    integer(kind=int32) :: g_min
    ! If an mpi object has been specified, use that, otherwise use global_mpi
    if(self%mpi_initialised)then
      call self%mpi%global_min( self%value, g_min )
    else
      call global_mpi%global_min( self%value, g_min )
    end if
  end function get_min

  !! Start the calculation of the global maximum of a scalar
  !!
  function get_max(self) result (g_max)
    use lfric_mpi_mod, only: global_mpi
    implicit none
    class(scalar_int32_type), intent(inout) :: self
    integer(kind=int32) :: g_max
    ! If an mpi object has been specified, use that, otherwise use global_mpi
    if(self%mpi_initialised)then
      call self%mpi%global_max( self%value, g_max )
    else
      call global_mpi%global_max( self%value, g_max )
    end if
  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, we only have blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)
    implicit none
    class(scalar_int32_type), intent(in) :: self
    integer(kind=int32) ::  value_tmp
    value_tmp=self%value            ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.
  end subroutine reduction_finish

end module scalar_int32_mod
