!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------

! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified: I. Kavcic, Met Office
!
!> @brief A module providing scalar-related classes.
!
! Notes: In LFRic infrastructure this module relies on mpi_mod.f90 which
!        performs the operations of finding a global sum, minimum, maximum
!        and reduction finish on a scalar. Here we only set dummy values for
!        the PSyclone compilation checks.
!
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

  !! Global sum of a scalar
  function get_sum(self) result (g_sum)

    implicit none

    class(scalar_type), intent(in) :: self

    real(kind=r_def) :: g_sum

    g_sum = 0.0_r_def

  end function get_sum

  !! Global minimum of a scalar
  function get_min(self) result (g_min)

    implicit none

    class(scalar_type), intent(in) :: self

    real(kind=r_def) :: g_min

    g_min = 0.0_r_def

  end function get_min

  !! Global maximum of a scalar
  function get_max(self) result (g_max)

    implicit none

    class(scalar_type), intent(in) :: self

    real(kind=r_def) :: g_max

    g_max = 0.0_r_def

  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, we only have blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)

    implicit none

    class(scalar_type), intent(in) :: self

    real(kind=r_def) ::  value_tmp

    value_tmp=self%value            ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.

  end subroutine reduction_finish

end module scalar_mod
