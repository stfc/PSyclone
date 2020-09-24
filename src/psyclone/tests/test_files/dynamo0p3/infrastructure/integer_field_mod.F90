!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2020, Science and Technology Facilities
! Council
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
!
!> @brief A module providing integer field related classes.
!>
!> @details This is a version of a field object that can hold integer data
!> values. It contains both a representation of an integer field which provides
!> no access to the underlying data (to be used in the algorithm layer) and an
!> accessor class (to be used in the PSy layer) are provided.

module integer_field_mod

  use constants_mod,      only: r_def, i_def, l_def
  use function_space_mod, only: function_space_type
  use mesh_mod,           only: mesh_type

  use field_parent_mod,   only: field_parent_type, &
                                field_parent_proxy_type

  implicit none

  private

! Public types

!______integer_field_type_____________________________________________________

  !> Algorithm layer representation of an integer field.
  !>
  !> Objects of this type hold all the data of the field privately.
  !> Unpacking the data is done via the proxy type accessed by the PSy
  !> layer alone.
  !>
  type, extends(field_parent_type), public :: integer_field_type
    private

    !> The integer values of the field
    integer(kind=i_def), allocatable :: data( : )

  contains

    !> Function to get a proxy with public pointers to the data in a
    !! field_type.
    procedure, public :: get_proxy

  end type integer_field_type

!______integer_field_proxy_type_______________________________________________

  !> PSy layer representation of a field.
  !>
  !> This is an accessor class that allows access to the actual field
  !> information with each element accessed via a public pointer.
  !>
  type, extends(field_parent_proxy_type), public :: integer_field_proxy_type

    private

    !> Allocatable array of type integer which holds the values of the field
    integer(kind=i_def), public, pointer :: data( : ) => null()

  contains

    !> Performs a blocking halo exchange operation on the field.
    !> @todo This is temporarily required by PSyclone for initial development
    !! Eventually, the PSy layer will call the asynchronous versions of
    !! halo_exchange and this function should be removed.
    !! @param[in] depth The depth to which the halos should be exchanged
    procedure, public :: halo_exchange
    !> Starts a halo exchange operation on the field. The halo exchange
    !> is non-blocking, so this call only starts the process. On Return
    !> from this call, outbound data will have been transferred, but no
    !> guarantees are made for in-bound data elements at this stage.
    !! @param[in] depth The depth to which the halos should be exchanged
    procedure, public :: halo_exchange_start

    !> Wait (i.e. block) until the transfer of data in a halo exchange
    !> (started by a call to halo_exchange_start) has completed.
    !! @param[in] depth The depth to which the halos have been exchanged
    procedure, public :: halo_exchange_finish

  end type integer_field_proxy_type

!______end of type declarations_______________________________________________

contains

!______integer_field_type procedures__________________________________________

  !> Function to create a proxy with access to the data in the field_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> field_type
  type(integer_field_proxy_type) function get_proxy(self)
    implicit none
    class(integer_field_type), target, intent(in)  :: self

    ! Call the routine that initialises the proxy for data held in the parent
    call self%field_parent_proxy_initialiser(get_proxy)

    get_proxy%data   => self%data

  end function get_proxy

!______integer_field_proxy_type procedures____________________________________

  !! Perform a blocking halo exchange operation on the field
  !!
  subroutine halo_exchange( self, depth )

    implicit none

    class( integer_field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange

  !! Start a halo exchange operation on the field
  !!
  subroutine halo_exchange_start( self, depth )

    implicit none

    class( integer_field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_start

  !! Wait for a halo exchange to complete
  !!
  subroutine halo_exchange_finish( self, depth )

    implicit none

    class( integer_field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_finish

end module integer_field_mod
