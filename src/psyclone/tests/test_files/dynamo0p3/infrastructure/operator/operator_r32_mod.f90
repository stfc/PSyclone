!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2023, Science and Technology Facilities Council
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
! Modified by: I. Kavcic, Met Office
!
!> @brief A module providing 32-bit operator related classes.
!>
!> @details Implements the locally assembled 32-bit operator (i.e. the stencil
!>          is assembled in each cell of the 3D grid).

module operator_r32_mod

  use, intrinsic :: iso_fortran_env, only : real32

  use constants_mod,            only : i_def, l_def
  use function_space_mod,       only : function_space_type
  use mesh_mod,                 only : mesh_type
  use operator_parent_mod,      only : operator_parent_type, &
                                       operator_parent_proxy_type
  implicit none
  private

  !> Algorithm layer representation of an operator type
  !>
   type, public, extends(operator_parent_type) :: operator_r32_type
    private
    !> Allocatable array of type real which holds the values of the operator
    real(kind=real32), allocatable :: local_stencil( :, :, : )
    !> Size of the outermost dimemsion of the local_stencil array, equal to
    !! ncell*nlayers
    integer(kind=i_def) :: ncell_3d
  contains
    !> Initialise an operator object
    procedure, public :: initialise => operator_r32_initialiser
    !> Function to get a proxy with public pointers to the data in a
    !! operator_r32_type.
    procedure, public :: get_proxy => get_proxy_operator_r32
    !> Deep copy methods
    procedure, private :: operator_r32_type_deep_copy
    procedure, public  :: deep_copy => operator_r32_type_deep_copy
    !> Checks if operator is initialised
    procedure, public :: is_initialised
    !> Destroys object
    procedure, public :: operator_final
    ! Finalizer for the object
    final :: operator_r32_destructor
 end type operator_r32_type

 !> PSy-layer representation of an operator.
 !>
 !> This is an accessor class that allows access to the actual operator
 !! information with each element accessed via a public pointer.
 !>
 type, public, extends(operator_parent_proxy_type) :: operator_r32_proxy_type
    private
    !> Allocatable array of type real which holds the values of the operator
    real(kind=real32), public, pointer :: local_stencil( :, :, : )
    !> Size of the outermost dimension
    integer(kind=i_def), public :: ncell_3d
    integer(kind=i_def), allocatable :: gnu_dummy(:)
  contains
    final :: operator_r32_proxy_destructor
 end type operator_r32_proxy_type

!______End of type declarations_______________________________________________

contains

  !> @brief Initialise an <code>operator_r32_type</code> object.
  !>
  !> @param[in] fs_from The function space that the operator maps from
  !> @param[in] fs_to   The function space that the operator maps to
  !>
  subroutine operator_r32_initialiser( self, fs_to, fs_from )

    use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_SIGNALING_NAN, IEEE_INVALID
    use, intrinsic :: ieee_exceptions, only: ieee_set_halting_mode, ieee_get_halting_mode

    implicit none
    class(operator_r32_type),   target, intent(inout) :: self
    class(function_space_type), target, intent(in)    :: fs_to
    class(function_space_type), target, intent(in)    :: fs_from

    ! Defines whether to halt when invalid floating point numbers are experienced
    logical :: halt_mode
    ! To be set to an invalid floating point number
    real(kind=real32) :: NaN

    ! Initialise the parent
    call self%operator_parent_initialiser( fs_to, fs_from )

    self%ncell_3d = fs_from%get_ncell() * fs_from%get_nlayers()
    ! Allocate the array in memory
    if(allocated(self%local_stencil))deallocate(self%local_stencil)

    ! If run-time checking for NaNs is on then initialise data with NaN
    call ieee_get_halting_mode(IEEE_INVALID, halt_mode)

    if (halt_mode) then
      ! Temporarily turn off halting mode to safely set invalid value
      call ieee_set_halting_mode(IEEE_INVALID, .false.)

      allocate(self%local_stencil( fs_to%get_ndf(),fs_from%get_ndf(), self%ncell_3d ), &
               source=ieee_value(NaN, IEEE_SIGNALING_NAN))

      call ieee_set_halting_mode(IEEE_INVALID, .true.)
    else
      ! Normal operator allocation
      allocate(self%local_stencil( fs_to%get_ndf(),fs_from%get_ndf(), self%ncell_3d ) )
    end if

  end subroutine operator_r32_initialiser

  !> @brief Function to create a proxy with access to the data in the
  !!        operator_r32_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !!         operator_r32_type
  type(operator_r32_proxy_type ) function get_proxy_operator_r32(self)
    implicit none
    class(operator_r32_type), target, intent(in)  :: self

    ! Call the routine that initialises the proxy for data held in the parent
    call self%operator_parent_proxy_initialiser(get_proxy_operator_r32)

    get_proxy_operator_r32 % local_stencil           => self % local_stencil
    get_proxy_operator_r32 % ncell_3d                =  self%ncell_3d
  end function get_proxy_operator_r32

  !> @brief Copies the current operator into a new returned object.
  !> @return The copy of operator type
  function operator_r32_type_deep_copy(self) result(other)
    implicit none
    class(operator_r32_type), intent(in) :: self
    type(operator_r32_type) :: other
    ! Make field_vector
    type( function_space_type ), pointer :: fs_to => null( )
    type( function_space_type ), pointer :: fs_from => null( )
    fs_to => self%get_fs_to()
    fs_from => self%get_fs_from()

    call other%initialise( fs_to, fs_from )
    other%local_stencil(:,:,:) = self%local_stencil(:,:,:)
  end function operator_r32_type_deep_copy

  !> @brief Return logical desribing whether operator has been initialised.
  function is_initialised(self) result(initialised)
    implicit none
    class(operator_r32_type), intent(in) :: self
    logical(kind=l_def)                  :: initialised
    initialised = allocated(self%local_stencil)
  end function is_initialised

  !> @brief Destroys the operator type.
  subroutine operator_final(self)
    implicit none
    class(operator_r32_type), intent(inout) :: self
    if(allocated(self%local_stencil)) then
       deallocate(self%local_stencil)
    end if
  end subroutine operator_final

  !> @brief Finalizer for the object type.
  subroutine operator_r32_destructor(self)
    implicit none
    type(operator_r32_type), intent(inout) :: self
    call self%destroy_operator_parent()
    call self%operator_final()
  end subroutine operator_r32_destructor

  !> @brief Destroy the operator proxy.
  subroutine operator_r32_proxy_destructor(self)
    implicit none
    type(operator_r32_proxy_type) :: self
    call self%destroy_operator_parent_proxy()
    nullify(self%local_stencil)
  end subroutine operator_r32_proxy_destructor

end module operator_r32_mod
