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
! Modifications copyright (c) 2022-2023, Science and Technology Facilities Council
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
!> @brief A module containing the abstract type that is the parent to all
!>        child operator types.
!>
!> @details All concrete operator types are part of the same family.
!!          This module contains the abstract type that will be the parent
!!          to all of them.

module operator_parent_mod

  use constants_mod,            only : i_def
  use function_space_mod,       only : function_space_type
  use mesh_mod,                 only : mesh_type

  implicit none
  private

  !> Abstract operator type that is the parent of any operator type in
  !! the operator object hierarchy
  type, public, abstract :: operator_parent_type
    private
     !> Each operator has pointers to the two function spaces on which it
     !! is defined as a map from one to the other
     type( function_space_type ), pointer :: fs_from => null( )
     type( function_space_type ), pointer :: fs_to => null( )
     integer(kind=i_def), allocatable     :: gnu_dummy
  contains
    !> Initialise a parent operator object
    procedure, public :: operator_parent_initialiser
    !> Initialise a parent operator proxy object
    procedure, public :: operator_parent_proxy_initialiser
    !> Copies one operator paremnt type to another
    procedure, public :: copy_operator_parent
    !> Function returns functions_space which the local stencil maps from
    procedure, public :: get_fs_from
    !> Function returns the the functions_space which the local stencil maps to
    procedure, public :: get_fs_to
    !> Returns a pointer to the mesh on which the function spaces, used by
    !! function returns the enumerated integer for the function_spaces which
    !! the local stencil maps from
    procedure, public :: which_fs_from
    !> Function returns the enumerated integer for the function_spaces which
    !! the local stencil maps to
    procedure, public :: which_fs_to
    !> Returns a pointer to the mesh on which the function spaces, used by
    !! this operator, are built
    procedure, public :: get_mesh
    !> Finaliser for an operator parent object
    procedure, public :: destroy_operator_parent
  end type operator_parent_type

  !> Abstract operator proxy type that is the patrent of any operator proxy
  type, public, abstract :: operator_parent_proxy_type
    private
    !> Each operator has pointers to the function spaces which it lives "between"
    type( function_space_type ), pointer, public :: fs_to => null( )
    type( function_space_type ), pointer, public :: fs_from => null( )
    integer(kind=i_def), allocatable             :: gnu_dummy_proxy
  contains
    ! Finaliser for the object
    procedure, public :: destroy_operator_parent_proxy
  end type operator_parent_proxy_type

!______End of type declarations_______________________________________________

contains

  !> @brief Initialise a <code>operator_parent_type</code> object.
  !>
  !> @param[in] fs_from The function space that the operator maps from
  !> @param[in] fs_to   The function space that the operator maps to
  !>
  subroutine operator_parent_initialiser( self, fs_to, fs_from )
    implicit none
    class(operator_parent_type), target, intent(inout) :: self
    class(function_space_type),  target, intent(in)    :: fs_to
    class(function_space_type),  target, intent(in)    :: fs_from
    self%fs_to   => fs_to
    self%fs_from => fs_from
  end subroutine operator_parent_initialiser

  !> @brief Initialise public pointers that belong to the operator_parent_type.
  subroutine operator_parent_proxy_initialiser(self, operator_proxy)
   implicit none
   class(operator_parent_type), target, intent(in)  :: self
   class(operator_parent_proxy_type), intent(inout) :: operator_proxy
   operator_proxy%fs_to   => self%fs_to
   operator_proxy%fs_from => self%fs_from
  end subroutine operator_parent_proxy_initialiser

  !> @brief Copy the contents of one operator_parent_type to another.
  subroutine copy_operator_parent(self, dest)
    implicit none
    class(operator_parent_type), target, intent(in)    :: self
    class(operator_parent_type), target, intent(inout) :: dest
    dest%fs_to   => self%fs_to
    dest%fs_from => self%fs_from
  end subroutine copy_operator_parent

  ! Base type procedures

  !> @brief Get the function space the operator maps from.
  !> @return fs Function space the operator maps from
  function get_fs_from(self) result(fs)
    implicit none
    class(operator_parent_type), intent(in) :: self
    type( function_space_type ), pointer :: fs
    fs => self%fs_from
  end function get_fs_from

  !> @brief Get the function space the operator maps to.
  !> @return fs Function space the operator maps to
  function get_fs_to(self) result(fs)
    implicit none
    class(operator_parent_type), intent(in) :: self
    type( function_space_type ), pointer :: fs
    fs => self%fs_to
  end function get_fs_to

  !> @brief Get the ID of the function space the operator maps from.
  !> @return fs ID of function space the operator maps from
  function which_fs_from(self) result(fs)
    implicit none
    class(operator_parent_type), intent(in) :: self
    integer(kind=i_def) :: fs
    fs = self%fs_from%which()
  end function which_fs_from

  !> @brief Get the ID of the function space the operator maps to.
  !> @return fs ID of function space the operator maps to
  function which_fs_to(self) result(fs)
    implicit none
    class(operator_parent_type), intent(in) :: self
    integer(kind=i_def) :: fs
    fs = self%fs_to%which()
  end function which_fs_to

  !> @brief Get the mesh the operator lives on.
  !> @return mesh Mesh the operator lives on
  function get_mesh(self) result(mesh)
    implicit none
    class (operator_parent_type), intent(in) :: self
    type(mesh_type), pointer :: mesh
    mesh => self%fs_from%get_mesh()
  end function get_mesh

  !> @brief Destroy an operator_parent_type instance.
  subroutine destroy_operator_parent(self)
    implicit none
    class(operator_parent_type), intent(inout) :: self
    nullify( self%fs_to )
    nullify( self%fs_from )
  end subroutine destroy_operator_parent

  !> @brief Destroy an operator_parent_type instance.
  subroutine destroy_operator_parent_proxy(self)
    implicit none
    class(operator_parent_proxy_type), intent(inout) :: self
    nullify( self%fs_to )
    nullify( self%fs_from )
  end subroutine destroy_operator_parent_proxy

end module operator_parent_mod
