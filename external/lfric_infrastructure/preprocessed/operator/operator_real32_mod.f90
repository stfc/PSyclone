!-----------------------------------------------------------------------------
! (C) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing operator related classes for operators with values
!>        of kind real32.
!>
!> @details Implements the locally assembled operator (i.e. the stencil
!>          is assembled in each cell of the 3d grid)

module operator_real32_mod

  use, intrinsic :: iso_fortran_env, only : real32

  use constants_mod,            only : i_def, l_def, &
                                       default_halo_depth
  use function_space_mod,       only : function_space_type
  use mesh_mod,                 only : mesh_type
  use log_mod,                  only : log_event, LOG_LEVEL_ERROR
  use operator_parent_mod,      only : operator_parent_type, &
                                       operator_parent_proxy_type
  use signalling_value_mod,     only : get_signalling_value

  implicit none
  private

  !> Algorithm layer representation of an operator type
  !>
   type, public, extends(operator_parent_type) :: operator_real32_type
    private
    !> Allocatable array of kind real32 which holds the values of the operator
    real(kind=real32), allocatable :: local_stencil( :, :, : )
    !> Size of the outermost dimemsion of the local_stencil array, equal to
    !! ncell*nlayers
    integer(i_def) :: ncell_3d
  contains
    !> Initialise an operator object
    procedure, public :: initialise => operator_real32_initialiser
    !> Function to get a proxy with public pointers to the data in a
    !! operator_real32_type.
    procedure, public :: get_proxy => get_proxy_operator_real32
    !> Deep copy methods
    procedure, private :: operator_real32_type_deep_copy
    procedure, public  :: deep_copy => operator_real32_type_deep_copy
    !> Destroys object
    procedure, public :: operator_final
    ! Finalizer for the object
    final :: operator_real32_destructor
 end type operator_real32_type

 !> Psy layer representation of an operator
 !>
 !> This is an accessor class that allows access to the actual operator information
 !> with each element accessed via a public pointer.
 !>
 type, public, extends(operator_parent_proxy_type) :: operator_real32_proxy_type
    private
    !> Allocatable array of type real which holds the values of the operator
    real(kind=real32), public, pointer :: local_stencil( :, :, : )
    !> size of the outermost dimension
    integer(i_def), public :: ncell_3d
  contains
    final :: operator_real32_proxy_destructor
 end type operator_real32_proxy_type

!______end of type declarations_______________________________________________

contains

  !> Initialise an <code>operator_real32_type</code> object.
  !>
  !> @param [in] fs_from the function space that the operator maps from
  !> @param [in] fs_to the function space that the operator maps to
  !>
  subroutine operator_real32_initialiser( self, fs_to, fs_from )

    use, intrinsic :: ieee_arithmetic, only: IEEE_INVALID
    use, intrinsic :: ieee_exceptions, only: ieee_set_halting_mode, ieee_get_halting_mode

    implicit none
    class(operator_real32_type), target, intent(inout) :: self
    class(function_space_type),  target, intent(in)    :: fs_to
    class(function_space_type),  target, intent(in)    :: fs_from

    ! Defines whether to halt when invalid floating point numbers are experienced
    logical :: halt_mode
    ! The signalling number
    real(real32) :: signalling_value

    type(mesh_type), pointer :: mesh

    nullify( mesh )

    ! initialise the parent
    call self%operator_parent_initialiser( fs_to, fs_from )

    mesh => fs_from%get_mesh()
    self%ncell_3d = mesh%get_last_halo_cell(default_halo_depth) * fs_from%get_nlayers()
    nullify(mesh)
    ! allocate the array in memory
    if(allocated(self%local_stencil))deallocate(self%local_stencil)

    ! If run-time checking for NaNs is on then initialise data with NaN
    call ieee_get_halting_mode(IEEE_INVALID, halt_mode)

    if (halt_mode) then
      ! Temporarily turn off halting mode to safely set invalid value
      call ieee_set_halting_mode(IEEE_INVALID, .false.)

      signalling_value = get_signalling_value(signalling_value)
      allocate(self%local_stencil( self%ncell_3d, fs_to%get_ndf(), fs_from%get_ndf() ), &
               source=signalling_value)

      call ieee_set_halting_mode(IEEE_INVALID, .true.)
    else
      ! Normal operator allocation
      allocate(self%local_stencil(self%ncell_3d, fs_to%get_ndf(), fs_from%get_ndf() ) )
    end if

  end subroutine operator_real32_initialiser

  !> Function to create a proxy with access to the data in the operator_real32_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> operator_real32_type
  type(operator_real32_proxy_type ) function get_proxy_operator_real32(self)
    implicit none
    class(operator_real32_type), target, intent(in)  :: self

    ! Call the routine that initialises the proxy for data held in the parent
    call self%operator_parent_proxy_initialiser(get_proxy_operator_real32)

    get_proxy_operator_real32 % local_stencil  => self % local_stencil
    get_proxy_operator_real32 % ncell_3d       =  self%ncell_3d
  end function get_proxy_operator_real32

  !> @brief Copies the current operator into a new returned object
  !> @return The copies operator type
  function operator_real32_type_deep_copy(self) result(other)
    implicit none
    class(operator_real32_type), intent(in) :: self
    type(operator_real32_type) :: other
    ! make field_vector
    type( function_space_type ), pointer :: fs_to => null( )
    type( function_space_type ), pointer :: fs_from => null( )
    fs_to => self%get_fs_to()
    fs_from => self%get_fs_from()

    call other%initialise( fs_to, fs_from )
    other%local_stencil(:,:,:) = self%local_stencil(:,:,:)
  end function operator_real32_type_deep_copy

  !>@brief Destroys the operator type
  subroutine operator_final(self)
    implicit none
    class(operator_real32_type), intent(inout) :: self
    if(allocated(self%local_stencil)) then
       deallocate(self%local_stencil)
    end if
  end subroutine operator_final

  !>@brief Finalizer for the object type
  subroutine operator_real32_destructor(self)
    implicit none
    type(operator_real32_type), intent(inout) :: self
    call self%destroy_operator_parent()
    call self%operator_final()
  end subroutine operator_real32_destructor

  !>@brief Destroy the operator proxy
  subroutine operator_real32_proxy_destructor(self)
    implicit none
    type(operator_real32_proxy_type) :: self
    call self%destroy_operator_parent_proxy()
    nullify(self%local_stencil)
  end subroutine operator_real32_proxy_destructor

end module operator_real32_mod
