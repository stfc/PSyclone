!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2022-2023, Science and Technology Facilities
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
! Modified by: I. Kavcic, Met Office,
!              J. Henrichs, Bureau of Meteorology
!
!> @brief A module providing 32-bit real field related classes.
!>
!> @details This is a version of a field object that can hold 32-bit real data
!> values. It contains both a representation of a 32-bit real field which
!> provides no access to the underlying data (to be used in the algorithm layer)
!> and an accessor class (to be used in the Psy layer).

module field_r32_mod

  use, intrinsic :: iso_fortran_env, only : real32

  use constants_mod,      only: i_def, i_halo_index, l_def, &
                                str_def, real_type
  use field_parent_mod,   only: field_parent_type, &
                                field_parent_proxy_type, &
                                write_interface, read_interface, &
                                checkpoint_write_interface, &
                                checkpoint_read_interface
  use function_space_mod, only: function_space_type

  use log_mod,            only: log_event, &
                                log_scratch_space, &
                                log_level, &
                                LOG_LEVEL_INFO, &
                                LOG_LEVEL_ERROR
  use scalar_r32_mod,     only: scalar_r32_type

  use pure_abstract_field_mod, &
                          only: pure_abstract_field_type

  implicit none

  private

! Public types

!______field_r32_type_________________________________________________________

  !> Algorithm layer representation of a 32-bit real field.
  !>
  !> Objects of this type hold all the data of the field privately.
  !> Unpacking the data is done via the proxy type accessed by the Psy layer
  !> alone.
  !>
  type, extends(field_parent_type), public :: field_r32_type
    private

    !> The 32-bit floating point values of the field
    real(kind=real32), allocatable :: data( : )
    !> Enable field to point to bespoke data provided by application
    !> instead of having allocated data
    real(kind=real32), pointer :: override_data( : )

    ! IO interface procedure pointers

    procedure(write_interface), nopass, pointer            :: write_method => null()
    procedure(read_interface), nopass, pointer             :: read_method => null()
    procedure(checkpoint_write_interface), nopass, pointer :: checkpoint_write_method => null()
    procedure(checkpoint_read_interface), nopass, pointer  :: checkpoint_read_method => null()

  contains
    !> Initialiser for a field. May only be called once.
    procedure, public :: initialise => field_initialiser

    ! Routine to return a deep copy of a field including all its data
    procedure, public :: copy_field_serial

    ! Routine to return a deep, but empty copy of a field
    procedure, public :: copy_field_properties

    !> Function to get a proxy with public pointers to the data in a
    !! field_type.
    procedure, public :: get_proxy

    ! Logging procedures
    procedure, public :: log_field
    procedure, public :: log_dofs
    procedure, public :: log_minmax
    procedure, public :: log_absmax

    !> Return global min and max of field
    procedure, public :: field_minmax

    !> Setter for the field write method
    procedure, public :: set_write_behaviour

    !> Getter for the field write method
    procedure, public :: get_write_behaviour

    !> Setter for the read method
    procedure, public :: set_read_behaviour

    !> Getter for the read method
    procedure, public :: get_read_behaviour

    !> Setter for the checkpoint method
    procedure, public :: set_checkpoint_write_behaviour

    !> Setter for the restart method
    procedure, public :: set_checkpoint_read_behaviour

    !> Routine to return whether field can be checkpointed
    procedure, public :: can_checkpoint

    !> Routine to return whether field can be written
    procedure, public :: can_write

    !> Routine to return whether field can be read
    procedure, public :: can_read

    !> Routine to write field
    procedure         :: write_field

    !> Routine to read field
    procedure         :: read_field

    !> Routine to read a checkpoint netCDF file
    procedure         :: read_checkpoint

    !> Routine to write a checkpoint netCDF file
    procedure         :: write_checkpoint

    !> Overloaded assignment operator
    procedure         :: field_type_assign

    !> Routine to destroy field_type
    procedure         :: field_final

    !> Checks if field has been initialised
    procedure, public :: is_initialised

    !> Finalizers for scalar and arrays of field_type objects
    final             :: field_r32_destructor_scalar, &
                         field_r32_destructor_array1d, &
                         field_r32_destructor_array2d

    !> Override default assignment for field_type pairs.
    generic           :: assignment(=) => field_type_assign

  end type field_r32_type

!______field_pointer_type_______________________________________________________

!> a class to hold a pointer to a field in an object that is a child of
!> the pure abstract field class

  type, extends(pure_abstract_field_type), public :: field_r32_pointer_type
    private
    !> A pointer to a field
    type(field_r32_type), pointer, public :: field_ptr
  contains
    !> Initialiser for a field pointer. May only be called once.
    procedure, public :: initialise => field_r32_pointer_initialiser
    !> Finaliser for a field pointer object
    final :: field_r32_pointer_destructor
  end type field_r32_pointer_type

!______field_r32_proxy_type_______________________________________________________

  !> Psy layer representation of a field.
  !>
  !> This is an accessor class that allows access to the actual field
  !> information with each element accessed via a public pointer.
  !>
  type, extends(field_parent_proxy_type), public :: field_r32_proxy_type

    private

    !> Allocatable array of type real32 which holds the values of the field
    real(kind=real32), public, pointer :: data( : ) => null()

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

    !> Perform a global sum operation on the field
    !> @return The global sum of the field values over all ranks
    procedure, public :: get_sum

    !> Calculate the global minimum of the field
    !> @return The minimum of the field values over all ranks
    procedure, public :: get_min

    !> Calculate the global maximum of the field
    !> @return The maximum of the field values over all ranks
    procedure, public :: get_max

    !> Wait (i.e. block) until all current non-blocking reductions
    !> (sum, max, min) are complete.
    !>
    !> We presently have only blocking reductions, so this
    !> subroutine currently returns without waiting.
    procedure, public :: reduction_finish

  end type field_r32_proxy_type

!______end of type declarations_______________________________________________

contains

!______field_r32_type procedures______________________________________________

  !> Function to create a proxy with access to the data in the field_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> field_type
  type(field_r32_proxy_type) function get_proxy(self)
    implicit none
    class(field_r32_type), target, intent(in)  :: self

    ! Call the routine that initialises the proxy for data held in the parent
    call self%field_parent_proxy_initialiser(get_proxy)

    if (allocated(self%data))then
      get_proxy%data => self%data
    else
      ! Fields can alternatively point to bespoke data
      get_proxy%data => self%override_data
    end if

  end function get_proxy

  !> Initialise a <code>field_r32_type</code> object.
  !>
  !> @param [in] vector_space the function space that the field lives on
  !> @param [in] name The name of the field. 'none' is a reserved name
  !> @param [in] ndata_first Whether mutlidata fields have data ordered by
  !>                         the multidata dimension first
  !> @param [in] override_data Optional alternative data that can be attached to field
  !>
  subroutine field_initialiser(self, &
                               vector_space, &
                               name, &
                               ndata_first, &
                               override_data)

    use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_SIGNALING_NAN, IEEE_INVALID
    use, intrinsic :: ieee_exceptions, only: ieee_set_halting_mode, ieee_get_halting_mode

    implicit none

    class(field_r32_type), intent(inout)           :: self
    type(function_space_type), pointer, intent(in) :: vector_space
    character(*), optional, intent(in)             :: name
    logical,      optional, intent(in)             :: ndata_first
    real(real32), target, optional, intent(in)     :: override_data( : )

    character(str_def) :: local_name

    ! Defines whether to halt when invalid floating point numbers are experienced
    logical :: halt_mode
    ! To be set to an invalid floating point number
    real(real32) :: NaN

    if ( present(name) ) then
      local_name = name
    else
      local_name = 'none'
    end if

    ! In case the field is already initialised, destruct it ready for
    ! re-initialisation
    call field_r32_destructor_scalar(self)

    call self%field_parent_initialiser(vector_space, &
                                       name=local_name, &
                                       fortran_type=real_type, &
                                       fortran_kind=real32, &
                                       ndata_first=ndata_first)

    ! Associate data with the field
    if (present(override_data))then
      ! Override normal field data if an alternative was provided
      self%override_data => override_data
    else
      ! Create space for holding field data

      ! If run-time checking for NaNs is on then initialise data with NaN
      call ieee_get_halting_mode(IEEE_INVALID, halt_mode)

      if (halt_mode) then
        ! Temporarily turn off halting mode to safely set invalid value
        call ieee_set_halting_mode(IEEE_INVALID, .false.)

        allocate( self%data(vector_space%get_last_dof_halo()), &
             source=ieee_value(NaN, IEEE_SIGNALING_NAN))

        call ieee_set_halting_mode(IEEE_INVALID, .true.)
      else
        ! Normal field allocation
        allocate( self%data(vector_space%get_last_dof_halo()))
      end if

      self%override_data => null()

    end if

  end subroutine field_initialiser

  !> Initialise a 32-bit real field pointer
  !>
  !> @param [in] field_r32_ptr A pointer to the field that is to be
  !>                           stored as a reference
  subroutine field_r32_pointer_initialiser(self, field_ptr)
    implicit none
    class(field_r32_pointer_type) :: self
    type(field_r32_type), pointer :: field_ptr
    self%field_ptr => field_ptr
  end subroutine field_r32_pointer_initialiser

  ! Finaliser for a 32-bit real field pointer
  !
  ! The following finaliser doesn't do anything. Without it, the Gnu compiler
  ! tries to create its own, but only ends up producing an Internal Compiler
  ! Error, so it is included here to prevent that.
  subroutine field_r32_pointer_destructor(self)
    implicit none
    type(field_r32_pointer_type), intent(inout) :: self
  end subroutine field_r32_pointer_destructor

  !> Create a new field that inherits the properties of the source field and
  !> has a copy of all the field data from the source field.
  !>
  !> @param[out] dest   field object into which the copy will be made
  !> @param[in]  name   An optional argument that provides an identifying name
  subroutine copy_field_serial(self, dest, name)

    implicit none
    class(field_r32_type), target, intent(in)  :: self
    type(field_r32_type),  target, intent(out) :: dest
    character(*), optional, intent(in)     :: name

    if ( .not. allocated(self%data) ) then
      call log_event( 'Error: copy_field_serial: Copied field must have field data', &
           LOG_LEVEL_ERROR )
    end if

    if (present(name)) then
      call self%copy_field_properties(dest, name)
    else
      call self%copy_field_properties(dest)
    end if

    dest%data(:) = self%data(:)
    call self%copy_field_parent(dest)

  end subroutine copy_field_serial

  !> Create a new empty field that inherits the properties of the source field.
  !>
  !> @param[out] dest   field object into which the copy will be made
  !> @param[in]  name   An optional argument that provides an identifying name
  subroutine copy_field_properties(self, dest, name)

    implicit none
    class(field_r32_type), target, intent(in)  :: self
    type(field_r32_type),  target, intent(out) :: dest
    character(*), optional, intent(in)     :: name

    type(function_space_type), pointer :: function_space => null()

    ! Get function space from parent
    function_space => self%get_function_space()

    if (present(name)) then
      call dest%initialise(vector_space = function_space,      &
                           name = name)
    else
      call dest%initialise(vector_space = function_space,      &
                           name = self%get_name())
    end if

    dest%write_method => self%write_method
    dest%read_method => self%read_method
    dest%checkpoint_write_method => self%checkpoint_write_method
    dest%checkpoint_read_method => self%checkpoint_read_method

  end subroutine copy_field_properties

  !> DEPRECATED: Assignment operator between field_type pairs. Currently, this
  !> routine generates a (hopefully) useful message, then forces an error
  !>
  !> @param[out] dest   field_type lhs
  !> @param[in]  source field_type rhs
  subroutine field_type_assign(dest, source)

    implicit none
    class(field_r32_type), intent(in)  :: source
    class(field_r32_type), intent(out) :: dest

    write(log_scratch_space,'(A,A)')&
              '"field2=field1" syntax no longer supported. '// &
              'Use "setval_X(field2, field1)". Field: ', &
              source%get_name()
    call log_event(log_scratch_space,LOG_LEVEL_ERROR )

  end subroutine field_type_assign

  !> Destroy a scalar field_type instance.
  subroutine field_final(self)

    implicit none

    class(field_r32_type), intent(inout)    :: self

    call self%field_parent_final()

    if(allocated(self%data)) then
      deallocate(self%data)
    end if

    nullify( self%write_method,             &
             self%read_method,              &
             self%checkpoint_write_method,  &
             self%checkpoint_read_method )

  end subroutine field_final

  !> Return a logical indicating whether the field has been initialised
  function is_initialised(self) result(initialised)
    implicit none
    class(field_r32_type), intent(in) :: self
    logical(l_def)                :: initialised

    initialised = allocated(self%data)

  end function is_initialised

  !> Finalizer for a scalar <code>field_type</code> instance.
  subroutine field_r32_destructor_scalar(self)

    implicit none

    type(field_r32_type), intent(inout)    :: self

    call self%field_final()

  end subroutine field_r32_destructor_scalar

  !> Finalizer for a 1d array of <code>field_type</code> instances.
  subroutine field_r32_destructor_array1d(self)

    implicit none

    type(field_r32_type), intent(inout)    :: self(:)
    integer(i_def) :: i

    do i=lbound(self,1), ubound(self,1)
      call self(i)%field_final()
    end do

  end subroutine field_r32_destructor_array1d

  !> Finalizer for a 2d array of <code>field_type</code> instances.
  subroutine field_r32_destructor_array2d(self)

    implicit none

    type(field_r32_type), intent(inout)    :: self(:,:)
    integer(i_def) :: i,j

    do i=lbound(self,1), ubound(self,1)
      do j=lbound(self,2), ubound(self,2)
        call self(i,j)%field_final()
      end do
    end do

  end subroutine field_r32_destructor_array2d

  !> Setter for field write behaviour
  !> @param[in,out]  self  field_type
  !> @param [in] write_behaviour - pointer to procedure implementing write method
  subroutine set_write_behaviour(self, write_behaviour)
    implicit none
    class(field_r32_type), intent(inout)            :: self
    procedure(write_interface), pointer, intent(in) :: write_behaviour
    self%write_method => write_behaviour
  end subroutine set_write_behaviour

  !> Getter to get pointer to field write behaviour
  !> @param[in]  self  field_type
  !> @param [in] write_behaviour -
  !>             pointer to procedure implementing write method
  !> @return pointer to procedure for field write behaviour
  subroutine get_write_behaviour(self, write_behaviour)

    implicit none

    class(field_r32_type), intent(in)                  :: self
    procedure(write_interface), pointer, intent(inout) :: write_behaviour

    write_behaviour => self%write_method

    return
  end subroutine get_write_behaviour

  !> Setter for read behaviour
  !> @param [in] read_behaviour - pointer to procedure implementing read method
  subroutine set_read_behaviour(self, read_behaviour)
    implicit none
    class(field_r32_type), intent(inout)           :: self
    procedure(read_interface), pointer, intent(in) :: read_behaviour
    self%read_method => read_behaviour
  end subroutine set_read_behaviour

  !> Getter to get pointer to read behaviour
  !> @param[in,out] read_behaviour -
  !>                pointer to procedure implementing read method
  !> @return pointer to procedure for read behaviour
  subroutine get_read_behaviour(self, read_behaviour)

    implicit none

    class(field_r32_type), intent(in)                 :: self
    procedure(read_interface), pointer, intent(inout) :: read_behaviour

    read_behaviour => self%read_method

    return
  end subroutine get_read_behaviour


  !> Setter for checkpoint write behaviour
  !>
  !> @param [in] checkpoint_write_behaviour -
  !>             pointer to procedure implementing checkpoint write method
  subroutine set_checkpoint_write_behaviour(self, checkpoint_write_behaviour)
    implicit none
    class(field_r32_type), intent(inout)              :: self
    procedure(checkpoint_write_interface), pointer, intent(in)   :: checkpoint_write_behaviour
    self%checkpoint_write_method => checkpoint_write_behaviour
  end subroutine set_checkpoint_write_behaviour

  !> Setter for checkpoint read behaviour
  !>
  !> @param [in] checkpoint_read_behaviour -
  !>             pointer to procedure implementing checkpoint read method
  subroutine set_checkpoint_read_behaviour(self, checkpoint_read_behaviour)
    implicit none
    class(field_r32_type), intent(inout)              :: self
    procedure(checkpoint_read_interface), pointer, intent(in)   :: checkpoint_read_behaviour
    self%checkpoint_read_method => checkpoint_read_behaviour
  end subroutine set_checkpoint_read_behaviour

  !> Returns whether field can be checkpointed
  !>
  !> @return .true. or .false.
  function can_checkpoint(self) result(checkpointable)

    implicit none

    class(field_r32_type), intent(in) :: self
    logical(l_def) :: checkpointable

    if (associated(self%checkpoint_write_method) .and. &
       associated(self%checkpoint_read_method)) then
      checkpointable = .true.
    else
      checkpointable = .false.
    end if

  end function can_checkpoint

  !> Returns whether field can be written
  !>
  !> @return .true. or .false.
  function can_write(self) result(writeable)

    implicit none

    class(field_r32_type), intent(in) :: self
    logical(l_def) :: writeable

    writeable = associated(self%write_method)

  end function can_write

  !> Returns whether field can be read
  !>
  !> @return .true. or .false.
  function can_read(self) result(readable)

    implicit none

    class(field_r32_type), intent(in) :: self
    logical(l_def) :: readable

    readable = associated(self%read_method)

  end function can_read

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !> Sends field contents to the log.
  !!
  !! @param[in] dump_level The level to use when sending the dump to the log.
  !! @param[in] label A title added to the log before the data is written out
  !>
  subroutine log_field( self, dump_level, label )

    implicit none

    class( field_r32_type ), target, intent(in) :: self
    integer(i_def),                  intent(in) :: dump_level
    character( * ),                  intent(in) :: label

    type(function_space_type), pointer :: function_space => null()
    integer(i_def)                     :: cell
    integer(i_def)                     :: layer
    integer(i_def)                     :: df
    integer(i_def),            pointer :: map(:) => null()

    ! Get function space from parent
    function_space => self%get_function_space()

    ! If we are not going to write the field to the log then do
    ! not write the field to the scratch space.
    if ( dump_level < log_level() ) return

    write( log_scratch_space, '( A, A)' ) trim( label ), " =["
    call log_event( log_scratch_space, dump_level )

    do cell=1,function_space%get_ncell()
      map => function_space%get_cell_dofmap( cell )
      do df=1,function_space%get_ndf()
        do layer=0,function_space%get_nlayers()-1
          write( log_scratch_space, '( I6, I6, I6, E16.8 )' ) &
              cell, df, layer+1, self%data( map( df ) + layer )
          call log_event( log_scratch_space, dump_level )
        end do
      end do
    end do

    call log_event( '];', dump_level )
    return

  end subroutine log_field

  !> Sends the field contents to the log
  !!
  !! @param[in] log_level The level to use for logging.
  !! @param[in] label A title added to the log before the data is written out
  !!
  subroutine log_dofs( self, log_level, label )

    implicit none

    class( field_r32_type ), target, intent(in) :: self
    integer(i_def),                  intent(in) :: log_level
    character( * ),                  intent(in) :: label

    type(function_space_type), pointer :: function_space => null()
    integer(i_def) :: df

    ! Get function space from parent
    function_space => self%get_function_space()

    call log_event( label, log_level )

    do df=1,function_space%get_undf()
      write( log_scratch_space, '( I6, E16.8 )' ) df,self%data( df )
      call log_event( log_scratch_space, log_level )
    end do

  end subroutine log_dofs

  !> Sends the min/max of a field to the log
  !!
  !! @param[in] log_lev The level to use for logging.
  !! @param[in] label A title added to the log before the data is written out
  !!
  subroutine log_minmax( self, log_lev, label )

    implicit none

    class( field_r32_type ), target, intent(in) :: self
    integer(i_def),                  intent(in) :: log_lev
    character( * ),                  intent(in) :: label

    type(function_space_type),      pointer :: function_space => null()
    integer(i_def)                          :: undf
    type(scalar_r32_type)                   :: fmin, fmax

    ! Get function space from parent
    function_space => self%get_function_space()

    ! If we aren't going to log the min and max then we don't need to
    ! do any further work here.
    if ( log_lev < log_level() ) return

    ! Ensure that the local version of this field is up-to-date if we're
    ! running on GPU.
    ! TODO #1727 this is just a quick fix. Ideally the whole (stub)
    ! infrastructure needs extending, probably following the way dl_esm_inf
    ! does it (abstract interfaces for subroutines that allow the user to
    ! implement appropriate push/pull functionality for the target
    ! programming model).
    !$acc update if_present host(self%data)

    undf = function_space%get_last_dof_owned()
    ! TODO this is really computation that ought to be captured in a kernel
    ! of some sort (perhaps a Builtin).
    fmin%value = minval( self%data(1:undf) )
    fmax%value = maxval( self%data(1:undf) )

    write( log_scratch_space, '( A, A, A, 2E16.8 )' ) &
         "Min/max ", trim( label ),                   &
         " = ", fmin%get_min(), fmax%get_max()
    call log_event( log_scratch_space, log_lev )

  end subroutine log_minmax

  !> Sends the max of the absolute value of field to the log
  !!
  !! @param[in] log_lev The level to use for logging.
  !! @param[in] label A title added to the log before the data is written out.
  !!
  subroutine log_absmax( self, log_lev, label )

    implicit none

    class( field_r32_type ), target, intent(in) :: self
    integer(i_def),                  intent(in) :: log_lev
    character( * ),                  intent(in) :: label

    type(function_space_type),      pointer :: function_space => null()
    integer(i_def)                          :: undf
    type(scalar_r32_type)                   :: fmax

    ! Get function space from parent
    function_space => self%get_function_space()

    ! If we aren't going to log the abs max then we don't need to
    ! do any further work here.
    if ( log_lev < log_level() ) return

    undf = function_space%get_last_dof_owned()
    fmax%value = maxval( abs(self%data(1:undf)) )

    write( log_scratch_space, '( A, A, E16.8 )' ) &
         trim( label ), " = ", fmax%get_max()
    call log_event( log_scratch_space, log_lev )

  end subroutine log_absmax

  !> @brief Returns the min/max of a field.
  !> @param[out] fmin Minimum value of the field
  !> @param[out] fmax Maximum value of the field
  !>
  !> This routine should be PSy built-in (intrinsic) function.
  !> PSyclone issue #489
  !>
  subroutine field_minmax( self, fmin, fmax )

    implicit none

    class( field_r32_type ), target, intent(in)  :: self
    real(real32),                    intent(out) :: fmin, fmax

    type(function_space_type),      pointer :: function_space => null()
    integer(i_def)                          :: undf
    type(scalar_r32_type)                   :: fmin1, fmax1

    ! Get function space from parent
    function_space => self%get_function_space()

    undf = function_space%get_last_dof_owned()
    fmin1%value = minval( self%data(1:undf) )
    fmax1%value = maxval( self%data(1:undf) )

    fmin = fmin1%get_min()
    fmax = fmax1%get_max()

  end subroutine field_minmax

  !> Calls the underlying IO implementation for writing a field
  !> throws an error if this has not been set
  !> @param [in] field_name - field name / id to write
  subroutine write_field(this, field_name)

    implicit none

    class(field_r32_type), intent(in) :: this
    character(len=*),      intent(in) :: field_name

    if (associated(this%write_method)) then

      call this%write_method(trim(field_name), this%get_proxy())

    else

      call log_event( 'Error trying to write field '// trim(field_name) // &
                      ', write_method not set up', LOG_LEVEL_ERROR )
    end if

  end subroutine write_field

  !> Calls the underlying IO implementation for reading into the field
  !> throws an error if this has not been set
  !> @param [in] field_name - field name / id to read
  subroutine read_field( self, field_name)

    implicit none

    class( field_r32_type ),  target, intent( inout ) :: self
    character(len=*),                 intent(in)      :: field_name


    type( field_r32_proxy_type )                      :: tmp_proxy

    if (associated(self%read_method)) then

      tmp_proxy = self%get_proxy()

      call self%read_method(trim(field_name), tmp_proxy)

      ! Set halos dirty here as for parallel read we only read in data for owned
      ! dofs and the halos will not be set

      call tmp_proxy%set_dirty()

    else

      call log_event( 'Error trying to read into field '// trim(field_name) // &
                      ', read_method not set up', LOG_LEVEL_ERROR )
    end if

  end subroutine read_field

  !> Reads a checkpoint file into the field
  !> @param [in] field_name - field name / id to read
  !> @param [in] file_name - file name to read from
  subroutine read_checkpoint( self, field_name, file_name)

    implicit none

    class( field_r32_type ), target, intent( inout ) :: self
    character(len=*),                intent(in)      :: field_name
    character(len=*),                intent(in)      :: file_name


    type( field_r32_proxy_type )                     :: tmp_proxy


    if (associated(self%checkpoint_read_method)) then

      tmp_proxy = self%get_proxy()

      call self%checkpoint_read_method(trim(field_name), trim(file_name), tmp_proxy)

      ! Set halos dirty here as for parallel read we only read in data for owned
      ! dofs and the halos will not be set

      call tmp_proxy%set_dirty()

    else

      call log_event( 'Error trying to read checkpoint for field '// trim(field_name) // &
                      ', checkpoint_read_method not set up', LOG_LEVEL_ERROR )
    end if

  end subroutine read_checkpoint

  !> Writes a checkpoint file
  !> @param [in] field_name - field name / id to write
  !> @param [in] file_name - file name to write to
  subroutine write_checkpoint( self, field_name, file_name )

    implicit none

    class( field_r32_type ), target, intent( inout ) :: self
    character(len=*),                intent(in)      :: field_name
    character(len=*),                intent(in)      :: file_name

    if (associated(self%checkpoint_write_method)) then

      call self%checkpoint_write_method(trim(field_name), trim(file_name), self%get_proxy())

    else

      call log_event( 'Error trying to write checkpoint for field '// trim(field_name) // &
                      ', checkpoint_write_method not set up', LOG_LEVEL_ERROR )
    end if

  end subroutine write_checkpoint

  !! Perform a blocking halo exchange operation on the field
  !!
  subroutine halo_exchange( self, depth )

    use count_mod,       only : halo_calls
    implicit none

    class( field_r32_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange

  !! Start an asynchronous halo exchange operation on the field
  !!
  subroutine halo_exchange_start( self, depth )

    implicit none

    class( field_r32_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_start

  !! Wait for an asynchronous halo exchange to complete
  !!
  subroutine halo_exchange_finish( self, depth )

    use count_mod,       only : halo_calls
    implicit none

    class( field_r32_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_finish

  !! Start performing a global sum operation on the field
  !!
  function get_sum(self) result (answer)

    use mpi_mod, only: global_sum
    implicit none

    class(field_r32_proxy_type), intent(in) :: self

    real(real32) :: l_sum
    real(real32) :: answer

    integer(i_def) :: i

    ! Generate local sum
    l_sum = 0.0_real32
    do i = 1, self%vspace%get_last_dof_owned()
      l_sum = l_sum + self%data(i)
    end do

    call global_sum( l_sum, answer )

  end function get_sum

  !! Start the calculation of the global minimum of the field
  !!
  function get_min(self) result (answer)

    use mpi_mod, only: global_min
    implicit none

    class(field_r32_proxy_type), intent(in) :: self

    real(real32) :: l_min
    real(real32) :: answer

    integer(i_def) :: i

    ! Generate local min
    l_min = self%data(1)
    do i = 2, self%vspace%get_last_dof_owned()
      if( self%data(i) < l_min ) l_min = self%data(i)
    end do

    call global_min( l_min, answer )

  end function get_min

  !! Start the calculation of the global maximum of the field
  !!
  function get_max(self) result (answer)

    use mpi_mod, only: global_max
    implicit none

    class(field_r32_proxy_type), intent(in) :: self

    real(real32) :: l_max
    real(real32) :: answer

    integer(i_def) :: i

    ! Generate local max
    l_max = self%data(1)
    do i = 2, self%vspace%get_last_dof_owned()
      if( self%data(i) > l_max ) l_max = self%data(i)
    end do

    call global_max( l_max, answer )

  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! We presently have only blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)

    implicit none

    class(field_r32_proxy_type), intent(in) :: self

    logical(l_def) :: is_dirty_tmp

    is_dirty_tmp=self%is_dirty(1)   ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.

  end subroutine reduction_finish

end module field_r32_mod
