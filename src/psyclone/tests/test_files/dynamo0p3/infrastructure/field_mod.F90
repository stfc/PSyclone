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
! Modifications copyright (c) 2017, Science and Technology Facilities Council
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
!> @brief A module providing field related classes.
!>
!> @details Both a representation of a field which provides no access to the
!> underlying data (to be used in the algorithm layer) and an accessor class
!> (to be used in the Psy layer) are provided.


module field_mod

  use constants_mod,      only: r_def, r_double, i_def, l_def
  use function_space_mod, only: function_space_type
  use mesh_mod,           only: mesh_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> Algorithm layer representation of a field.
  !>
  !> Objects of this type hold all the data of the field privately.
  !> Unpacking the data is done via the proxy type accessed by the Psy layer
  !> alone.
  !>
  type, public :: field_type
    private

    !> Each field has a pointer to the function space on which it lives
    type( function_space_type ), pointer         :: vspace => null( )
    !> Pointer array of type real which holds the values of the field
    real(kind=r_def), pointer             :: data( : ) => null()
    !> The data for each field is held within an ESMF array component
    !type(ESMF_Array) :: esmf_array
    !> Flag that holds whether each depth of halo is clean or dirty (dirty=1)
    integer(kind=i_def), allocatable :: halo_dirty(:)

  contains

    !> Function to get a proxy with public pointers to the data in a
    !! field_type.
    procedure, public :: get_proxy

    procedure, public :: write_checksum

    !> function returns the enumerated integer for the functions_space on which
    !! the field lives
    procedure, public :: which_function_space

    !> Routine to return the mesh used by this field
    procedure         :: get_mesh
    procedure         :: get_mesh_id

    !> Overloaded assigment operator
    procedure         :: field_type_assign

    !> Override default assignment for field_type pairs.
    generic           :: assignment(=) => field_type_assign

  end type field_type

  !> Psy layer representation of a field.
  !>
  !> This is an accessor class that allows access to the actual field information
  !> with each element accessed via a public pointer.
  !>
  type, public :: field_proxy_type

    private

    !> An unused allocatable integer that prevents an intenal compiler error
    !> with the Gnu Fortran compiler. Adding an allocatable forces the compiler
    !> to accept that the object has a finaliser. It gets confused without it.
    !> This is a workaround for GCC bug id 61767 - when this bug is fixed, the
    !> integer can be removed. 
    integer(kind=i_def), allocatable :: dummy_for_gnu
    !> Each field has a pointer to the function space on which it lives
    type( function_space_type ), pointer, public :: vspace => null()
    !> Allocatable array of type real which holds the values of the field
    real(kind=r_def), public, pointer         :: data( : ) => null()
    !> pointer to array that holds halo dirtiness
    integer(kind=i_def), pointer :: halo_dirty(:) => null()

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
    !> ESMF have only implemented blocking reductions, so this
    !> subroutine currently returns without waiting.
    procedure reduction_finish

    !> Returns whether the halos at the given depth are dirty or clean
    !! @param[in] depth The depth at which to check the halos
    !! @return True if the halos are dirty or false if they are clean
    procedure is_dirty

    !> Flags all halos as being dirty
    procedure set_dirty

    !> Flags all the halos up the given depth as clean
    !! @param[in] depth The depth up to which to set the halo to clean
    procedure set_clean

  end type field_proxy_type

contains

  !> Function to create a proxy with access to the data in the field_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> field_type
  type(field_proxy_type ) function get_proxy(self)
    implicit none
    class(field_type), target, intent(in)  :: self

    get_proxy % vspace                 => self % vspace
    get_proxy % data                   => self % data
    get_proxy % halo_dirty             => self % halo_dirty

  end function get_proxy

  !> Assignment operator between field_type pairs.
  !>
  !> @param[out] dest   field_type lhs
  !> @param[in]  source field_type rhs
  subroutine field_type_assign(dest, source)

    implicit none
    class(field_type), intent(out)     :: dest
    class(field_type), intent(in)      :: source

    integer(i_def), allocatable :: global_dof_id(:)
    integer(i_def) :: rc
    integer(i_def) :: halo_start, halo_finish

    type (mesh_type), pointer   :: mesh => null()

    dest%data(:) = source%data(:)

  end subroutine field_type_assign

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !> Function to get mesh information from the field.
  !>
  !> @return Mesh object
  function get_mesh(self) result(mesh)

    implicit none

    class (field_type) :: self
    type (mesh_type), pointer :: mesh
    mesh => null()

    return
  end function get_mesh

  !> Function to get mesh id from the field.
  !>
  !> @return mesh_id
  function get_mesh_id(self) result(mesh_id)
    implicit none

    class (field_type) :: self
    integer(i_def) :: mesh_id

    mesh_id = 0

    return
  end function get_mesh_id

  

  !> Writes a checksum of this field to a file.
  !>
  !> \param[in] stream I/O unit number for output.
  !> \param[in] label Title to identify the field added to output.
  !>
  subroutine write_checksum( self, stream, label )

    implicit none

    class(field_type), intent(in) :: self
    integer,           intent(in) :: stream
    character(*),      intent(in) :: label

  end subroutine write_checksum

  function which_function_space(self) result(fs)
    implicit none
    class(field_type), intent(in) :: self
    integer(i_def) :: fs

    fs = 0
    return
  end function which_function_space

  !! Perform a blocking halo exchange operation on the field
  !!
  subroutine halo_exchange( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange

  !! Start a halo exchange operation on the field
  !!
  subroutine halo_exchange_start( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_start

  !! Wait for a halo exchange to complete
  !!
  subroutine halo_exchange_finish( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_finish

  !! Start performing a global sum operation on the field
  !!
  function get_sum(self) result (answer)

    class(field_proxy_type), intent(in) :: self
    real(r_def) :: answer


    answer = 0.0_r_def
  end function get_sum

  !! Start the calculation of the global minimum of the field
  !!
  function get_min(self) result (answer)

    class(field_proxy_type), intent(in) :: self
    real(r_def) :: answer

    answer = 0.0_r_def
  end function get_min

  !! Start the calculation of the global maximum of the field
  !!
  function get_max(self) result (answer)

    class(field_proxy_type), intent(in) :: self
    real(r_def) :: answer

    answer = 0.0_r_def
  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, ESMF has only implemented blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)

    class(field_proxy_type), intent(in) :: self

  end subroutine reduction_finish

  ! Returns true if a halo depth is dirty
  ! @param[in] depth The depth of halo to inquire about
  function is_dirty(self, depth) result(dirtiness)

    implicit none

    class(field_proxy_type), intent(in) :: self
    integer(i_def), intent(in) :: depth
    logical(l_def) :: dirtiness

    dirtiness = .false.

  end function is_dirty

  ! Sets a halo depth to be flagged as dirty
  ! @param[in] depth The depth up to which to make the halo dirty
  subroutine set_dirty( self )

    implicit none

    class(field_proxy_type), intent(inout) :: self

  end subroutine set_dirty

  ! Sets the halos up to depth to be flagged as clean
  ! @param[in] depth The depth up to which to make the halo clean
  subroutine set_clean(self, depth)

    implicit none

    class(field_proxy_type), intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine set_clean

end module field_mod
