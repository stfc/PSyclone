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
module field_mod

  use constants_mod,      only: r_def, r_double, i_def, l_def
  use function_space_mod, only: function_space_type
  use mesh_mod,           only: mesh_type

  implicit none

  private

  type, public :: field_type
    private

    type( function_space_type ), pointer  :: vspace => null( )
    real(kind=r_def), pointer             :: data( : ) => null()
    integer(kind=i_def), allocatable :: halo_dirty(:)

  contains

    procedure, public :: get_proxy
    procedure, public :: write_checksum
    procedure, public :: which_function_space

    procedure         :: get_mesh
    procedure         :: get_mesh_id
    procedure         :: field_type_assign
    generic           :: assignment(=) => field_type_assign

  end type field_type


  type, public :: field_proxy_type

    private

    integer(kind=i_def), allocatable :: dummy_for_gnu
    type( function_space_type ), pointer, public :: vspace => null()
    real(kind=r_def), public, pointer         :: data( : ) => null()
    integer(kind=i_def), pointer :: halo_dirty(:) => null()

  contains

    procedure, public :: halo_exchange
    procedure, public :: halo_exchange_start

    procedure, public :: halo_exchange_finish

    procedure, public :: get_sum
    procedure, public :: get_min
    procedure, public :: get_max
    procedure reduction_finish
    procedure is_dirty
    procedure set_dirty
    procedure set_clean

  end type field_proxy_type

contains

  type(field_proxy_type ) function get_proxy(self)
    implicit none
    class(field_type), target, intent(in)  :: self

    get_proxy % vspace                 => self % vspace
    get_proxy % data                   => self % data
    get_proxy % halo_dirty             => self % halo_dirty

  end function get_proxy

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

  function get_mesh(self) result(mesh)

    implicit none

    class (field_type) :: self
    type (mesh_type), pointer :: mesh
    mesh => null()

    return
  end function get_mesh

  function get_mesh_id(self) result(mesh_id)
    implicit none

    class (field_type) :: self
    integer(i_def) :: mesh_id

    mesh_id = 0

    return
  end function get_mesh_id

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

  subroutine halo_exchange( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange

  subroutine halo_exchange_start( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_start

  subroutine halo_exchange_finish( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine halo_exchange_finish

  function get_sum(self) result (answer)

    class(field_proxy_type), intent(in) :: self
    real(r_def) :: answer

    answer = 0.0_r_def
  end function get_sum

  function get_min(self) result (answer)

    class(field_proxy_type), intent(in) :: self
    real(r_def) :: answer

    answer = 0.0_r_def
  end function get_min

  function get_max(self) result (answer)

    class(field_proxy_type), intent(in) :: self
    real(r_def) :: answer

    answer = 0.0_r_def
  end function get_max

  subroutine reduction_finish(self)

    class(field_proxy_type), intent(in) :: self

  end subroutine reduction_finish

  function is_dirty(self, depth) result(dirtiness)

    implicit none
    class(field_proxy_type), intent(in) :: self
    integer(i_def), intent(in) :: depth
    logical(l_def) :: dirtiness

    dirtiness = .false.

  end function is_dirty

  subroutine set_dirty( self )

    implicit none
    class(field_proxy_type), intent(inout) :: self

  end subroutine set_dirty

  subroutine set_clean(self, depth)

    implicit none
    class(field_proxy_type), intent(inout) :: self
    integer(i_def), intent(in) :: depth

  end subroutine set_clean

end module field_mod
