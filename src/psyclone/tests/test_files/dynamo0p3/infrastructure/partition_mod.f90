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

module partition_mod

use constants_mod,   only: i_def, r_def, l_def

implicit none

private

type, public :: partition_type
  private
  integer(i_def)              :: local_rank
  integer(i_def)              :: total_ranks
  integer(i_def), allocatable :: global_cell_id( : )
  integer(i_def), allocatable :: cell_owner( : )
  integer(i_def), allocatable :: num_inner( : )
  integer(i_def), allocatable :: last_inner_cell( : )
  integer(i_def)              :: inner_depth
  integer(i_def)              :: num_edge
  integer(i_def)              :: last_edge_cell
  integer(i_def), allocatable :: num_halo( : )
  integer(i_def), allocatable :: last_halo_cell( : )
  integer(i_def)              :: halo_depth
  integer(i_def)              :: num_ghost
  integer(i_def)              :: global_num_cells

contains
  procedure, public :: get_num_cells_in_layer
  procedure, public :: get_inner_depth
  procedure, public :: get_num_cells_inner
  procedure, public :: get_last_inner_cell
  procedure, public :: get_num_cells_edge
  procedure, public :: get_last_edge_cell
  procedure, public :: get_halo_depth
  procedure, public :: get_num_cells_halo
  procedure, public :: get_last_halo_cell
  procedure, public :: get_num_cells_ghost
  procedure, public :: get_local_rank
  procedure, public :: get_total_ranks
  procedure, public :: get_cell_owner
  procedure, public :: get_gid_from_lid
  procedure, public :: get_lid_from_gid
  procedure, public :: partition_type_assign
  generic :: assignment(=) => partition_type_assign

end type partition_type

contains

subroutine partition_type_assign(dest, source)
  class(partition_type), intent(out)   :: dest
  class(partition_type), intent(in)    :: source

  dest%local_rank = 0
  dest%total_ranks = 0
  dest%halo_depth = 0

end subroutine partition_type_assign

function get_num_cells_in_layer( self ) result ( num_cells )
  implicit none

  class(partition_type), intent(in) :: self

  integer(i_def) :: num_cells

  num_cells = 0
end function get_num_cells_in_layer

function get_inner_depth( self ) result ( inner_depth )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: inner_depth

  inner_depth = 0
end function get_inner_depth

function get_num_cells_inner( self, depth ) result ( inner_cells )
  implicit none

  class(partition_type), intent(in) :: self

  integer(i_def), intent(in) :: depth
  integer(i_def)             :: inner_cells

  inner_cells = 0
end function get_num_cells_inner

function get_last_inner_cell( self, depth ) result ( last_inner_cell )
  implicit none

  class(partition_type), intent(in) :: self

  integer(i_def), intent(in) :: depth
  integer(i_def)             :: last_inner_cell

  last_inner_cell = 0
end function get_last_inner_cell

function get_num_cells_edge( self ) result ( edge_cells )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: edge_cells

  edge_cells = 0
end function get_num_cells_edge

function get_last_edge_cell( self ) result ( last_edge_cell )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: last_edge_cell

  last_edge_cell = 0
end function get_last_edge_cell

function get_halo_depth( self ) result ( halo_depth )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: halo_depth

  halo_depth = 0
end function get_halo_depth

function get_num_cells_halo( self, depth ) result ( halo_cells )
  implicit none

  class(partition_type), intent(in) :: self

  integer(i_def), intent(in) :: depth
  integer(i_def)             :: halo_cells

  halo_cells = 0
end function get_num_cells_halo

function get_last_halo_cell( self, depth ) result ( last_halo_cell )
  implicit none

  class(partition_type), intent(in) :: self

  integer(i_def), intent(in) :: depth
  integer(i_def)             :: last_halo_cell

  last_halo_cell = 0
end function get_last_halo_cell

function get_num_cells_ghost( self ) result ( ghost_cells )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: ghost_cells

  ghost_cells = 0
end function get_num_cells_ghost

function get_local_rank( self ) result ( local_rank )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: local_rank

  local_rank = 0
end function get_local_rank

function get_total_ranks( self ) result ( total_ranks )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def) :: total_ranks

  total_ranks = 0
end function get_total_ranks

function get_cell_owner( self, cell_number ) result ( cell_owner )

  implicit none

  class(partition_type), intent(in) :: self

  integer(i_def), intent(in) :: cell_number
  integer(i_def) :: cell_owner

  cell_owner = 0
end function get_cell_owner

function get_gid_from_lid( self, lid ) result ( gid )

  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def), intent(in) :: lid
  integer(i_def)             :: gid

  gid = 0
end function get_gid_from_lid

function get_lid_from_gid( self, gid ) result ( lid )
  implicit none

  class(partition_type), intent(in) :: self
  integer(i_def), intent(in) :: gid           ! global index
  integer(i_def)             :: lid           ! local index

  lid = 0
end function get_lid_from_gid

pure function binary_search( array_to_be_searched, value_to_find ) result ( id )

  implicit none
  integer(i_def), intent(in) :: array_to_be_searched( : )
  integer(i_def), intent(in) :: value_to_find
  integer(i_def)             :: id

  id = 0
end function binary_search

subroutine bubble_sort(array, len)
integer(i_def), intent(inout) :: array(:)
integer(i_def), intent(in)    :: len

end subroutine bubble_sort

end module partition_mod
