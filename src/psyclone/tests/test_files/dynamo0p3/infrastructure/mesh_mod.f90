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

module mesh_mod

  use constants_mod,        only : i_def, r_def, l_def, pi, imdi
  use linked_list_data_mod, only : linked_list_data_type
  use partition_mod, only: partition_type

  implicit none

  private

  type, extends(linked_list_data_type), public :: mesh_type

    private

    type(partition_type) :: partition
    integer(i_def) :: nlayers
    real(r_def) :: domain_top
    real(r_def), allocatable :: eta(:)
    real(r_def), allocatable :: dz(:)
    real(r_def), allocatable :: vertex_coords(:,:)

    integer(i_def) :: nverts_2d
    integer(i_def) :: nedges_2d
    integer(i_def) :: ncells_2d
    integer(i_def) :: ncells_2d_with_ghost

    integer(i_def) :: nverts
    integer(i_def) :: nedges
    integer(i_def) :: nfaces
    integer(i_def) :: ncells
    integer(i_def) :: ncells_with_ghost

    integer(i_def) :: nverts_per_cell
    integer(i_def) :: nedges_per_cell
    integer(i_def) :: nfaces_per_cell

    integer(i_def), allocatable :: cell_lid_gid_map(:)

    integer(i_def), allocatable :: cell_next    (:,:)
    integer(i_def), allocatable :: vert_on_cell (:,:)
    integer(i_def), allocatable :: face_on_cell (:,:)
    integer(i_def), allocatable :: edge_on_cell (:,:)
    integer(i_def), allocatable :: vert_cell_owner(:,:)
    integer(i_def), allocatable :: edge_cell_owner(:,:)
    integer(i_def), allocatable :: vertex_ownership(:,:)
    integer(i_def), allocatable :: edge_ownership(:,:)

    integer(i_def),              private :: ncolours
    integer(i_def), allocatable, private :: ncells_per_colour(:)
    integer(i_def), allocatable, private :: cells_in_colour(:,:)

    integer(i_def),allocatable           :: last_halo_cell_per_colour(:,:)

  contains

    procedure, public :: get_nlayers
    procedure, public :: get_ncells_2d
    procedure, public :: get_ncells_2d_with_ghost
    procedure, public :: get_nedges_2d
    procedure, public :: get_nverts_2d
    procedure, public :: get_ncells
    procedure, public :: get_nverts
    procedure, public :: get_nedges
    procedure, public :: get_nfaces
    procedure, public :: get_vert_coords
    procedure, public :: get_cell_coords
    procedure, public :: get_column_coords
    procedure, public :: get_nverts_per_cell
    procedure, public :: get_nedges_per_cell
    procedure, public :: get_nfaces_per_cell
    procedure, public :: get_cell_gid
    procedure, public :: get_cell_lid
    procedure, public :: get_cell_next
    procedure, public :: get_face_on_cell
    procedure, public :: get_edge_on_cell
    procedure, public :: get_vert_on_cell
    procedure, public :: get_domain_top
    procedure, public :: get_dz
    procedure, public :: get_eta
    procedure, public :: get_vertex_cell_owner
    procedure, public :: get_edge_cell_owner
    procedure, public :: is_vertex_owned
    procedure, public :: is_edge_owned
    procedure, public :: is_cell_owned
    procedure, public :: get_inner_depth
    procedure, public :: get_num_cells_inner
    procedure, public :: get_last_inner_cell
    procedure, public :: get_num_cells_edge
    procedure, public :: get_last_edge_cell
    procedure, public :: get_halo_depth
    procedure, public :: get_num_cells_halo
    procedure, public :: get_last_halo_cell_any
    procedure, public :: get_last_halo_cell_deepest
    generic           :: get_last_halo_cell => &
                            get_last_halo_cell_any, &
                            get_last_halo_cell_deepest

    procedure, public :: get_last_halo_cell_per_colour_any
    procedure, public :: get_last_halo_cell_per_colour_deepest
    generic           :: get_last_halo_cell_per_colour => &
                            get_last_halo_cell_per_colour_any, &
                            get_last_halo_cell_per_colour_deepest
    
    procedure, public :: get_num_cells_ghost
    procedure, public :: get_gid_from_lid

    procedure, public :: get_total_ranks
    procedure, public :: get_local_rank

    procedure, public :: set_colours
    procedure, public :: get_ncolours
    procedure, public :: get_colours
    procedure, public :: get_colour_map
    procedure, public :: is_coloured

  end type mesh_type

  integer(i_def) :: mesh_id_counter = 0

  integer(i_def), parameter, public :: PLANE             = 1
  integer(i_def), parameter, public :: PLANE_BI_PERIODIC = 2

contains

  subroutine get_vert_coords(self, vert_lid, vertex_coords)

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: vert_lid
    real(r_def),      intent(out) :: vertex_coords(:)

    vertex_coords(:) = 0.0_r_def
  end subroutine get_vert_coords

  subroutine get_cell_coords(self, cell_lid, cell_coords)

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: cell_lid
    real(r_def),      intent(out) :: cell_coords(:,:)

    cell_coords = 0.0_r_def
  end subroutine get_cell_coords

  subroutine get_column_coords(self, cell_lid, column_coords)

    implicit none
    class(mesh_type), intent(in)  :: self
    integer(i_def),   intent(in)  :: cell_lid
    real(r_def),      intent(out) :: column_coords(:,:,:)

    column_coords = 0.0_r_def
  end subroutine get_column_coords

  function get_nlayers(self) result (nlayers)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nlayers

    nlayers = 0
  end function get_nlayers

  function get_nverts_per_cell(self) result (nverts_per_cell)

    implicit none
    class (mesh_type), intent(in) :: self
    integer(i_def)                :: nverts_per_cell

    nverts_per_cell = 0
  end function get_nverts_per_cell

  function get_nedges_per_cell(self) result (nedges_per_cell)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nedges_per_cell

    nedges_per_cell = 0
  end function get_nedges_per_cell

  function get_nfaces_per_cell(self) result (nfaces_per_cell)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nfaces_per_cell

    nfaces_per_cell = 0
  end function get_nfaces_per_cell

  function get_cell_next(self, iface, cell_lid) result (cell_next_lid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: iface
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_next_lid

    cell_next_lid = 0
  end function get_cell_next

  function get_face_on_cell(self, iface, icell) result (face_lid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: iface
    integer(i_def),   intent(in) :: icell
    integer(i_def)               :: face_lid

    face_lid = 0
  end function get_face_on_cell

  function get_edge_on_cell(self, iedge, icell) result (edge_lid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: iedge
    integer(i_def),   intent(in) :: icell
    integer(i_def)               :: edge_lid

    edge_lid = 0
  end function get_edge_on_cell

  function get_vert_on_cell(self, ivert, icell) result (vert_lid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: ivert
    integer(i_def),   intent(in) :: icell
    integer(i_def)               :: vert_lid

    vert_lid = 0
  end function get_vert_on_cell

  function get_ncells_2d(self) result (ncells_2d)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def) :: ncells_2d

    ncells_2d = 0
  end function get_ncells_2d

  function get_ncells_2d_with_ghost(self) result (ncells_2d_with_ghost)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def) :: ncells_2d_with_ghost

    ncells_2d_with_ghost = 0
  end function get_ncells_2d_with_ghost

  function get_nedges_2d(self) result (nedges_2d)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nedges_2d

    nedges_2d = 0
  end function get_nedges_2d

  function get_nverts_2d(self) result (nverts_2d)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nverts_2d

    nverts_2d = 0
  end function get_nverts_2d

  function get_ncells(self) result (ncells)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ncells

    ncells = 0
  end function get_ncells

  function get_nverts(self) result (nverts)

    class(mesh_type), intent(in) :: self
    integer(i_def) :: nverts

    nverts = 0
  end function get_nverts

  function get_nedges(self) result (nedges)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nedges

    nedges = 0
  end function get_nedges

  function get_nfaces(self) result (nfaces)

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: nfaces

    nfaces = 0
  end function get_nfaces

  function get_cell_gid(self, cell_lid) result (cell_gid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_gid

    cell_gid = 0
  end function get_cell_gid

  function get_cell_lid(self, cell_gid) result (cell_lid)

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_gid
    integer(i_def)               :: cell_lid

    cell_lid = 0
  end function get_cell_lid

  function get_domain_top(self) result (domain_top)

    implicit none
    class (mesh_type), intent(in) :: self
    real  (r_def)                 :: domain_top

    domain_top = 0.0_r_def
  end function get_domain_top

  subroutine get_dz(self,dz)

    implicit none
    class (mesh_type), intent(in) :: self
    real(r_def),      intent(out) :: dz(:)

    dz(:) = 0.0_r_def
  end subroutine get_dz

  subroutine get_eta(self,eta)

    implicit none
    class (mesh_type), intent(in) :: self
    real(r_def),      intent(out) :: eta(:)

    eta(:) = 0.0_r_def
  end subroutine get_eta

  function get_vertex_cell_owner( self, vertex_index, cell_lid ) &
                                  result (cell_owner)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: vertex_index
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_owner

    cell_owner = 0
  end function get_vertex_cell_owner

  function get_edge_cell_owner( self, edge_index, cell_lid ) &
                                result (cell_owner)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: edge_index
    integer(i_def),   intent(in) :: cell_lid
    integer(i_def)               :: cell_owner

    cell_owner = 0
  end function get_edge_cell_owner

  function is_vertex_owned( self, vertex_index, cell_lid ) result (owned)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: vertex_index
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
  end function is_vertex_owned

  function is_edge_owned( self, edge_index, cell_lid ) result (owned)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: edge_index
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
  end function is_edge_owned

  function is_cell_owned( self, cell_lid ) result (owned)

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid
    logical(l_def)               :: owned

    owned = .false.
  end function is_cell_owned

  function get_inner_depth( self ) result ( inner_depth )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def) :: inner_depth

    inner_depth = 0
  end function get_inner_depth

  function get_num_cells_inner( self, depth ) result ( inner_cells )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def), intent(in) :: depth
    integer(i_def)             :: inner_cells

    inner_cells = 0
  end function get_num_cells_inner

  function get_last_inner_cell( self, depth ) result ( last_inner_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: last_inner_cell

    last_inner_cell = 0
  end function get_last_inner_cell

  function get_num_cells_edge( self ) result ( edge_cells )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def) :: edge_cells

    edge_cells = 0
  end function get_num_cells_edge

  function get_last_edge_cell( self ) result ( last_edge_cell )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def) :: last_edge_cell

    last_edge_cell = 0
  end function get_last_edge_cell

  function get_halo_depth( self ) result ( halo_depth )
    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def)               :: halo_depth

    halo_depth = 0
  end function get_halo_depth

  function get_num_cells_halo( self, depth ) result ( halo_cells )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: depth
    integer(i_def)               :: halo_cells

    halo_cells = 0
  end function get_num_cells_halo

  function get_last_halo_cell_any( self, depth ) result ( last_halo_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def)             :: last_halo_cell

    last_halo_cell = 0
  end function get_last_halo_cell_any


  function get_last_halo_cell_deepest( self ) result ( last_halo_cell )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def)             :: last_halo_cell
    last_halo_cell = 0
  end function get_last_halo_cell_deepest

  function get_last_halo_cell_per_colour_any( self, colour, depth ) &
                                        result ( ncells_colour )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: depth
    integer(i_def), intent(in) :: colour
    integer(i_def)             :: ncells_colour

    ncells_colour = 0
  end function get_last_halo_cell_per_colour_any

  function get_last_halo_cell_per_colour_deepest( self, colour) &
                                        result ( ncells_colour )
    implicit none

    class(mesh_type), intent(in) :: self

    integer(i_def), intent(in) :: colour
    integer(i_def)             :: ncells_colour

    ncells_colour = 0
  end function get_last_halo_cell_per_colour_deepest

  function get_num_cells_ghost( self ) result ( ghost_cells )

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ghost_cells

    ghost_cells = 0
  end function get_num_cells_ghost

  function get_gid_from_lid( self, cell_lid ) result ( cell_gid )

    implicit none

    class(mesh_type), intent(in) :: self
    integer(i_def),   intent(in) :: cell_lid  ! local index
    integer(i_def)               :: cell_gid  ! global index

    cell_gid = 0
  end function get_gid_from_lid

  function get_total_ranks( self ) result ( total_ranks )

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: total_ranks 

    total_ranks = 0
  end function get_total_ranks

  function get_local_rank( self ) result ( local_rank )

    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: local_rank

    local_rank = 0
  end function get_local_rank

  function get_ncolours(self) result(ncolours)
    implicit none
    class(mesh_type), intent(in) :: self
    integer(i_def)               :: ncolours

    ncolours = 0
  end function get_ncolours

  subroutine get_colours(self, ncolours, ncells_per_colour, colour_map)
    implicit none
    class(mesh_type), intent(in), target      :: self
    integer(i_def), intent(out)               :: ncolours
    integer(i_def), pointer, intent(out)  :: ncells_per_colour(:)
    integer(i_def), pointer, intent(out)  :: colour_map(:,:)
  

    ncolours = self%ncolours
    ncells_per_colour => null()
    colour_map => null()
  end subroutine get_colours


  function get_colour_map(self) result (colour_map)
    implicit none
    class(mesh_type), intent(in), target      :: self
    integer(i_def), pointer                   :: colour_map(:,:)

    colour_map => null()

  end function get_colour_map

  function is_coloured(self) result(cstat)
    implicit none
    class(mesh_type), intent(in) :: self
    logical(l_def)               :: cstat

    cstat = .false.
  end function is_coloured 

  subroutine set_colours(self)
    implicit none
    class(mesh_type), intent(inout) :: self

  end subroutine set_colours

end module mesh_mod
