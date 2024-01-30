!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council
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
! Modified by J. Henrichs, Bureau of Meteorology

!> Mesh map
!>
module mesh_map_mod

use constants_mod,         only: i_def, str_def
use linked_list_data_mod,  only: linked_list_data_type
use log_mod,               only: log_event, log_scratch_space, &
                                 LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
implicit none

private
public :: generate_mesh_map_id

!> Stores mesh cell mappings in local ids (LID) between two mesh objects.
!> Cell mappings are from a LID of a cell in the source mesh to LIDs of
!> overlapping cells in the target mesh object.
!>
!===============================================================================
type, extends(linked_list_data_type), public :: mesh_map_type

  private

  integer(i_def), allocatable :: mesh_map(:,:,:) ! In LIDs

contains

  !> Gets the ID of the source mesh object for this mesh map object.
  !> @return Id of source mesh object
  procedure, public :: get_source_id
  !>
  !> Gets the ID of the target mesh object for this mesh map object.
  !> @return Id of target mesh object
  procedure, public :: get_target_id
  !>
  !> Gets the number of source cells in this mesh map object.
  !> @return Number of cells in source mesh
  procedure, public :: get_nsource_cells
  !>
  !> Gets the number of target cells for each source cell in this
  !> mesh map object.
  !> @return Number of target cells per source cell
  procedure, public :: get_ntarget_cells_per_source_cell
  !>
  !> Gets the number of target cells in the y-direction for each
  !> source cell in this mesh map object.
  !> @return Number of target cells per source cell in x-direction
  procedure, public :: get_ntarget_cells_per_source_x
  !>
  !> Gets the number of target cells in the y-direction for each
  !> source cell in this mesh map object.
  !> @return Number of target cells per source cell in y-direction
  procedure, public :: get_ntarget_cells_per_source_y
  !>
  !> Gets the target cells ids mapped to the requested source cell
  !> @param [in]  source_lid Local ID of requested source cell
  !> @return map  Pointer to array of local IDs of cells in target
  !>              mesh which overlap with the requested local ID
  !>              in source mesh. Argument should be of rank-1 array
  procedure, public :: get_cell_map
  !>
  !> Gets the target cells ids mapped to all source cells.
  !> @return  map  Pointer to array of local IDs of cells in
  !>               target mesh which overlap with the requested
  !>               local IDs in source mesh. Argument should be
  !>               a rank-2 array
  procedure, public :: get_whole_cell_map

  !>
  !> Forced clear of this oject from memory.
  !> This routine should not need to be called manually except
  !> (possibly) in pfunit tests
  procedure, public :: clear

  !> Finalizer routine, should be called automatically by code when
  !> the object is out of scope
  final :: mesh_map_destructor

end type mesh_map_type

interface mesh_map_type
  module procedure mesh_map_constructor
end interface

contains

!> Instantiates a mesh map object
!> @param[in] source_mesh_id  ID of source mesh object
!> @param[in] target_mesh_id  ID of target mesh object
!> @param[in] map             LID-LID cell map. Dimensions
!>                            of [ntarget_cells_per_source_cell, nsource_cells]
!> @return    mesh_map_type
!===============================================================================
function mesh_map_constructor( source_mesh_id, target_mesh_id, map ) &
                       result( instance )

implicit none

integer(i_def), intent(in) :: source_mesh_id
integer(i_def), intent(in) :: target_mesh_id
integer(i_def), intent(in) :: map(:,:,:)

type(mesh_map_type) :: instance

integer(i_def) :: mesh_map_id
integer(i_def) :: nsource_cells
integer(i_def) :: ntarget_cells_per_source_x
integer(i_def) :: ntarget_cells_per_source_y

mesh_map_id = generate_mesh_map_id(source_mesh_id, target_mesh_id)

! Set the map id
!-------------------------------------------------
call instance%set_id( mesh_map_id )

ntarget_cells_per_source_x =  size(map,1)
ntarget_cells_per_source_y =  size(map,2)
nsource_cells              =  size(map,3)
allocate( instance%mesh_map ( ntarget_cells_per_source_x, &
                              ntarget_cells_per_source_y, &
                              nsource_cells) )

instance%mesh_map(:,:,:) = map(:,:,:)

return
end function mesh_map_constructor


!==============================================================================
function get_source_id(self) result(source_mesh_id)

implicit none
class(mesh_map_type) :: self
integer(i_def)       :: source_mesh_id, mesh_map_id

mesh_map_id = self%get_id()

source_mesh_id = floor(real(mesh_map_id/10000))

return
end function get_source_id


!==============================================================================
function get_target_id(self) result(target_mesh_id)

implicit none
class(mesh_map_type) :: self
integer(i_def)       :: target_mesh_id, mesh_map_id

mesh_map_id = self%get_id()
target_mesh_id = mesh_map_id - self%get_source_id()*10000

return
end function get_target_id


!==============================================================================
function get_nsource_cells(self) result(nsource_cells)

implicit none
class(mesh_map_type) :: self
integer(i_def)       :: nsource_cells

nsource_cells = size(self%mesh_map,3)

return
end function get_nsource_cells


!==============================================================================
function get_ntarget_cells_per_source_cell(self) result(ratio)

implicit none
class(mesh_map_type) :: self
integer(i_def)       :: ratio_x, ratio_y, ratio

ratio_x = size(self%mesh_map,1)
ratio_y = size(self%mesh_map,2)
ratio = ratio_x*ratio_y

return
end function get_ntarget_cells_per_source_cell

function get_ntarget_cells_per_source_x(self) result(ratio_x)

implicit none
class(mesh_map_type) :: self
integer(i_def)       :: ratio_x

ratio_x = size(self%mesh_map,1)

return
end function get_ntarget_cells_per_source_x

function get_ntarget_cells_per_source_y(self) result(ratio_y)

implicit none
class(mesh_map_type) :: self
integer(i_def)       :: ratio_y

ratio_y = size(self%mesh_map,2)

return
end function get_ntarget_cells_per_source_y

function get_cell_map(self, cell) result(map)

  implicit none

  class(mesh_map_type), target, intent(in) :: self

  integer(i_def), intent(in) :: cell
  integer(i_def), pointer  :: map(:,:)

  nullify(map)
  map => self%mesh_map(:,:,cell)

  return
end function get_cell_map


function get_whole_cell_map(self) result(map)

  implicit none

  class(mesh_map_type), target, intent(in) :: self

  integer(i_def), pointer :: map(:,:,:)

  nullify(map)
  map => self%mesh_map(:,:,:)

  return
end function get_whole_cell_map


!==============================================================================
subroutine clear(self)

implicit none

class (mesh_map_type), intent(inout) :: self

if (allocated(self%mesh_map)) deallocate( self%mesh_map)

return
end subroutine clear


!==============================================================================
subroutine mesh_map_destructor(self)

implicit none

type (mesh_map_type), intent(inout) :: self

call self%clear()

return
end subroutine mesh_map_destructor


!> Returns a mesh map id using the ids of source and target meshes.
!> @param[in] source_mesh_id  ID of source mesh object
!> @param[in] target_mesh_id  ID of target mesh object
!> @return    mesh_map_id
!==============================================================================
function generate_mesh_map_id( source_mesh_id,  &
                               target_mesh_id ) &
                       result( mesh_map_id )

implicit none

integer(i_def), intent(in) :: source_mesh_id
integer(i_def), intent(in) :: target_mesh_id

integer(i_def) :: mesh_map_id

mesh_map_id = 10000*source_mesh_id + target_mesh_id

return
end function generate_mesh_map_id

end module mesh_map_mod
