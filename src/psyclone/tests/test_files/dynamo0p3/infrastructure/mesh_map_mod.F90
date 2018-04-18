!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2018, Science and Technology
! Facilities Council
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
! Modified by: A. R. Porter, STFC

module mesh_map_mod

  use constants_mod,         only: i_def, str_def
  use linked_list_data_mod,  only: linked_list_data_type
  implicit none

  private

  type, extends(linked_list_data_type), public :: mesh_map_type
     private

     integer(i_def), allocatable :: mesh_map(:,:) ! In LIDs

   contains

     procedure, public :: get_nsource_cells
     procedure, public :: get_ntarget_cells_per_source_cell
     procedure, public :: get_ncell_map_ratio => get_ntarget_cells_per_source_cell
     procedure, public :: get_map_from_cell
     procedure, public :: get_map_from_cells
     procedure, public :: get_cell_map
     procedure, public :: get_whole_cell_map
     procedure, public :: get_full_map
  end type mesh_map_type

contains
  
  function get_nsource_cells(self) result(nsource_cells)

    implicit none
    class(mesh_map_type) :: self
    integer(i_def)       :: nsource_cells

    nsource_cells = 0

  end function get_nsource_cells

  function get_ntarget_cells_per_source_cell(self) result(ratio)

    implicit none
    class(mesh_map_type) :: self
    integer(i_def)       :: ratio

    ratio = 0

  end function get_ntarget_cells_per_source_cell

  subroutine get_map_from_cell(self, source_lid, map)

    implicit none
    class(mesh_map_type), intent(in)  :: self
    integer(i_def),       intent(in)  :: source_lid
    integer(i_def),       intent(out) :: map(:)

    map(:) = self%mesh_map(:,source_lid)

  end subroutine get_map_from_cell

  function get_cell_map(self, cell) result(map)
    class(mesh_map_type), target, intent(in) :: self
    integer(kind=i_def),  intent(in) :: cell  
    integer(kind=i_def), pointer :: map(:)

    map => null()

  end function get_cell_map

  function get_whole_cell_map(self) result(map)
    class(mesh_map_type), target, intent(in) :: self

    integer(kind=i_def), pointer :: map(:,:)
    nullify(map)

    map => null()

  end function get_whole_cell_map

  subroutine get_map_from_cells(self, source_lids, map)

    implicit none
    class(mesh_map_type), intent(in)  :: self
    integer(i_def),       intent(in)  :: source_lids(:)
    integer(i_def),       intent(out) :: map(:,:)

    integer(i_def) :: ncells
    integer(i_def) :: i

    ncells = size(source_lids)

    do i=1, ncells
       call self%get_map_from_cell(source_lids(i),map(:,i))
    end do

  end subroutine get_map_from_cells

  subroutine get_full_map(self, map)

    implicit none
    class(mesh_map_type), intent(in)  :: self
    integer(i_def),       intent(out) :: map(:,:)

    integer(i_def) :: ncells
    integer(i_def) :: i

    ncells = size(self%mesh_map,2)

    do i=1, ncells
       call self%get_map_from_cell( i, map(:,i) )
    end do

  end subroutine get_full_map

end module mesh_map_mod
