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
module stencil_dofmap_mod

use constants_mod,     only: i_def
use linked_list_data_mod, only : linked_list_data_type

implicit none

private
type, extends(linked_list_data_type), public :: stencil_dofmap_type
  private 
  integer(i_def) :: dofmap_shape
  integer(i_def) :: dofmap_extent
  integer(i_def) :: dofmap_size 
  integer(i_def), allocatable :: dofmap(:,:,:)
contains
  procedure :: get_dofmap
  procedure :: get_whole_dofmap
  procedure :: get_size
end type stencil_dofmap_type

integer(i_def), public, parameter :: STENCIL_POINT = 1100
integer(i_def), public, parameter :: STENCIL_1DX   = 1200
integer(i_def), public, parameter :: STENCIL_1DY   = 1300
integer(i_def), public, parameter :: STENCIL_CROSS = 1400

contains 

function get_dofmap(self,cell) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def),                     intent(in) :: cell
  integer(i_def), pointer                        :: map(:,:) 

  map => null()
  return
end function get_dofmap

function get_whole_dofmap(self) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                        :: map(:,:,:) 

  map => null()
  return
end function get_whole_dofmap

function get_size(self) result(size)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def)                                 :: size

  size = 0
  return
end function get_size

function compute_dofmap_size(st_shape, st_extent) result(size)
  implicit none
  integer(i_def),           intent(in) :: st_shape
  integer(i_def),           intent(in) :: st_extent
  integer(i_def)                       :: size

  size = 1
  return
end function compute_dofmap_size

end module stencil_dofmap_mod

