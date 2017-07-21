! Modifications copyright (c) 2017, Science and Technology Facilities Council
!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!> @brief A type which holds information about the dofmap.
!> @details Types for storing the general stencil dofmaps of a general size
!> Inherits from linked_list_data_type so it can be stored in a linked_list
!> Allowable stencil types ( the size is variable ):
!> Currently on the point stencil is allowed 
!> 
!> POINT --> [1]
!>   
!> 1DX --> |4|2|1|3|5|
!>   
!>         |5|
!>         |3|
!> 1DY --> |1|
!>         |2|
!>         |4|      
!>
!>           |5|
!> CROSS-> |2|1|4|
!>           |3|
!>
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

!-----------------------------------------------------------------------------
! Get the stencil dofmap for a single cell
!-----------------------------------------------------------------------------
!> Function Returns a pointer to the dofmap for the cell 
!! @param[in] self The calling function_space
!! @param[in] cell Which cell
!! @return The pointer which points to a slice of the dofmap
function get_dofmap(self,cell) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def),                     intent(in) :: cell
  integer(i_def), pointer                        :: map(:,:) 

  map => self%dofmap(:,:,cell)
  return
end function get_dofmap
!-----------------------------------------------------------------------------
! Get the stencil dofmap for the whole mesh
!-----------------------------------------------------------------------------
!> Function Returns a pointer to the dofmap for the mesh 
!! @param[in] self The calling function_space
!! @return The pointer which points to the dofmap
function get_whole_dofmap(self) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                        :: map(:,:,:) 

  map => self%dofmap(:,:,:)
  return
end function get_whole_dofmap

!> Returns the size of the stencil in cells
!! @param[in] self The calling function_space
!! @return The size of the stencil in cells
function get_size(self) result(size)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer(i_def)                                 :: size

  size = self%dofmap_size
  return
end function get_size

!> Returns required stencil size in cells for a given stencil shape and extent
!! @param[in] self The calling function_space
!> @param[in] st_shape The shape of the required stencil
!> @param[in] st_extent The extent of the stencil
!! @return The size of the stencil in cells
function compute_dofmap_size(st_shape, st_extent) result(size)
  implicit none
  integer(i_def),           intent(in) :: st_shape
  integer(i_def),           intent(in) :: st_extent
  integer(i_def)                       :: size

  size = 1
  return
end function compute_dofmap_size


end module stencil_dofmap_mod

