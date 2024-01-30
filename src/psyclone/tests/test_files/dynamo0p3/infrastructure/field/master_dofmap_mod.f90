

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

!> @brief A type which holds information about the master dofmap.
!> @details Type for storing the master_dofmap ( dofmap for a single cell)
module master_dofmap_mod

use constants_mod, only : i_def

implicit none

private
type, public :: master_dofmap_type
  private
  integer(i_def), allocatable :: dofmap(:,:)
contains
  procedure :: get_master_dofmap
  procedure :: get_whole_master_dofmap
  procedure :: clear

  final :: master_dofmap_destructor

 end type master_dofmap_type

interface master_dofmap_type
  module procedure master_dofmap_constructor
end interface

contains

!-----------------------------------------------------------------------------
! Construct the master dofmap
!-----------------------------------------------------------------------------
!> Function to construct the master (cell) dofmap
!> @param[in] master_dofmap
!> @return The master dofmap object
function master_dofmap_constructor( master_dofmap ) result(self)

  implicit none

  integer(i_def), intent(in) :: master_dofmap(:,:)
  type(master_dofmap_type) :: self

  integer(i_def) :: dim1, dim2

  dim1 = size(master_dofmap,1)
  dim2 = size(master_dofmap,2)-1

  allocate( self%dofmap(dim1,0:dim2) )
  self%dofmap(:,:) = master_dofmap(:,:)

  return
end function master_dofmap_constructor
!-----------------------------------------------------------------------------
! Get the master dofmap for a single cell
!-----------------------------------------------------------------------------
!> Returns a pointer to the dofmap for the cell
!! @param[in] self The calling function_space
!! @param[in] cell Which cell
!! @return The pointer which points to a slice of the dofmap
function get_master_dofmap(self,cell) result(map)
  implicit none
  class(master_dofmap_type), target, intent(in) :: self
  integer(i_def),                    intent(in) :: cell
  integer(i_def), pointer                       :: map(:)

  map => self%dofmap(:,cell)
  return
end function get_master_dofmap

!-----------------------------------------------------------------------------
! Get the whole master dofmap for all cells
!-----------------------------------------------------------------------------
!> Returns a pointer to the dofmap for the whole domain
!! @param[in] self The calling function_space
!! @return The pointer which points to the whole dofmap
function get_whole_master_dofmap(self) result(map)
  implicit none
  class(master_dofmap_type), target, intent(in) :: self
  integer(i_def), pointer                       :: map(:,:)

  ! Point to element 1 as dofmap address starts from unused 0
  map => self%dofmap(:,1:)
  return
end function get_whole_master_dofmap

!-----------------------------------------------------------------------------
!> @details Explcitly deallocates any allocatable arrays in the object
!>          to avoid memory leaks
subroutine clear(self)

  implicit none

  class (master_dofmap_type) :: self

  if (allocated(self%dofmap))  deallocate( self%dofmap )

  return
end subroutine clear

!-------------------------------------------------------------------------------
!> @brief Finalizer routine which should automatically call clear
!>        when object is out of scope.
!> @param[in] self, The calling master_dofmap instance
subroutine master_dofmap_destructor(self)

  implicit none

  type (master_dofmap_type), intent(inout) :: self

  call self%clear()

end subroutine master_dofmap_destructor


end module master_dofmap_mod

