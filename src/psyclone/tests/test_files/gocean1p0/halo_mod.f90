!> Module providing a type for describing halo regions
module halo_mod
  use region_mod
  implicit none

  private

  !> A halo object. Used to apply boundary conditions to a field.
  type, public :: halo_type
     !> Whether this halo is currently out of date and therefore
     !! must be updated before it can be read.
     integer :: needs_update

     type(region_type) :: source
     type(region_type) :: dest

! Commented-out for now because it upset the Intel compiler...
!   contains

     !> Flag that this halo is out of date
!     procedure, public :: set_dirty
     !> Query whether this halo is out of date
!     procedure, public :: is_dirty

  end type halo_type

contains

!  subroutine set_dirty(self)
!    implicit none
!    class(halo_type), target, intent(inout) :: self
!    self%needs_update = 1
!  end subroutine set_dirty

!  function is_dirty(self) result (status)
!    implicit none
!    class(halo_type), target, intent(inout) :: self
!    logical :: status
!    status =  (self%needs_update == 1)
!  end function is_dirty

end module halo_mod
