!> Module defining the region_type, used to define a region within
!! some global domain.
module region_mod
  implicit none

  !> Specify a region on the simulation grid
  type :: region_type
     !> Extent of this region in x and y
     integer :: nx, ny
     integer :: xstart, xstop
     integer :: ystart, ystop
  end type region_type

  interface region_type
     module procedure region_constructor
  end interface region_type

contains

  function region_constructor() result(self)
    implicit none
    type(region_type) :: self

    self%nx = 0
    self%ny = 0

    self%xstart = 0
    self%xstop  = 0
    self%ystart = 0
    self%ystop  = 0

  end function region_constructor

end module region_mod
