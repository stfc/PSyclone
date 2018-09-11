!-----------------------------------------------------------------------------
! (c) The copyright relating to this information/data is jointly owned by
! the Crown, Met Office and NERC 2013.
! The contribution of STFC in creating this information/data is acknowledged.
!-----------------------------------------------------------------------------

!> Module containing definitions that enable kernel meta-data to be
!! written as valid Fortran.
module argument_mod
use iso_c_binding
use global_parameters_mod
implicit none
private

enum, bind(c) 
   ! The following value is valid for any arg.
   enumerator :: GO_READ
   ! The following values are only valid for fields.
   enumerator :: GO_WRITE, GO_READWRITE, GO_INC
   ! The following values are only valid for globals.
   enumerator :: GO_MIN, GO_MAX, GO_SUM
end enum

! Fake array to enable us to provide stencil meta-data in the args() array
integer, public, parameter :: stencil(0:111,0:111,0:111) = 0

  !args(fs,stencil,arg_intent) ! this need defining
type :: go_arg
  integer :: arg_intent
  integer :: element
  integer :: stencil=0
end type go_arg

!-------------------------------------------------------------------------------
! Expose public types
!-------------------------------------------------------------------------------

! Types to enable declarations of elements.
integer, public, parameter :: GO_R_SCALAR=0, GO_I_SCALAR=1
integer, public, parameter :: GO_EVERY=1
! The four types of grid-point on an Arakawa C-grid
integer, public, parameter :: GO_CU=1, GO_CV=2, GO_CT=3, GO_CF=4
! Arguments that a kernel can request that are supported/provided by
! the infrastructure
!> Kernel requires the model time-step
integer, public, parameter :: GO_TIME_STEP   = 1
!> Kernel requires the cell areas of the T-point grid
integer, public, parameter :: GO_GRID_AREA_T = 2
!> Kernel requires the cell areas of the U-point grid
integer, public, parameter :: GO_GRID_AREA_U = 3
!> Kernel requires the cell areas of the V-point grid
integer, public, parameter :: GO_GRID_AREA_V = 4
!> Kernel requires the land/sea mask at T points
integer, public, parameter :: GO_GRID_MASK_T = 5
!> Kernel requires the horizontal grid spacings of the T-point grid
integer, public, parameter :: GO_GRID_DX_T   = 6
!> Kernel requires the horizontal grid spacings of the U-point grid
integer, public, parameter :: GO_GRID_DX_U   = 7
!> Kernel requires the horizontal grid spacings of the V-point grid
integer, public, parameter :: GO_GRID_DX_V   = 8
!> Kernel requires the vertical grid spacings of the T-point grid
integer, public, parameter :: GO_GRID_DY_T   = 9
!> Kernel requires the vertical grid spacings of the U-point grid
integer, public, parameter :: GO_GRID_DY_U   = 10
!> Kernel requires the vertical grid spacings of the V-point grid
integer, public, parameter :: GO_GRID_DY_V   = 11
!> Kernel requires the geographical latitude of U points
integer, public, parameter :: GO_GRID_LAT_U  = 12
!> Kernel requires the geographical latitude of V points
integer, public, parameter :: GO_GRID_LAT_V  = 13
!> Kernel requires the horizontal grid spacing of the grid.
!! Requires/assumes that this quantity is constant.
integer, public, parameter :: GO_GRID_DX_CONST = 14
!> Kernel requires the vertical grid spacing of the grid.
!! Requires/assumes that this quantity is constant.
integer, public, parameter :: GO_GRID_DY_CONST = 15

public :: go_arg
public :: GO_READ, GO_WRITE, GO_READWRITE, GO_INC
public :: GO_SUM, GO_MIN, GO_MAX

!-------------------------------------------------------------------------------
! Member subroutines
!-------------------------------------------------------------------------------
!contains

end module argument_mod
