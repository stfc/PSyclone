!-----------------------------------------------------------------------------
! (c) The copyright relating to this information/data is jointly owned by
! the Crown, Met Office and NERC 2013.
! The contribution of STFC in creating this information/data is acknowledged.
!-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! DESCRIPTION
!   An arg has a function space (where dofs live on cell) what the stencil is,
!   this is not Grad Phi, but which facets are touched.  could be simple, for
!   example the FE cell integral stencil.  intent: what happens to the data
!   members
!-------------------------------------------------------------------------------

module argument_mod
use iso_c_binding
use global_parameters_mod
implicit none
private

enum, bind(c) 
   ! The following value is valid for any arg.
   enumerator :: READ
   ! The following values are only valid for fields.
   enumerator :: WRITE, READWRITE, INC
   ! The following values are only valid for globals.
   enumerator :: MIN, MAX, SUM
end enum

  !args(fs,stencil,arg_intent) ! this need defining
type :: arg
  integer(kind(READ)) :: arg_intent
  integer :: element
  integer(kind(FE)) :: stencil=0
end type arg


!-------------------------------------------------------------------------------
! Expose public types
!-------------------------------------------------------------------------------

! Types to enable declarations of elements.
integer, public, parameter :: CG(3) = [1,2,3]
integer, public, parameter :: DG(0:3) = [0,1,2,3]
integer, public, parameter :: R=0
integer, public, parameter :: EVERY=1
! The four types of point on an Arakawa C-grid
integer, public, parameter :: CU=1, CV=2, CT=3, CF=4

public :: arg
public :: READ, WRITE, READWRITE, INC
public :: SUM, MIN, MAX

!-------------------------------------------------------------------------------
! Member subroutines
!-------------------------------------------------------------------------------
!contains

end module argument_mod
