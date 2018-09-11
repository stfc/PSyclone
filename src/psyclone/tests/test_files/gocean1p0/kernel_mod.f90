!-----------------------------------------------------------------------------
! (c) The copyright relating to this information/data is jointly owned by
! the Crown, Met Office and NERC 2013.
! The contribution of STFC in creating this information/data is acknowledged.
!-----------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! NAME
!   kernel_mod
!
! DESCRIPTION
!   The base type for a kernel.
!-------------------------------------------------------------------------------
!> Module defining the base type for a kernel, used in meta-data.
module kernel_mod
use argument_mod
use global_parameters_mod
implicit none
private

public GO_CELLS, GO_EDGES, GO_VERTICES, GO_FE, GO_ARG
public GO_READ, GO_WRITE, GO_READWRITE, GO_INC
public GO_SUM, GO_MIN, GO_MAX

!> These quantities should be defined somewhere in the lfric
!! infrastructure but at the moment they are not!
!! \todo Work out where POINTWISE and DOFS should be declared.
integer, public, parameter :: GO_POINTWISE = 2, GO_DOFS = 5

!> The points in the domain that a kernel will update
integer, public, parameter :: GO_INTERNAL_PTS = 0, &
                              GO_EXTERNAL_PTS = 1, &
                              GO_ALL_PTS = 2

!> The type of grid that a kernel is written to operate on.
!! NEMO uses an orthogonal, curvilinear mesh while
!! shallow has a regular mesh (constant spatial resolution).
!! \todo These should probably be declared somewhere else!
integer, public, parameter :: GO_ORTHOGONAL_REGULAR     = 7
integer, public, parameter :: GO_ORTHOGONAL_CURVILINEAR = 8

type :: kernel_type
  private
  logical :: no_op

end type kernel_type

!-------------------------------------------------------------------------------
! Expose public types
!-------------------------------------------------------------------------------

public :: kernel_type

  
end module kernel_mod



