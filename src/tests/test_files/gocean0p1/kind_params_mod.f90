!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> Module holding basic KIND parameters
MODULE kind_params_mod
  IMPLICIT none

  PUBLIC

  !> Douple precision kind parameter
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307)

END MODULE kind_params_mod
