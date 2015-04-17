!> Module holding basic KIND parameters
MODULE kind_params_mod
  IMPLICIT none

  PUBLIC

  !> Douple precision kind parameter
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307)

END MODULE kind_params_mod
