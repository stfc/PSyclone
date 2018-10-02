!> Module holding basic KIND parameters
module kind_params_mod
  use iso_c_binding
  implicit none

  public

  !> Douple precision kind parameter
  integer, parameter :: GO_WP = SELECTED_REAL_KIND(12,307)

  ! Kind type for double precision
  integer, parameter :: GO_DP = c_double

end module kind_params_mod
