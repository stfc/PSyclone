!> Module holding basic KIND parameters
module ocl_params_mod
  implicit none

  public

  !> Douple precision kind parameter
  integer, parameter :: wp = SELECTED_REAL_KIND(12,307)

end module ocl_params_mod
