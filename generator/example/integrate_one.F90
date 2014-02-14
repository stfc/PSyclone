program main
  use lfric
  use algorithm
  use integrate_one_module, only : integrate_one_kernel
  use read_triangle_module

  implicit none

  type(ConstantFunctionSpace_type), pointer :: R_space
  type(state_type) :: state
  type(field_type), pointer :: integral, x

  integer :: i
  
  call read_triangle(state, "data/unitsquare.1",&
       & layer_heights=[real(dp) :: (i/1000., i=0,1000)])

  x => state%extract_field("Coordinate")
  
  R_space => new_ConstantFunctionSpace("R_space")
  integral => new_Field("integral", R_space)

  call invoke(integrate_one_kernel(x, integral))

  print *, integral%data

end program main

