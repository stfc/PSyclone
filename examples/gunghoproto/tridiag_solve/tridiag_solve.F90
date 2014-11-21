!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author S. Pickles STFC Daresbury Lab

program tridiag_solver
  use lfric
  use debug
  use algorithm
  use tridiag_solve_module, only : &
    tridiag_solve_kernel,         &
    tridiag_testdata_kernel,      &
    tridiag_verify_kernel      
  use read_triangle_module

  implicit none

  type(element_type) :: h_element, v_element
  type(FunctionSpace_type), pointer :: matrix_space, vector_space
  type(ConstantFunctionSpace_type), pointer :: R_space
  type(state_type) :: state
  type(field_type), pointer :: coord, rhs, soln, matrix, cum_error

  integer :: i

  ! Set debugging level, the higher, the more verbose
  current_debug_level=9
  
  write(1,*) "reading mesh"
  call read_triangle(state, "../data/unitsquare.1",&
       & layer_heights=[real(dp) :: (i/100., i=0,100)])

  coord => state%extract_field("Coordinate")

  h_element = new_Element("DG", cell=referenceTriangle, degree=0)
  v_element = new_Element("DG", cell=referenceInterval, degree=0)

  vector_space => new_FunctionSpace("vector_space",state%topology, h_element*v_element)
  rhs  => new_Field("rhs", vector_space)
  soln => new_Field("soln", vector_space)

  matrix_space => new_FunctionSpace("matrix_space",state%topology, h_element*v_element, shape=[3])
  matrix => new_Field("matrix", matrix_space)

  R_space => new_ConstantFunctionSpace("R_space")
  cum_error => new_Field("cum_error", R_space)

  call invoke(tridiag_testdata_kernel(coord, matrix, rhs))
  call invoke(tridiag_solve_kernel(matrix, rhs, soln))

  ! I think we have to initialise the reduction value in the algorithm layer
  ! We can't do it in the kernel code, obviously.
  ! The generator could do it in the PSy layer, if that's what the SUM argument
  ! means. But SUM doesn't have to mean this.

  cum_error%data = 0.0_dp
  call invoke(tridiag_verify_kernel(matrix, rhs, soln, cum_error))

  print *, cum_error%data

end program tridiag_solver

