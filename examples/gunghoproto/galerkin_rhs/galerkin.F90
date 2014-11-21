!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author D. Ham Imperial College

program main
  use lfric
  use algorithm
  use galerkin_module, only : populate_rhs,galerkin_rhs_kernel
  use read_triangle_module

  implicit none

  type(element_type) :: h_cg1_element, v_cg1_element
  type(element_type) :: h_dg0_element, v_dg0_element
  type(FunctionSpace_type), pointer :: cg1_space, dg0_space
  type(state_type) :: state
  type(field_type), pointer :: x, rhs, f

  integer :: i

  call read_triangle(state, "data/unitsquare.1",&
       & layer_heights=[real(dp) :: (i/1000., i=0,1000)])

  x => state%extract_field("Coordinate")
  
  h_cg1_element = new_Element("CG", cell=referenceTriangle, degree=1)
  v_cg1_element = new_Element("CG", cell=referenceInterval, degree=1)
  
  h_dg0_element = new_Element("DG", cell=referenceTriangle, degree=0)
  v_dg0_element = new_Element("DG", cell=referenceInterval, degree=0)

  cg1_space => new_FunctionSpace("CG1_space", state%topology, h_cg1_element*v_cg1_element)
  dg0_space => new_FunctionSpace("DG0_space", state%topology, h_dg0_element*v_dg0_element)

  rhs => new_Field("rhs", cg1_space)
  f => new_Field("f", dg0_space)
  call invoke(populate_rhs(x, f))
  call invoke(galerkin_rhs_kernel(x, rhs, f))

end program main

