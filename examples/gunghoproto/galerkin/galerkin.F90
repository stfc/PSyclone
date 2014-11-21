!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author D. Ham Imperial College

 program main
  use lfric
  ! We need to sort the rather unfortunate name clash for min, max and sum
  use algorithm, notsum=>sum
  use galerkin_module, only : populate_rhs,galerkin_rhs_kernel,galerkin_action, &
                              galerkin_matrix_free_update
  use read_triangle_module

  implicit none

  type(element_type) :: h_cg1_element, v_cg1_element
  type(element_type) :: h_dg0_element, v_dg0_element
  type(FunctionSpace_type), pointer :: cg1_space, dg0_space
  type(ConstantFunctionSpace_type), pointer :: r_space
  type(state_type) :: state
  type(field_type), pointer :: x, rhs, f
  type(field_type), pointer :: one, M_l, Mu
  type(field_type), pointer :: u, res, res_norm

  integer :: i

  call read_triangle(state, "data/unitsquare.1",&
       & layer_heights=[real(dp) :: (i/100., i=0,100)])

  x => state%extract_field("Coordinate")
  
  h_cg1_element = new_Element("CG", cell=referenceTriangle, degree=1)
  v_cg1_element = new_Element("CG", cell=referenceInterval, degree=1)
  
  h_dg0_element = new_Element("DG", cell=referenceTriangle, degree=0)
  v_dg0_element = new_Element("DG", cell=referenceInterval, degree=0)

  cg1_space => new_FunctionSpace("CG1_space", state%topology, &
       &                         h_cg1_element*v_cg1_element)
  dg0_space => new_FunctionSpace("DG0_space", state%topology, &
       &                         h_dg0_element*v_dg0_element)

  R_space => new_ConstantFunctionSpace("R_space")

  ! Populate RHS of projection.
  ! rhs = v * f * dx
  ! where:
  !   v is a test function.
  !   f is the prescribed RHS function.
  rhs => new_Field("rhs", cg1_space)
  f => new_Field("f", dg0_space)
  call invoke(&
       populate_rhs(x, f),&
       galerkin_rhs_kernel(x, rhs, f))

  ! Create the lumped mass matrix using:
  ! action(M, 1)
  ! where:
  ! 1 is the unit function in CG1.
  ! M is the mass matrix in CG1 given by:
  !   M = v * u * dx.
  one => new_field("one", cg1_space)
  M_l => new_field("M_l", cg1_space)
  call invoke(&
       set(one, 1.0), &
       set(M_l, 0.0),&
       galerkin_action(x, M_l, one)&
       )      
  deallocate(one)

  ! Note: should be a unit test.
  print *, "Sum of lumped mass, should be 1.0: ", sum(M_l%data)

  u => new_field("u", cg1_space) 
  Mu => new_field("Mu", cg1_space) 
  res_norm => new_field("norm of residual", r_space) 
  ! Now the iterative solver. This implements the recurrence relation:
  ! u_new = u + (rhs-Mu)/M_l 
  do 
     call invoke(&
          set(res_norm, 0.0), &
          galerkin_action(x, Mu, u), &
          galerkin_matrix_free_update(u, Mu, b, M_l, res_norm)&
          )

     print *, "res_norm", res_norm
     ! Exit on convergence.
     if (res_norm%data(1) < (1.e-7)**2) exit
  end do

end program main

