!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_operator_orient_2_mod
  type, extends(kernel_type) :: testkern_operator_orient_2_type
     type(arg_type), dimension(2) :: meta_args =    &
          (/ arg_type(gh_operator,gh_write,w1,w2),  &
             arg_type(gh_field*3,gh_read,w0)        &
          /)
     type(func_type) :: meta_funcs(3) =               &
          (/ func_type(w0, gh_diff_basis),            &
             func_type(W1, gh_basis, gh_orientation), &
             func_type(W2, gh_orientation)            &
          /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: evaluator_shape = quadrature_XYoZ
   contains
     procedure() :: code => testkern_operator_orient_2_code
  end type testkern_operator_orient_2_type
contains
  subroutine testkern_operator_orient_2_code()
  end subroutine testkern_operator_orient_2_code
end module testkern_operator_orient_2_mod
