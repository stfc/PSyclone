!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_anyw2_operator_mod
  type, extends(kernel_type) :: testkern_anyw2_operator_type
     type(arg_type), dimension(4) :: meta_args =           &
          (/ arg_type(gh_operator,gh_write,any_w2,any_w2), &
             arg_type(gh_field,gh_read,any_w2),            &
             arg_type(gh_field,gh_read,any_w2),            &
             arg_type(gh_field,gh_read,any_w2)             &
          /)
     type(func_type) :: meta_funcs(1) =  &
          (/ func_type(any_w2, gh_basis) &
          /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: evaluator_shape = quadrature_XYoZ
   contains
     procedure() :: code => testkern_anyw2_operator_code
  end type testkern_anyw2_operator_type
contains
  subroutine testkern_anyw2_operator_code()
  end subroutine testkern_anyw2_operator_code
end module testkern_anyw2_operator_mod
