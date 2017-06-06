!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_operator_read_mod
  type, extends(kernel_type) :: testkern_operator_type
     type(arg_type), dimension(3) :: meta_args =    &
          (/ arg_type(gh_operator,gh_read,w0,w0),  &
             arg_type(gh_field*3,gh_write,w0),       &
             arg_type(gh_integer,gh_read)           &
          /)
     type(func_type) :: meta_funcs(1) =             &
          (/ func_type(w0, gh_basis, gh_diff_basis) &
          /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure() :: code => testkern_operator_code
  end type testkern_operator_type
contains
  subroutine testkern_operator_code()
  end subroutine testkern_operator_code
end module testkern_operator_read_mod
