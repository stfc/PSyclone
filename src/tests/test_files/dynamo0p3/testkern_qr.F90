!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_qr_mod
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), dimension(6) :: meta_args =    &
          (/ arg_type(gh_field,  gh_write,w1), &
             arg_type(gh_field,  gh_read, w2), &
             arg_type(gh_field,  gh_read, w2), &
             arg_type(gh_real,   gh_read),     &
             arg_type(gh_field,  gh_read, w3), &
             arg_type(gh_integer,gh_read)      &
           /)
     type(func_type), dimension(3) :: meta_funcs =    &
          (/ func_type(w1, gh_basis), &
             func_type(w2, gh_diff_basis), &
             func_type(w3, gh_basis, gh_diff_basis)  &
           /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: evaluator_shape = quadrature_XYoZ
   contains
     procedure() :: code => testkern_qr_code
  end type testkern_qr_type
contains

  subroutine testkern_qr_code()
  end subroutine testkern_qr_code
end module testkern_qr_mod
