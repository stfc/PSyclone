!-----------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-----------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module dummy_orientation_mod
  type, extends(kernel_type) :: dummy_orientation_type
     type(arg_type), meta_args(4) =    &
          (/ arg_type(gh_field,   gh_write,w0), &
             arg_type(gh_operator,gh_inc,  w1, w1), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_operator,gh_write,w3, w3)  &
           /)
     type(func_type), meta_funcs(4) =    &
          (/ func_type(w0, gh_orientation), &
             func_type(w1, gh_orientation), &
             func_type(w2, gh_orientation), &
             func_type(w3, gh_orientation) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_orientation_code
  end type dummy_orientation_type
contains
  subroutine dummy_orientation_code()
  end subroutine dummy_orientation_code
end module dummy_orientation_mod

