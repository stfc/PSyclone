!-----------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-----------------------------------------------------------------------------
! Author A. R. Porter, STFC Daresbury Lab

module simple_with_reduction_mod
type, extends(kernel_type) :: simple_with_reduction_type
    type(arg_type), dimension(3) :: meta_args =  &
         (/ arg_type(gh_real,    gh_sum),        &
            arg_type(gh_field,   gh_read, w1),   &
            arg_type(gh_integer, gh_read) /)
    integer, parameter :: iterates_over = cells
  contains
    procedure() :: code => simple_with_reduction_code
end type simple_with_reduction_type

contains

subroutine simple_with_reduction_code()
end subroutine simple_with_reduction_code

end module simple_with_reduction_mod
