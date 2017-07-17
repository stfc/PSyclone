!-----------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-----------------------------------------------------------------------------
! Author A. R. Porter, STFC Daresbury Lab

module simple_with_scalars_mod
type, extends(kernel_type) :: simple_with_scalars_type
    type(arg_type), dimension(3) :: meta_args =  &
         (/ arg_type(gh_real,    gh_read),       &
            arg_type(gh_field,   gh_write,w1),   &
            arg_type(gh_integer, gh_read) /)
    integer, parameter :: iterates_over = cells
  contains
    procedure() :: code => simple_with_scalars_code
end type simple_with_scalars_type

contains

subroutine simple_with_scalars_code()
end subroutine simple_with_scalars_code

end module simple_with_scalars_mod
