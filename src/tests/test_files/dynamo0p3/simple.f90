module simple_mod
type, extends(kernel_type) :: simple_type
    type(arg_type), dimension(1) :: meta_args =  &
        (/ arg_type(gh_field,gh_write,w1) /)
    integer, parameter :: iterates_over = cells
  contains
    procedure() :: code => simple_code
end type simple_type
contains
subroutine simple_code()
end subroutine
end module simple_mod
