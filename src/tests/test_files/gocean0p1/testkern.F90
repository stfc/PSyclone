module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(3) :: meta_args =    &
          (/ arg(READ, EVERY, POINTWISE), &
             arg(READ, EVERY, POINTWISE), &
             arg(WRITE, EVERY, POINTWISE)  &
           /)
     integer, parameter :: iterates_over = dofs
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(a,b,c)
  end subroutine testkern_code
end module testkern
