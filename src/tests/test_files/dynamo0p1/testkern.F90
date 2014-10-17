module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(3) :: meta_args =    &
          (/ arg_type(gh_rw,v1,fe,.false.,.false.,.false.,.false.), &
             arg_type(gh_rw,v2,fe,.false.,.false.,.false.,.false.), &
             arg_type(gh_rw,v3,fe,.false.,.false.,.false.,.false.)  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(a,b,c)
  end subroutine testkern_code
end module testkern
