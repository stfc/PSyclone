! Author R. Ford STFC Daresbury Lab

module testkern_same_anyspace_stencil_mod
  type, extends(kernel_type) :: testkern_same_anyspace_stencil_type
     type(arg_type), dimension(3) :: meta_args =                      &
          (/ arg_type(gh_field,gh_write,w1),                          &
             arg_type(gh_field,gh_read, any_space_1, stencil(cross)), &
             arg_type(gh_field,gh_read, any_space_1, stencil(cross))  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_same_anyspace_stencil_code
  end type testkern_same_anyspace_stencil_type
contains

  subroutine testkern_same_anyspace_stencil_code()
  end subroutine testkern_same_anyspace_stencil_code

end module testkern_same_anyspace_stencil_mod
