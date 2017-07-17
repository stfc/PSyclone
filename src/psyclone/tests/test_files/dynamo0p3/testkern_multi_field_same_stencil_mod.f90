! Author R. Ford STFC Daresbury Lab

module testkern_multi_field_same_stencil_mod
  type, extends(kernel_type) :: testkern_multi_field_same_stencil_type
     type(arg_type), dimension(5) :: meta_args =              &
          (/ arg_type(gh_field,gh_write,w1),                  &
             arg_type(gh_field,gh_read, w1, stencil(cross)),  &
             arg_type(gh_field,gh_read, w1, stencil(cross)),  &
             arg_type(gh_field,gh_read, w2, stencil(xory1d)), &
             arg_type(gh_field,gh_read, w2, stencil(xory1d))  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_multi_field_same_stencil_code
  end type testkern_multi_field_same_stencil_type
contains

  subroutine testkern_multi_field_same_stencil_code()
  end subroutine testkern_multi_field_same_stencil_code

end module testkern_multi_field_same_stencil_mod
