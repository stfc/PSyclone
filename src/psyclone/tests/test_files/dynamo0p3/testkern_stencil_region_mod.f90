! Author R. Ford STFC Daresbury Lab

module testkern_stencil_region_mod
  type, extends(kernel_type) :: testkern_stencil_region_type
     type(arg_type), dimension(4) :: meta_args =              &
          (/ arg_type(gh_field,gh_write,w1),                  &
             arg_type(gh_field,gh_read, w2, stencil(region)), &
             arg_type(gh_field,gh_read, w2),                  &
             arg_type(gh_field,gh_read, w3)                   &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_stencil_region_code
  end type testkern_stencil_region_type
contains

  subroutine testkern_stencil_region_code()
  end subroutine testkern_stencil_region_code

end module testkern_stencil_region_mod
