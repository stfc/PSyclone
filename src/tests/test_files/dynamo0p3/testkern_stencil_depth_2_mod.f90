!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_stencil_depth_2_mod
  type, extends(kernel_type) :: testkern_stencil_depth_2_type
     type(arg_type), dimension(4) :: meta_args =               &
          (/ arg_type(gh_field, gh_inc,   w0),                 &
             arg_type(gh_field, gh_read,  w1, stencil(cross)), &
             arg_type(gh_field, gh_read,  w2, stencil(cross)), &
             arg_type(gh_field, gh_read,  w3, stencil(cross))  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_stencil_depth_2_code
  end type testkern_stencil_depth_2_type
contains

  subroutine testkern_stencil_depth_2_code()
  end subroutine testkern_stencil_depth_2_code

end module testkern_stencil_depth_2_mod
