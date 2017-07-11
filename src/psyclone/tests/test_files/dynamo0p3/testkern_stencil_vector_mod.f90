!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_stencil_vector_mod
  type, extends(kernel_type) :: testkern_stencil_vector_type
     type(arg_type), dimension(2) :: meta_args =                     &
          (/ arg_type(gh_field*3,gh_inc,w0),                         &
             arg_type(gh_field*4,gh_read, w3, stencil(cross))        &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_stencil_vector_code
  end type testkern_stencil_vector_type
contains

  subroutine testkern_stencil_vector_code()
  end subroutine testkern_stencil_vector_code

end module testkern_stencil_vector_mod
