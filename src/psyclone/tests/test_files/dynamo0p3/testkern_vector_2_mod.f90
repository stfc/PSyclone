!-------------------------------------------------------------------------------
! Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_vector_2_mod
  type, extends(kernel_type) :: testkern_vector_2_type
     type(arg_type), dimension(1) :: meta_args =                       &
          (/ arg_type(gh_field*3,gh_inc,w0)                            &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_vector_2_code
  end type testkern_vector_2_type
contains

  subroutine testkern_vector_2_code()
  end subroutine testkern_vector_2_code

end module testkern_vector_2_mod
