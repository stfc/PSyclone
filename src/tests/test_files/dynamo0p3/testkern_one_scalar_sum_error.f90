!---------------------------------------------------------
! Copyright Science and Technology Facilities Council 2016
!---------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(1) :: meta_args = (/ &
             arg_type(gh_integer, gh_sum)           &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(isum)
    integer, intent(inout) :: isum
  end subroutine testkern_code
end module testkern
