module testkern_any_space_3_mod

! test for any_space producing correct code where there are 1) different spaces for the to and from parts of an operator, 2) no other arguments

type, public, extends(kernel_type) ::testkern_any_space_3_type
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_OPERATOR, GH_INC, ANY_SPACE_1, ANY_SPACE_2)        &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, public, nopass :: testkern_any_space_3_code
end type testkern_any_space_3_type
!
subroutine testkern_any_space_3_code()
end subroutine testkern_any_space_3_code
!
end module testkern_any_space_3_mod
