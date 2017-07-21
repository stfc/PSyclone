module testkern_any_space_2_mod

! test for any_space producing correct code where there are a) multi declarations of the same any_space
! space, 2) no other spaces in the arguments, 3) no functions (e.g. basis, diff_basis) declared,
! 4) any_space used with an operator

type, public, extends(kernel_type) ::testkern_any_space_2_type
  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &  
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_1),                    &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_1),       &
       arg_type(GH_INTEGER,  GH_READ)                                  &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, public, nopass :: testkern_any_space_2_code
end type testkern_any_space_2_type
!
subroutine testkern_any_space_2_code()
end subroutine testkern_any_space_2_code
!
end module testkern_any_space_2_mod
