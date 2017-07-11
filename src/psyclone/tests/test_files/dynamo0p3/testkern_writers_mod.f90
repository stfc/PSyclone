module testkern_writers_mod

! test for any_space producing correct code where there are a) multi declarations of the same any_space
! space, 2) no other spaces in the arguments, 3) no functions (e.g. basis, diff_basis) declared,
! 4) any_space used with an operator

type, public, extends(kernel_type) :: testkern_writers_type
  type(arg_type) :: meta_args(8) = (/                                  &
       arg_type(GH_FIELD,    GH_WRITE, W1),                            &
       arg_type(GH_FIELD,    GH_READ,  W1),                            &
       arg_type(GH_FIELD,    GH_INC,   W1),                            &
       arg_type(GH_FIELD,    GH_READ,  W1),                            &
       arg_type(GH_FIELD,    GH_WRITE, W1),                            &
       arg_type(GH_FIELD,    GH_WRITE, W1),                            &
       arg_type(GH_FIELD,    GH_INC,   W1),                            &
       arg_type(GH_FIELD,    GH_INC,   W1)                             &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, public, nopass :: testkern_writers_code
end type testkern_writers_type
!
subroutine testkern_writers_code()
end subroutine testkern_writers_code
!
end module testkern_writers_mod
