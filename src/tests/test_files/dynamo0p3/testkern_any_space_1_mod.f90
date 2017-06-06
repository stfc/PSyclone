module testkern_any_space_1_mod

! test for any_space producing correct code where there are a) more than one any_space declarations, 
! 2) an existing space as another argument (W0 in this case), 3) func_type basis functions on any_space.

type, public, extends(kernel_type) ::testkern_any_space_1_type
  private
  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1),                     &
       arg_type(GH_REAL,    GH_READ),                                  &
       ARG_TYPE(GH_FIELD,   GH_READ, ANY_SPACE_2),                     &
       ARG_TYPE(GH_FIELD*3, GH_READ, W0)                               &
       /)
  type(func_type) :: meta_funcs(3) = (/                                &
       func_type(ANY_SPACE_1, GH_BASIS),                               &
       FUNC_TYPE(ANY_SPACE_2, GH_BASIS),                               &
       FUNC_TYPE(W0,          GH_DIFF_BASIS)                           &
       /)
  integer :: iterates_over = CELLS
  integer, parameter :: evaluator_shape = quadrature_XYoZ

contains
  procedure, public, nopass :: testkern_any_space_1_code
end type testkern_any_space_1_type
!
subroutine testkern_any_space_1_code()
end subroutine testkern_any_space_1_code
!
end module testkern_any_space_1_mod
