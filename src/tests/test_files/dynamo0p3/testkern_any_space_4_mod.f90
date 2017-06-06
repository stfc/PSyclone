module testkern_any_space_4_mod

! test for any_space producing correct code with different
! permutations of whether ANY_SPACE is used by another operator/field
! or not and whether it has a basis function or not

type, public, extends(kernel_type) :: testkern_any_space_4_type
  type(arg_type) :: meta_args(6) = (/                                  &
       arg_type(GH_FIELD, GH_READ, ANY_SPACE_5),                       &
       arg_type(GH_OPERATOR, GH_INC, ANY_SPACE_1, ANY_SPACE_2),        &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_3, ANY_SPACE_2),       &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_4, ANY_SPACE_4),       &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_3, ANY_SPACE_5),       &
       arg_type(GH_FIELD, GH_READ, ANY_SPACE_4)                         &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(ANY_SPACE_1, GH_BASIS),                               &
       func_type(ANY_SPACE_4, GH_BASIS, GH_DIFF_BASIS)                 &
       /)
  integer :: iterates_over = CELLS
  integer, parameter :: evaluator_shape = quadrature_XYoZ
contains
  procedure, public, nopass :: testkern_any_space_4_code
end type testkern_any_space_4_type
!
subroutine testkern_any_space_4_code()
end subroutine testkern_any_space_4_code
!
end module testkern_any_space_4_mod
