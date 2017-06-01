module ru_kernel_mod

type, public, extends(kernel_type) :: ru_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  W2),                              &
       arg_type(GH_FIELD,   GH_READ, W3),                              &
       arg_type(GH_INTEGER, GH_READ),                                  &
       arg_type(GH_REAL,    GH_READ),                                  &
       arg_type(GH_FIELD,   GH_READ, W0),                              &
       arg_type(GH_FIELD*3, GH_READ, W0)                               &
       /)
  type(func_type) :: meta_funcs(3) = (/                                &
       func_type(W2, GH_BASIS, GH_DIFF_BASIS),                         &
       func_type(W3, GH_BASIS),                                        &
       func_type(W0, GH_BASIS, GH_DIFF_BASIS)                          &
       /)
  integer :: iterates_over = CELLS
  integer, parameter :: gh_shape = gh_quadrature_XYoZ
contains
  procedure, nopass ::ru_code
end type

contains

subroutine ru_code()
end subroutine ru_code

end module ru_kernel_mod
