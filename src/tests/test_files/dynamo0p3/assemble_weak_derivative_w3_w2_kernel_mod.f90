module assemble_weak_derivative_w3_w2_kernel_mod
!
  IMPLICIT NONE
!
  type, public, extends(kernel_type) :: assemble_weak_derivative_w3_w2_kernel_type
    private
    type(arg_type) :: meta_args(2) = (/                        &
         ! Operator maps *to* FS W3 *from* FS W2
         arg_type(GH_OPERATOR, GH_WRITE, W3, W2),              &
         arg_type(GH_FIELD*3,  GH_READ,  W0)                   &
         /)
    type(func_type) :: meta_funcs(3) = (/                      &
         func_type(W0, GH_DIFF_BASIS),                         &
         func_type(W3, GH_BASIS),                              &
         func_type(W2, GH_DIFF_BASIS, GH_ORIENTATION)          &
         /)
    integer :: iterates_over = CELLS
    integer, parameter :: evaluator_shape = quadrature_XYoZ
  contains
    procedure, nopass :: assemble_weak_derivative_w3_w2_kernel_code
  end type
!
CONTAINS
!
  subroutine assemble_weak_derivative_w3_w2_kernel_code()
  end subroutine assemble_weak_derivative_w3_w2_kernel_code
!
end module assemble_weak_derivative_w3_w2_kernel_mod
