module matrix_vector_mod

type, public, extends(kernel_type) :: matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &  
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_1),                    &
       arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_1)        &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::matrix_vector_code
end type

contains

subroutine matrix_vector_code()
end subroutine matrix_vector_code

end module matrix_vector_mod
