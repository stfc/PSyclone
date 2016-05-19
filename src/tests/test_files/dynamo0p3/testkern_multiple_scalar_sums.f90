!---------------------------------------------------------
! Copyright Science and Technology Facilities Council 2016
!---------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_multiple_scalar_sums_mod

  type, extends(kernel_type) :: testkern_multiple_scalar_sums_type
     type(arg_type), dimension(5) :: meta_args = (/ &
             arg_type(gh_real,    gh_sum),          &
             arg_type(gh_integer, gh_sum),          &
             arg_type(gh_field,   gh_write, w3),    &
             arg_type(gh_real,    gh_sum),          &
             arg_type(gh_integer, gh_sum)           &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_multiple_scalar_sums_code
  end type testkern_multiple_scalar_sums_type

contains

  subroutine testkern_multiple_scalar_sums_code(rsum1, isum1, field, rsum2, isum2)
    integer, intent(inout) :: isum1, isum2
    real, intent(inout) :: rsum1, rsum2
    real, intent(out) :: field
  end subroutine testkern_multiple_scalar_sums_code

end module testkern_multiple_scalar_sums_mod
