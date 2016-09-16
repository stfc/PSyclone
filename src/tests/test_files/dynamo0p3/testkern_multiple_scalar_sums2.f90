!---------------------------------------------------------
! Copyright Science and Technology Facilities Council 2016
!---------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_multiple_scalar_sums2_mod

  type, extends(kernel_type) :: testkern_multiple_scalar_sums2_type
     type(arg_type), dimension(3) :: meta_args = (/ &
             arg_type(gh_real,    gh_sum),          &
             arg_type(gh_real,    gh_sum),          &
             arg_type(gh_field,   gh_write, w3)    &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_multiple_scalar_sums2_code
  end type testkern_multiple_scalar_sums2_type

contains

  subroutine testkern_multiple_scalar_sums2_code(rsum1, rsum2, field)
    real, intent(inout) :: rsum1, rsum2
    real, intent(out) :: field
  end subroutine testkern_multiple_scalar_sums2_code

end module testkern_multiple_scalar_sums2_mod
