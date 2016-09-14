!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: single kernel, multiple scalar sums & field writer argument
  use testkern_multiple_scalar_sums2, only: testkern_multiple_scalar_sums2_type
  use inf, only : r_def
  implicit none
  integer(r_def)   :: rsum1, rsum2
  type(field_type) :: f1

  call invoke( testkern_multiple_scalar_sums2_type(rsum1, rsum2, f1) )

end program single_invoke
