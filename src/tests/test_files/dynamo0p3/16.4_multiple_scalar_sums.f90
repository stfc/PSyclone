!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: single kernel, multiple scalar sums & field writer argument
  use testkern_multiple_scalar_sums, only: testkern_multiple_scalar_sums_type
  use inf, only : r_def, i_def
  implicit none
  integer(i_def)   :: isum1, isum2
  integer(r_def)   :: rsum1, rsum2
  type(field_type) :: f1

  call invoke( testkern_multiple_scalar_sums_type(rsum1, isum1, f1, rsum2, isum2) )

end program single_invoke
