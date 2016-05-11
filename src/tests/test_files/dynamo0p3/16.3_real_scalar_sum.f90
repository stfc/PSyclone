!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: single kernel, single scalar sum & field reader argument
  use testkern_one_real_scalar_sum, only: testkern_type
  use inf, only : r_def
  implicit none
  real(r_def)   :: rsum
  type(field_type) :: f1

  call invoke( testkern_type(rsum, f1) )

end program single_invoke
