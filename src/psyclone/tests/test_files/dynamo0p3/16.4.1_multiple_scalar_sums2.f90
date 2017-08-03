!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office

program single_invoke

  ! Description: single kernel, multiple scalar sums & field writer argument
  use inf, only : r_def
  implicit none
  integer(r_def)   :: rsum1, rsum2
  type(field_type) :: f1

  call invoke( X_innerproduct_Y(f1, rsum1, rsum2) )

end program single_invoke
