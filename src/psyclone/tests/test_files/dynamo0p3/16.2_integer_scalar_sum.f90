!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office

program single_invoke

  ! Description: single kernel, single int scalar sum & field reader argument.
  ! Tests that using incorrect meta-data to perform a reduction into an
  ! integer variable raises the expected error.
  use inf, only : i_def
  implicit none
  integer(i_def)   :: isum
  type(field_type) :: f1, f2

  call invoke( X_innerproduct_Y(isum, f1, f2) )

end program single_invoke
