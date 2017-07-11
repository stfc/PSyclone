!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: single kernel, single int scalar sum & field reader argument
  use inf, only : i_def
  implicit none
  integer(i_def)   :: isum
  type(field_type) :: f1

  call invoke( inner_product(isum, f1) )

end program single_invoke
