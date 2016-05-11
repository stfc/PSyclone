!-------------------------------------------------------------
! (c) Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke
  ! Description: single kernel, single scalar sum argument.
  ! Code generation should fail.
  use testkern_one_scalar_sum_error, only: testkern_type
  use inf, only : i_def
  implicit none
  integer(i_def)   :: isum

  call invoke( testkern_type(isum) )

end program single_invoke
