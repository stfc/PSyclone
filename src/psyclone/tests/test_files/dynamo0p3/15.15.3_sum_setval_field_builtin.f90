! Modifications copyright (c) 2017, Science and Technology Facilities Council
!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab
! Modified I. Kavcic Met Office

program single_invoke

  ! Description: three point-wise operations specified in an invoke call.
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1
  real(r_def) :: asum = 1.0

  call invoke( setval_c(f1, asum), &
               sum_X(asum, f1),    &
               setval_c(f1, asum) ) 

end program single_invoke
