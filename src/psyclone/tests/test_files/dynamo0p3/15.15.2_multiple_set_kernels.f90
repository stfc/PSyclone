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

  ! Description: multiple point-wise set operations specified in an invoke call
  ! with the scalar values passed by both value and reference.
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2
  real(r_def) :: fred, ginger

  fred = 20.1_r_def
  ginger = 40.5_r_def
  
  call invoke(              &
       setval_c(f1, fred),  &
       setval_c(f2, 3.0),   &
       setval_c(f3, ginger) &
          )

end program single_invoke
