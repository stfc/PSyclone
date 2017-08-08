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

  ! Description: multi aX_plus_Y point-wise operations
  ! specified in an invoke call.
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2(7), f3
  real(r_def) :: a

  a = 0.5

  call invoke(                             &
              aX_plus_Y(f2(1), a, f1, f3), &
              aX_plus_Y(f2(2), a, f1, f3), &
              aX_plus_Y(f2(3), a, f1, f3), &
              aX_plus_Y(f3, a, f1, f2(4)), &
              aX_plus_Y(f2(5), a, f1, f3), &
              aX_plus_Y(f2(6), a, f1, f3), &
              aX_plus_Y(f2(7), a, f1, f3)  &
             )

end program single_invoke
