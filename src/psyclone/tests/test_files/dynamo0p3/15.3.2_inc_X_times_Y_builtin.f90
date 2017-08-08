! Modifications copyright (c) 2017, Science and Technology Facilities Council
!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author I. Kavcic Met Office

program single_invoke

  ! Description: single point-wise operation (incremental multiplication of a 
  ! field by another field) specified in an invoke call.
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2

  call invoke( inc_X_times_Y(f1, f2) )

end program single_invoke