!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_function

  ! Description: single function specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none

  write (*,*) 'This function does nothing!'

end program single_function
