!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter, STFC Daresbury Lab

program single_invoke

  ! Description: single function specified in an invoke call
  use testkern_no_fields, only: testkern_type
  implicit none
  real(r_def)      :: a
  integer(i_def)   :: istep

  call invoke(                              &
       testkern_type(a,istep)   &
          )

end program single_invoke
