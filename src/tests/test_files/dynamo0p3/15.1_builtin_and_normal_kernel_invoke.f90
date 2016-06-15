!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: single point-wise operation specified in an invoke call
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  real(r_def) :: ginger
  
  call invoke(                                &
       testkern_type(ginger, f1, f2, m1, m2), &
       set_field_scalar(0.0, f1)              &
          )

end program single_invoke
