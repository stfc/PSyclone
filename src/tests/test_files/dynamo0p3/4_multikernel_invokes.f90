!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program multikernel_invokes_1

  ! Multiple kernel calls within an invoke

  use testkern, only : testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a

  call invoke(                            &
       testkern_type(a,f1,f2,m1,m2),        &
       testkern_type(a,f1,f2,m1,m2)         &
       )

end program multikernel_invokes_1
