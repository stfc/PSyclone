!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_writers

  ! Description: single function specified in an invoke call
  use testkern_writers_mod, only: testkern_writers_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4, f5, f6, f7, f8

  call invoke(                      &
       testkern_writers_type(f1,f2,f3,f4,f5,f6,f7,f8)   &
          )

end program halo_writers
