!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program explicit_name

  ! Description: provide an explicit name for the invoke call
  use testkern1, only: testkern1_type
  use testkern2, only: testkern2_type
  use inf,       only: field_type
  type(field_type)  :: f1, f2, f3, m1, m2, m3

  call invoke(                                                          &
       testkern1_type(f1,f2,m1,m2),                                     &
       testkern2_type(f1,f3,m1,m3),                                     &
       name="multikern"                                                 &
          )

end program explicit_name
