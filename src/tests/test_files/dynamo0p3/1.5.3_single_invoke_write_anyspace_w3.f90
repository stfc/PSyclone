!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke_fs

  ! Description: single function that writes to fields on any-space and
  ! W3
  use testkern_write_any_w3, only: testkern_write_any_w3_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4, m1, m2, m3

  call invoke( testkern_write_any_w3_type(f1, m1, m2, f2, f3, f4, m3) )

end program single_invoke_fs
