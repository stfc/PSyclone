!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: multiply-field point-wise operation specified in an invoke call
  ! where the supplied fields can be deduced to be on different spaces
  use testkern_fs, only: testkern_fs_type
  use inf,         only: field_type
  implicit none
  type(field_type) :: f1, f2, f3

  call invoke( multiply_fields(f1, f2, f3) )

end program single_invoke
