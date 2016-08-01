!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: four kernels specified in an invoke call where the
  ! one integer argument is obtained from different components of two
  ! different objects
  use testkern_one_int_scalar, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  type(some_type)  :: obj_a, obj_b 

  call invoke(                                 &
       testkern_type(f1,obj_a%iflag,f2,m1,m2), &
       testkern_type(f1,obj_b%iflag,f2,m1,m2), &
       testkern_type(f1,obj_a%obj_b,f2,m1,m2), &
       testkern_type(f1,obj_b%obj_a,f2,m1,m2)  &
          )

end program single_invoke
