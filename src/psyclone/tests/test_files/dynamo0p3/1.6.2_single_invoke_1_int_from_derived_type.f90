!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: three kernels specified in an invoke call where the
  ! one integer is pulled out of a derived type for the first and
  ! is obtained from a type-bound routine in the second and third.
  ! In the third the type-bound routine takes an argument and in the
  ! fourth this argument is itself obtained by dereferencing another
  ! derived type.
  use testkern_one_int_scalar, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  type(some_type)  :: my_obj 
  type(some_type2) :: int_wrapper
  integer :: switch = 4

  call invoke(                                                      &
       testkern_type(f1,my_obj%iflag,f2,m1,m2),                     &
       testkern_type(f1,my_obj%get_flag(),f2,m1,m2),                &
       testkern_type(f1,my_obj%get_flag(switch),f2,m1,m2),          &
       testkern_type(f1,my_obj%get_flag(int_wrapper%data),f2,m1,m2) &
          )

end program single_invoke
