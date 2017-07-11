!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program single_invoke

  ! Description: kernel that has two integer, scalar arguments
  ! specified in an invoke call. One called by value.
  use testkern_two_int_scalars, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, m1, m2
  real(r_def)      :: a
  integer(i_def)   :: iflag, istep

  call invoke(                                  &
       testkern_type(iflag,f1,f2,m1,m2,istep),  &
       testkern_type(1_i_def,f1,f2,m1,m2,istep) &
          )

end program single_invoke
