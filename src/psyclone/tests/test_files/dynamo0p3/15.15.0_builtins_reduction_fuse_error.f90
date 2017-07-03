!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. W. Ford STFC Daresbury Lab

program single_invoke

  ! Description: one reduction builtin followed by an access to the reduction value 
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2
  real(r_def) :: asum

  call invoke( inner_product(f1, f2, asum), &
               scale_field(b, f1), &
               scale_field(asum, f1) )

end program single_invoke
