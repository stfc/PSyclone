!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. W. Ford STFC Daresbury Lab

program single_invoke

  ! Description: two different builtin reductions specified in an invoke call
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2
  real(r_def) :: asum, bsum

  call invoke( inner_product(f1, f2, asum), &
               scale_field(f1, asum), &
               sum_field(f2, bsum) )

end program single_invoke
