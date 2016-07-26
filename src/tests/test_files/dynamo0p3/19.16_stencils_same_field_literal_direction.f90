! Author R. Ford STFC Daresbury Lab
program multiple_stencils
  ! Description: multiple stencils specified with the same direction
  ! provided in two cases and a different direction in the third.
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction, y_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4

  call invoke(                                                  &
       testkern_stencil_xory1d_type(f1,f2,2,x_direction,f3,f4), &
       testkern_stencil_xory1d_type(f1,f2,2,x_direction,f3,f4), &
       testkern_stencil_xory1d_type(f1,f2,2,y_direction,f3,f4)  &
       )

end program multiple_stencils
