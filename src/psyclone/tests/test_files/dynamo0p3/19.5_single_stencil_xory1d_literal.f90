! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single xory1d stencil with the direction value passed
  ! as a literal.
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4

  call invoke(                                                 &
       testkern_stencil_xory1d_type(f1,f2,2,x_direction,f3,f4) &
       )

end program single_stencil
