! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single stencil specified incorrectly in an invoke call
  ! as stencil extent argument is not provided
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: f2_extent=1
  integer :: f2_direction=x_direction

  call invoke(                                                          &
       testkern_stencil_xory1d_type(f1,f2,f2_extent,f2_direction,f3,f4) &
       )

end program single_stencil
