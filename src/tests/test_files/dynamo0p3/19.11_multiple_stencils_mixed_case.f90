! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single kernel call with the same extent and direction
  ! names but mixed case
  use testkern_stencil_multi_2_mod, only: testkern_stencil_multi_2_type
  use inf, only: field_type
  use flux_direction_mod, only: y_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: extent=2
  integer :: direction=y_direction

  call invoke(                                                         &
       testkern_stencil_multi_2_type(f1,f2,EXTENT,DIRECTION,f3,Extent, &
                                     Direction,f4,extent,direction)    &
       )

end program single_stencil
