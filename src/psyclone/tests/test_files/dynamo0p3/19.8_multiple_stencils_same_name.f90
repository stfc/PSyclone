! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single kernel call with multiple extents having the
  ! same name
  use testkern_stencil_multi_mod, only: testkern_stencil_multi_type
  use inf, only: field_type
  use flux_direction_mod, only: y_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: extent=2
  integer :: f3_direction=y_direction

  call invoke(                                             &
       testkern_stencil_multi_type(f1,f2,extent,f3,extent, &
                                   f3_direction,f4,extent) &
       )

end program single_stencil
