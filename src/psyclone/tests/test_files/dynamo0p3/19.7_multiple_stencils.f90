! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single stencil with multiple extents having different
  ! values
  use testkern_stencil_multi_mod, only: testkern_stencil_multi_type
  use inf, only: field_type
  use flux_direction_mod, only: y_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: f2_extent=2, f3_extent=1
  integer :: f3_direction=y_direction

  call invoke(                                                    &
       testkern_stencil_multi_type(f1,f2,f2_extent,f3,f3_extent,  &
                                   f3_direction,f4,1)             &
       )

end program single_stencil
