! Author R. Ford STFC Daresbury Lab
program multiple_stencil
  ! Description: multiple kernel calls with the same and different
  ! extent and direction names.
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type
  use testkern_stencil_multi_mod, only: testkern_stencil_multi_type
  use testkern_stencil_multi_2_mod, only: testkern_stencil_multi_2_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction, y_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: extent=2, f2_extent=1, f3_extent=1
  integer :: direction=y_direction, f3_direction=x_direction

  call invoke(                                                           &
        testkern_stencil_xory1d_type(f1,f2,f2_extent,x_direction,f3,f4), &
        testkern_stencil_multi_type(f1,f2,f2_extent,f3,f3_extent,        &
                                    f3_direction,f4,1),                  &
        testkern_stencil_multi_2_type(f1,f2,extent,direction,f3,extent,  &
                                      direction,f4,extent,direction)     &
       )

end program multiple_stencil
