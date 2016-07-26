! Author R. Ford STFC Daresbury Lab
program single_kernel_multi_field_same_stencil
  ! Description: an example where the same stencil is used by different
  ! fields in a single kernel (f1 and f2 are the same and f3 and f4 are
  ! the same). Therefore we should only generate a single stencil dofmap
  ! for each.
  use testkern_multi_field_same_stencil_mod, only: testkern_multi_field_same_stencil_type
  use inf, only: field_type
  use flux_direction_mod, only: y_direction
  implicit none
  type(field_type) :: f0,f1,f2,f3,f4
  integer :: extent=2
  integer :: direction=y_direction

  call invoke(                                                     &
       testkern_multi_field_same_stencil_type(f0,                  &
                                              f1,extent,f2,extent, &
                                              f3,extent,direction, &
                                              f4,extent,direction) &
       )

end program single_kernel_multi_field_same_stencil
