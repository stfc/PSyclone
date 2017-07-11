! Author R. Ford STFC Daresbury Lab
program multiple_stencil
  ! Description: multiple kernels in an invoke with each stencil
  ! access being to a different field. Also shared and individual extents.
  ! f2b and f2c have the same stencil dofmap.
  use testkern_stencil_mod, only: testkern_stencil_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction, y_direction
  implicit none
  type(field_type) :: f1,f2a,f2b,f2c,f3,f4
  integer :: extent=2, f2a_extent=1

  call invoke(                                          &
        testkern_stencil_type(f1,f2a,f2a_extent,f3,f4), &
        testkern_stencil_type(f1,f2b,extent,f3,f4),     &
        testkern_stencil_type(f1,f2c,extent,f3,f4)      &
       )

end program multiple_stencil
