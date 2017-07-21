! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single stencil specified in an invoke call with field
  ! names that will clash with internal PSy-layer names
  use testkern_stencil_mod, only: testkern_stencil_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f2_dofmap,f2,f3
  integer :: f2_extent=1, f3_stencil_size=1

  call invoke(                                                  &
       testkern_stencil_type(f2_stencil_map,f2,f2_extent,       &
                             f2_stencil_dofmap,stencil_cross),  &
       testkern_stencil_type(f3_stencil_map,f3,f3_stencil_size, &
                             f3_stencil_dofmap,stencil_cross)   &
       )

end program single_stencil
