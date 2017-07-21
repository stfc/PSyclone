! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single stencil with an xory1d stencil specified in
  ! the metadata which therefore requires a direction argument. Check
  ! that name clashes are avoided for extent and direction arguments.
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: f2_stencil_size=1
  integer :: nlayers=x_direction

  ! rename "f2_extent" to "f2_stencil_size" as this is an internally
  ! generated name in the PSy-layer. Similarly, rename "f2_direction" to
  ! "nlayers" as this is an internally generated name in the PSy-layer.

  call invoke(                                                           &
       testkern_stencil_xory1d_type(f1,f2,f2_stencil_size,nlayers,f3,f4) &
       )

end program single_stencil
