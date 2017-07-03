! Author R. Ford STFC Daresbury Lab
program single_stencil
  ! Description: single stencil with an xory1d stencil specified in
  ! the metadata which therefore requires a direction argument. Check
  ! that unique names are produced in the PSy layer for the extent and
  ! direction arguments when we use array indices.
  use testkern_stencil_xory1d_mod, only: testkern_stencil_xory1d_type
  use inf, only: field_type
  use flux_direction_mod, only: x_direction
  implicit none
  type(field_type) :: f1,f2,f3,f4
  integer :: f2_info(2,2)

  ! access extent and direction arguments via an array with different
  ! indices in the same invoke call. The generated extent and
  ! direction names in the PSy-layer should be unique.

  call invoke(                                                              &
       testkern_stencil_xory1d_type(f1,f2,f2_info(1,1),f2_info(1,2),f3,f4), &
       testkern_stencil_xory1d_type(f1,f2,f2_info(2,1),f2_info(2,2),f3,f4)  &
       )

end program single_stencil
