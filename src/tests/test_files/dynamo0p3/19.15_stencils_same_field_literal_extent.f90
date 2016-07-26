! Author R. Ford STFC Daresbury Lab
program multiple_stencils
  ! Description: multiple kernels with the same field having a stencil
  ! access (f2) with a literal value for extent and the same value in
  ! one case and a different value in another.
  use testkern_stencil_mod, only: testkern_stencil_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1,f2,f3,f4

  call invoke(                               &
       testkern_stencil_type(f1,f2,1,f3,f4), &
       testkern_stencil_type(f1,f2,1,f3,f4), &
       testkern_stencil_type(f1,f2,2,f3,f4)  &
       )

end program multiple_stencils
