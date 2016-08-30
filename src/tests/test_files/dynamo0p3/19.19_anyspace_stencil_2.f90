! Author R. Ford STFC Daresbury Lab
program multi_kernel_anyspace_stencil
  ! Description: an example where stencils and any_space are used
  ! in different kernels. We check that the same field always has the
  ! same stencil dofmap irrespective of the any_space name.
  use testkern_same_anyspace_stencil_mod, only: testkern_same_anyspace_stencil_type
  use testkern_different_anyspace_stencil_mod, only: testkern_different_anyspace_stencil_type
  use inf, only: field_type
  implicit none
  type(field_type) :: f0,f1,f2,f3
  integer :: extent=2

  ! 1) different kernel, same field, same anyspace (f1)
  ! 2) different kernel, same field, different anyspace (f2)

  call invoke(                                             &
       testkern_same_anyspace_stencil_type(f0,             &
                                           f1,extent,      &
                                           f2,extent),     &
       testkern_different_anyspace_stencil_type(f3,        &
                                                f1,extent, &
                                                f2,extent) &
       )

end program multi_kernel_anyspace_stencil
