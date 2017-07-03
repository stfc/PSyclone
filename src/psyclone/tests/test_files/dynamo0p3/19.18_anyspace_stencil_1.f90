! Author R. Ford STFC Daresbury Lab
program single_kernel_anyspace_stencil
  ! Description: an example where stencils and any_space are used
  ! within a single kernel. We check when any_space is the same and
  ! when it is different. When it is the same we should have the same
  ! stencil dofmap (as all other stencil information is the same) and
  ! when it is different we should have a different stencil dofmap (as
  ! we do not know if they are on the same space). This also tests the
  ! case where we have different fields with the same and different
  ! any_space names in different kernels.
  use testkern_same_anyspace_stencil_mod, only: testkern_same_anyspace_stencil_type
  use testkern_different_anyspace_stencil_mod, only: testkern_different_anyspace_stencil_type
  use inf, only: field_type
  implicit none
  type(field_type) :: f0,f1,f2,f3,f4,f5
  integer :: extent=2

  ! 1) same kernel, different field, same anyspace (f1,f2)
  ! 2) same kernel, different field, different anyspace (f4,f5)
  ! 3) different kernel, different field, same anyspace (f1,f4)
  ! 4) different kernel, different field, different anyspace (f2,f5)

  call invoke(                                             &
       testkern_same_anyspace_stencil_type(f0,             &
                                           f1,extent,      &
                                           f2,extent),     &
       testkern_different_anyspace_stencil_type(f3,        &
                                                f4,extent, &
                                                f5,extent) &
       )

end program single_kernel_anyspace_stencil
