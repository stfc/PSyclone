! Author R. Ford STFC Daresbury Lab
program multiple_stencil
  ! Description: two kernel calls with the same name (f2_w2) being
  ! used for two different stencil accesses
  use testkern_stencil_mod, only: testkern_stencil_type
  use testkern_stencil_depth_mod, only: testkern_stencil_depth_type
  use inf, only: field_type
  implicit none
  type(field_type) :: f1_w1,f1_w3,f2_w2,f3_w2,f4_w3
  integer :: extent=2, f2_extent=1,

  call invoke(                                                                    &
        testkern_stencil_type(f1_w1,f2_w2,f2_extent,f3_w2,f4_w3),                 &
        testkern_stencil_depth_type(f1_w3,f1_w1,extent,f2_w2,extent,f4_w3,extent) &
  )

end program multiple_stencil
