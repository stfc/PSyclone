program invoke_test
  use stencil_domain_mod, only: stencil_domain_type
  use constants_mod, only: i_def, r_def
  use field_mod, only: field_type
  implicit none
  ! Declare minimal variables used in the invoke call
  type(field_type) :: a, b, c, d
  integer(kind=i_def) :: b_extent

  ! The algorithm invokes the kernel via the standard 'invoke' call
  call invoke( stencil_domain_type(a, b, b_extent, c, d) )

end program invoke_test
