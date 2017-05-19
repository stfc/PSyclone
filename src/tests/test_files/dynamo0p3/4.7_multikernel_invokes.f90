!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford and A. Porter STFC Daresbury Lab

program multikernel_invokes_7

  ! Multiple kernel calls within an invoke where the fields updated by
  ! the two kernels are on different spaces

  use ru_kernel_mod, only : ru_kernel_type
  use testkern,      only: testkern_type

  use inf, only : field_type
  implicit none
  type(field_type)      :: a, b, c, d(3), e, f, g, h
  real(r_def)           :: ascalar, rdt
  integer(i_def)        :: istp

  call invoke( ru_kernel_type(a, b, h, istp, rdt, d, e), &
               testkern_type(ascalar, f, b, c, g) )

end program multikernel_invokes_7
