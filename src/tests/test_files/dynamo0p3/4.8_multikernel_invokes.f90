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
  type(field_type)      :: a, b, c, d, e(3), f, qr, g
  real(r_def)           :: ascalar, rdt
  integer(i_def)        :: istp
               
  call invoke(                                            &
               ! h is written, rest are read-only
               testkern_type(rdt, h, f, c, d),            &
               ! b is written, rest are read-only
               testkern_type(rdt, b, f, c, d),            &
               ! b is gh_inc, rest are read-only
               ru_kernel_type(b, a, istp, rdt, c, e, qr), &
               ! g is gh_inc, rest are read-only
               ru_kernel_type(g, a, istp, rdt, c, e, qr), &
               ! f is written, rest are read-only
               testkern_type(ascalar, f, b, c, d) )

  ! => b and h must be intent(out)
  ! => g and f must be intent(inout)
  ! => a, c, d and e are intent(in)
end program multikernel_invokes_7
