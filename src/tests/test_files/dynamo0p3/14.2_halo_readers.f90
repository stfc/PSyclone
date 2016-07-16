!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_reader

  ! Description: single function specified in an invoke call. On
  ! argument (f2) requires a halo operation as it has a stencil. The
  ! extent of the halo is passed in as an argument.
  use testkern_stencil_mod, only: testkern_stencil_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4
  integer :: f2_extent=2

  call invoke(                                      &
       testkern_stencil_type(f1,f2,f2_extent,f3,f4) &
          )

end program halo_reader
