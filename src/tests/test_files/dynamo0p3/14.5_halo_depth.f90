!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_depth

  use testkern_stencil_depth_mod, only: testkern_stencil_depth_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4
  integer :: extent=2

  call invoke(                                             &
       testkern_stencil_depth_type(f1,f2,extent,f3,extent, &
                                   f4,extent)              &
          )

end program halo_depth
