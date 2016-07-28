!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_depth_2

  use testkern_stencil_depth_2_mod, only: testkern_stencil_depth_2_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4
  integer :: f2_extent=1, f3_extent=3, f4_extent=2

  call invoke(                                                  &
       testkern_stencil_depth_2_type(f1,f2,f2_extent,           &
                                     f3,f3_extent,f4,f4_extent) &
          )

end program halo_depth_2
