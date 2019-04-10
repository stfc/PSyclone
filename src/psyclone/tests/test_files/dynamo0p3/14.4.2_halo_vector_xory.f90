!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_reader_vector_xory

  ! test halo exchange calls and vector fields
  use testkern_stencil_vector_xory_mod, only: testkern_stencil_vector_xory_type
  use flux_direction_mod, only: x_direction
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2
  integer :: f2_extent=3
  integer :: f2_direction=x_direction

  call invoke(                                                         &
       testkern_stencil_vector_xory_type(f1,f2,f2_extent,f2_direction) &
          )

end program halo_reader_vector_xory
