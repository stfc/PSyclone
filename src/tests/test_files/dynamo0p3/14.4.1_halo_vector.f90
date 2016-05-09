!-------------------------------------------------------------------------------
! Copyright Science and Technology Facilities Council 2016
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_reader_vector

  use testkern_vector_2_mod, only: testkern_vector_2_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1(3)

  call invoke(                         &
       testkern_vector_2_type(f1)      &
          )

end program halo_reader_vector
