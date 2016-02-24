!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_reader_fs

  use testkern_stencil_fs_mod, only: testkern_stencil_fs_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4, f5, f6, f7, f8, f9, f10

  call invoke(                                                    &
       testkern_stencil_fs_type(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10)   &
          )

end program halo_reader_fs
