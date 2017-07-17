!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

program halo_reader_fs

  ! a single kernel call testing all function spaces. Each function
  ! space has a stencil operation so that it requires halo exchange
  ! calls. All extents are passed in and are the same for all fields.
  use testkern_stencil_fs_mod, only: testkern_stencil_fs_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4, f5, f6, f7, f8, f9, f10
  integer :: extent=1

  call invoke(                                                    &
       testkern_stencil_fs_type(f1,f2,extent,f3,extent,f4,extent, &
                                f5,extent,f6,extent,f7,extent,f8, &
                                extent,f9,extent,f10,extent)      &
          )

end program halo_reader_fs
