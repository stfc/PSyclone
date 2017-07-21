!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern
  use argument_mod
  use kernel_mod
  use constants_mod
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(5) :: meta_args =    &
          (/ arg_type(gh_real, gh_read),     &
             arg_type(gh_field,gh_write,w1), &
             arg_type(gh_field,gh_read, w2), &
             arg_type(gh_field,gh_read, w2), &
             arg_type(gh_field,gh_read, w3)  &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(nlayers, ascalar, fld1, fld2, fld3, fld4, &
                           ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, map_w2, &
                           ndf_w3, undf_w3, map_w3)
    integer :: nlayers
    real(kind=r_def) :: ascalar
    real(kind=r_def), dimension(:) :: fld1, fld2, fld3, fld4
    integer :: ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3
    integer, dimension(:) :: map_w1, map_w2, map_w3

  end subroutine testkern_code
end module testkern
