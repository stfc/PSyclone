!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern
  type, extends(kernel_type) :: testkern_type
     type(arg_type), dimension(6) :: meta_args = &
          (/ arg_type(gh_integer, gh_read    ), &
             arg_type(gh_field,   gh_write,w1), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_field,   gh_read, w3), &
             arg_type(gh_integer, gh_read    )  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_type
contains

  subroutine testkern_code(iflag, afield1, afield2, afield3, afield4, istep)
    integer, intent(in) :: iflag, istep
    real(wp), dimension(:,:)  :: afield1, afield2, afield3, afield4
  end subroutine testkern_code
end module testkern
