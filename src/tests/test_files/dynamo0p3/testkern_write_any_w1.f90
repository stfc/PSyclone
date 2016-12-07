!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab
! A kernel which writes to two fields, one on any space and one on W1
! (continuous). The generated loop bounds in the PSy layer must therefore
! be for the 'worst case' which is the continuous space.
module testkern_write_any_w1
  type, extends(kernel_type) :: testkern_write_any_w1_type
     type(arg_type), dimension(7) :: meta_args = &
          (/ arg_type(gh_field,gh_write,any_space_1), &
             arg_type(gh_field,gh_read, w2),     &
             arg_type(gh_field,gh_read, w2),     &
             arg_type(gh_field,gh_write, w1),     &
             arg_type(gh_field,gh_read, wtheta),&
             arg_type(gh_field,gh_read, w2h),    &
             arg_type(gh_field,gh_read, w2v)     &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_code
  end type testkern_write_any_w1_type
contains

  subroutine testkern_code()
  end subroutine testkern_code
end module testkern_write_any_w1
