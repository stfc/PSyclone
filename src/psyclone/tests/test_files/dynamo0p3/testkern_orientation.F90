!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R. Ford STFC Daresbury Lab

module testkern_orientation_mod
  type, public, extends(kernel_type) :: testkern_orientation_type
    private
    type(arg_type) :: meta_args(3) = (/                                  &
         arg_type(GH_FIELD,   GH_WRITE, W3),                             &
         arg_type(GH_FIELD,   GH_READ,  W2),                             &
         arg_type(GH_FIELD*3, GH_READ,  W0)                              &
         /)
    type(func_type) :: meta_funcs(3) = (/                                &
         func_type(W3, GH_BASIS),                                        &
         func_type(W2, GH_BASIS, GH_DIFF_BASIS, GH_ORIENTATION),         &
         func_type(W0, GH_BASIS, GH_DIFF_BASIS)                          &
         /)
    integer :: iterates_over = CELLS
    integer, parameter :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure() :: code => testkern_orientation_code
  end type testkern_orientation_type
contains
  subroutine testkern_orientation_code()
  end subroutine testkern_orientation_code
end module testkern_orientation_mod
