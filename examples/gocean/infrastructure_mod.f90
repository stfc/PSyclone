
module infrastructure_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod
  use field_mod
  implicit none

  !> This module is for kernels that implement what will one
  !! day be infrastructure calls. For the moment we provide
  !! these in normal kernel form so that they can be included
  !! in a standard invoke().

  type, extends(kernel_type) :: copy
     type(go_arg), dimension(2) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_EVERY, GO_POINTWISE),     & ! output fld
             go_arg(GO_READ,  GO_EVERY, GO_POINTWISE)      & ! input fld
           /)
     !> This kernel copies a whole field
     integer :: ITERATES_OVER = GO_ALL_PTS

     !> This kernel doesn't care about grids and offsets
     integer :: index_offset = GO_OFFSET_ANY

  contains
    procedure, nopass :: code => field_copy_code
  end type copy

contains

  !===================================================

  subroutine field_copy_code(ji, jj,                     &
                             output, input)
    implicit none
    integer,                  intent(in)  :: ji, jj
    real(go_wp), dimension(:,:), intent(in)  :: input
    real(go_wp), dimension(:,:), intent(out) :: output

    output(ji,jj) = input(ji,jj)

  end subroutine field_copy_code

end module infrastructure_mod
