module inc_field_mod
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use grid_mod, only: OFFSET_SW
  implicit none

  type, extends(kernel_type) :: inc_field
     type(arg), dimension(2) :: meta_args =    &
          (/ arg(WRITE, CT, POINTWISE),        & ! field
             arg(READ,  I_SCALAR, POINTWISE)   & ! istp
           /)
     !> This kernel writes only to internal points of the
     !! simulation domain.
     integer :: ITERATES_OVER = INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = OFFSET_SW

  contains
    procedure, nopass :: code => inc_field_code
  end type inc_field

contains

  subroutine inc_field_code(ji, jj, fld1, istp)
    integer, intent(in) :: ji, jj
    real(wp), allocatable, dimension(:,:), intent(inout) :: fld1
    integer, intent(in) :: istp

    fld1(ji,jj) = fld1(ji,jj) + real(istp, wp)
    
  end subroutine inc_field_code

end module inc_field_mod
