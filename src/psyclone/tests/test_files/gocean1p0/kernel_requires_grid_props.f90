module kernel_requires_grid_props
  use argument_mod
  use field_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  !=======================================

  type, extends(kernel_type) :: next_sshu
     type(go_arg), dimension(5) :: meta_args =  &
          (/ go_arg(GO_READWRITE, GO_CU, GO_POINTWISE),   &
             go_arg(GO_READ,      GO_CU, GO_STENCIL(000,011,000)),   &
             go_arg(GO_READ,             GO_GRID_MASK_T), &
             go_arg(GO_READ,             GO_GRID_AREA_T), &
             go_arg(GO_READ,             GO_GRID_AREA_U)  &
           /)

     !> We update only those points within the internal region
     !! of the simulated domain.
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the North and East of it.
     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => next_sshu_code
  end type next_sshu

contains

  !================================================

  subroutine next_sshu_code(ji,jj, sshn_u, sshn, &
                            tmask,e12t,e12u)
    implicit none
    integer,                  intent(in)    :: ji, jj
    integer,  dimension(:,:), intent(in)    :: tmask
    real(go_wp), dimension(:,:), intent(in)    :: e12t, e12u
    real(go_wp), dimension(:,:), intent(inout) :: sshn_u
    real(go_wp), dimension(:,:), intent(in)    :: sshn
    ! Locals
    real(go_wp) :: rtmp1

    if(tmask(ji,jj) + tmask(ji+1,jj) <= 0)  return   !jump over non-computational domain

    IF(tmask(ji,jj) * tmask(ji+1,jj) > 0) THEN
      rtmp1 = e12t(ji,jj) * sshn(ji,jj) + e12t(ji+1,jj) * sshn(ji+1,jj)
      sshn_u(ji,jj) = 0.5_go_wp * rtmp1 / e12u(ji,jj) 
    ELSE IF(tmask(ji,jj) <= 0) THEN
      sshn_u(ji,jj) = sshn(ji+1,jj)
    ELSE IF(tmask(ji+1,jj) <= 0) THEN
      sshn_u(ji,jj) = sshn(ji,jj)
    END IF

  end subroutine next_sshu_code
    
  !================================================

end module kernel_requires_grid_props
