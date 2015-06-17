module kernel_requires_grid_props
  !use kind_params_mod
  !use kernel_mod
  !use argument_mod
  !use grid_mod
  !use field_mod
  implicit none

  !=======================================

  type, extends(kernel_type) :: next_sshu
     type(arg), dimension(5) :: meta_args =  &
          (/ arg(READWRITE, CU, POINTWISE),  &
             arg(READ,      CU, POINTWISE),  &
             arg(READ,      GRID_MASK_T),    &
             arg(READ,      GRID_AREA_T),    &
             arg(READ,      GRID_AREA_U)     &
           /)

     !> We update only those points within the internal region
     !! of the simulated domain.
     integer :: ITERATES_OVER = INTERNAL_PTS

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the North and East of it.
     integer :: index_offset = OFFSET_NE

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
    real(wp), dimension(:,:), intent(in)    :: e12t, e12u
    real(wp), dimension(:,:), intent(inout) :: sshn_u
    real(wp), dimension(:,:), intent(in)    :: sshn
    ! Locals
    real(wp) :: rtmp1

    if(tmask(ji,jj) + tmask(ji+1,jj) <= 0)  return   !jump over non-computational domain

    IF(tmask(ji,jj) * tmask(ji+1,jj) > 0) THEN
      rtmp1 = e12t(ji,jj) * sshn(ji,jj) + e12t(ji+1,jj) * sshn(ji+1,jj)
      sshn_u(ji,jj) = 0.5_wp * rtmp1 / e12u(ji,jj) 
    ELSE IF(tmask(ji,jj) <= 0) THEN
      sshn_u(ji,jj) = sshn(ji+1,jj)
    ELSE IF(tmask(ji+1,jj) <= 0) THEN
      sshn_u(ji,jj) = sshn(ji,jj)
    END IF

  end subroutine next_sshu_code
    
  !================================================

end module kernel_requires_grid_props
