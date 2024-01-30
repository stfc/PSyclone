!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Split a W2 field into the component W2v and W2h fields
!>
module split_w2_field_kernel_mod

  use argument_mod,      only : arg_type,             &
                                GH_FIELD, GH_REAL,    &
                                GH_INC, GH_READWRITE, &
                                GH_READ, ANY_SPACE_1, &
                                CELL_COLUMN
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W2, W2h, W2v
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: split_w2_field_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                  &
         arg_type(GH_FIELD, GH_REAL, GH_INC,       W2h), &
         arg_type(GH_FIELD, GH_REAL, GH_READWRITE, W2v), &
         arg_type(GH_FIELD, GH_REAL, GH_READ,      W2)   &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: split_w2_field_code
  end type
  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: split_w2_field_code

contains

!> @brief Split a W2 field into the component W2v and W2h fields
!> @param[in,out] uv Horizontal wind
!> @param[in,out] w Vertical wind
!> @param[in]     uvw 3D wind
!> @param[in]     ndf_w2h  Number of degrees of freedom per cell for W2h
!> @param[in]     undf_w2h Number of unique degrees of freedom for W2h
!> @param[in]     map_w2h  Dofmap for the cell at the base of the column for W2h
!> @param[in]     ndf_w2v  Number of degrees of freedom per cell for W2v
!> @param[in]     undf_w2v Number of unique degrees of freedom for W2v
!> @param[in]     map_w2v  Dofmap for the cell at the base of the column for W2v
!> @param[in]     ndf_w2   Number of degrees of freedom per cell for W2
!> @param[in]     undf_w2  Number of unique degrees of freedom for W2
!> @param[in]     map_w2   Dofmap for the cell at the base of the column for W2

subroutine split_w2_field_code(nlayers,                    &
                               uv, w, uvw,                 &
                               ndf_w2h, undf_w2h, map_w2h, &
                               ndf_w2v, undf_w2v, map_w2v, &
                               ndf_w2,  undf_w2,  map_w2 )
  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: ndf_w2, ndf_w2h, ndf_w2v
  integer(kind=i_def), intent(in) :: undf_w2, undf_w2h, undf_w2v
  integer(kind=i_def), dimension(ndf_w2),  intent(in) :: map_w2
  integer(kind=i_def), dimension(ndf_w2h), intent(in) :: map_w2h
  integer(kind=i_def), dimension(ndf_w2v), intent(in) :: map_w2v
  real(kind=r_def), dimension(undf_w2h), intent(inout)    :: uv
  real(kind=r_def), dimension(undf_w2v), intent(inout)    :: w
  real(kind=r_def), dimension(undf_w2),  intent(in) :: uvw

  ! Internal variables
  integer(kind=i_def) :: df, k

  ! First 2/3 of ndf are horizontal df's (=ndf_w2h),
  ! the rest are vertical dfs' (=ndf_w2v)
  do k = 0, nlayers-1
    do df = 1,ndf_w2h
      uv(map_w2h(df) + k) = uvw(map_w2(df) + k)
    end do
    do df = 1,ndf_w2v
      w(map_w2v(df) + k) = uvw(map_w2(ndf_w2h+df) + k)
    end do
  end do

end subroutine split_w2_field_code

end module split_w2_field_kernel_mod
