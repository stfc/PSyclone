!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Applies 3D tracer_viscosity mu * (d2dx2 + d2dy2 + d2dz2) to a tracer
!>        variable in the Wtheta space for lowest order elements.
!>
module tracer_viscosity_kernel_mod

  use argument_mod,      only : arg_type,                  &
                                GH_FIELD, GH_REAL,         &
                                GH_READ, GH_WRITE,         &
                                GH_SCALAR,                 &
                                CELL_COLUMN, STENCIL, CROSS
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : Wtheta, W2
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: tracer_viscosity_kernel_type
    private
    type(arg_type) :: meta_args(4) = (/                                      &
         arg_type(GH_FIELD,   GH_REAL, GH_WRITE, Wtheta),                    &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  Wtheta, STENCIL(CROSS)),    &
         arg_type(GH_FIELD,   GH_REAL, GH_READ,  W2),                        &
         arg_type(GH_SCALAR,  GH_REAL, GH_READ)                              &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: tracer_viscosity_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: tracer_viscosity_code

contains

!> @brief Computes tracer viscosity
!! @param[in] nlayers Number of layers in the mesh
!! @param[in,out] theta_inc Diffusion increment for temperature field
!! @param[in] theta_n Input temperature field
!! @param[in] map_wt_size Number of cells in the stencil at the base of the
!!                        column for Wtheta
!! @param[in] map_wt Array holding the dofmap for the stencil at the base
!!                   of the column for Wtheta
!! @param[in] dx_at_w2 Grid length at cell faces
!! @param[in] viscosity_mu Viscosity constant
!! @param[in] ndf_wt Number of degrees of freedom per cell for theta space
!! @param[in] undf_wt  Number of unique degrees of freedom for theta space
!! @param[in] cell_map_wt Cell dofmap for the theta space
!! @param[in] ndf_w2 Number of degrees of freedom per cell for w2 space
!! @param[in] undf_w2  Number of unique degrees of freedom for w2 space
!! @param[in] map_w2 Cell dofmap for w2 space
subroutine tracer_viscosity_code(nlayers,                               &
                                 theta_inc, theta_n,                    &
                                 map_wt_size, map_wt,                   &
                                 dx_at_w2,                              &
                                 viscosity_mu,                          &
                                 ndf_wt, undf_wt, cell_map_wt,          &
                                 ndf_w2, undf_w2, map_w2)

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: ndf_wt, undf_wt
  integer(kind=i_def), intent(in) :: ndf_w2, undf_w2
  integer(kind=i_def), intent(in) :: map_wt_size
  integer(kind=i_def), dimension(ndf_wt,map_wt_size), intent(in)  :: map_wt
  integer(kind=i_def), dimension(ndf_wt),             intent(in)  :: cell_map_wt
  integer(kind=i_def), dimension(ndf_w2),             intent(in)  :: map_w2

  real(kind=r_def), dimension(undf_wt),  intent(inout) :: theta_inc
  real(kind=r_def), dimension(undf_wt),  intent(in)    :: theta_n
  real(kind=r_def), dimension(undf_w2),  intent(in)    :: dx_at_w2

  real(kind=r_def), intent(in) :: viscosity_mu

  ! Internal variables
  integer(kind=i_def)                      :: k, km, kp
  real(kind=r_def)                         :: d2dx, d2dy, d2dz
  real(kind=r_def), dimension(0:nlayers-1) :: idx2, idy2, idz2

  ! Assumed direction for derivatives in this kernel is:
  !  y
  !  ^
  !  |_> x
  !

  ! If the full stencil isn't available, we must be at the domain edge.
  ! Simply set the increment to 0 for now, and exit the routine.
  if (map_wt_size < 5_i_def) then
    do k = 0, nlayers
      theta_inc(cell_map_wt(1) + k) = 0.0_r_def
    end do
    return
  end if

  ! Compute grid spacing
  do k = 0, nlayers - 1
    idx2(k) = (2.0_r_def/(dx_at_w2(map_w2(1)+k)+dx_at_w2(map_w2(3)+k)))**2
    idy2(k) = (2.0_r_def/(dx_at_w2(map_w2(2)+k)+dx_at_w2(map_w2(4)+k)))**2
    idz2(k) = 1.0_r_def/(dx_at_w2(map_w2(5)+k))**2
  end do

  ! Theta diffusion
  k = 0
  km = 0
  kp = k + 1
  d2dx = (theta_n(map_wt(1,2) + k)  - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,4) + k) )*idx2(k)
  d2dy = (theta_n(map_wt(1,3) + k)  - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,5) + k) )*idy2(k)
  d2dz = (theta_n(map_wt(1,1) + kp) - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,1) + km))*idz2(k)
  theta_inc(cell_map_wt(1)+k) = viscosity_mu*(d2dx + d2dy + d2dz)
  do k = 1, nlayers-1
    km = k - 1
    kp = k + 1
    d2dx = (theta_n(map_wt(1,2) + k)  - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,4) + k) )*idx2(k)
    d2dy = (theta_n(map_wt(1,3) + k)  - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,5) + k) )*idy2(k)
    d2dz = (theta_n(map_wt(1,1) + kp) - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,1) + km))*idz2(k)
    theta_inc(cell_map_wt(1)+k) = viscosity_mu*(d2dx + d2dy + d2dz)
  end do
  k = nlayers
  km = k - 1
  kp = k
  d2dx = (theta_n(map_wt(1,2) + k)  - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,4) + k) )*idx2(nlayers-1)
  d2dy = (theta_n(map_wt(1,3) + k)  - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,5) + k) )*idy2(nlayers-1)
  d2dz = (theta_n(map_wt(1,1) + kp) - 2.0_r_def*theta_n(map_wt(1,1) + k) + theta_n(map_wt(1,1) + km))*idz2(nlayers-1)
  theta_inc(cell_map_wt(1)+k) = viscosity_mu*(d2dx + d2dy + d2dz)
end subroutine tracer_viscosity_code

end module tracer_viscosity_kernel_mod
