!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Apply the Helmholtz operator when stored as a stencil of coefficients
!>        for the pressure field.
!> @details Apply the Helmholtz operator H to the pressure field p
!>          with each coefficient in the 9-point stencil stored as
!>          a field. The operator is
!>          H*p = HC*p(C) + HE*p(E) + HW*p(W) + HS*p(S) + HN*p(N)
!>              + HU*p(U) + HUU*p(UU) + HD*p(D) + HDD*p(DD)
!>          where C is the central cell, N,E,S,W are the horizontal
!>          neighbours and U, UU, D, DD are the vertical neighbours
!>          (k+1, k+2, k-1, k-2) respectively.
module apply_helmholtz_operator_kernel_mod

  use argument_mod,      only: arg_type,              &
                               GH_FIELD, GH_REAL,     &
                               GH_SCALAR, GH_LOGICAL, &
                               GH_READ, GH_WRITE,     &
                               STENCIL, CROSS2D,      &
                               CELL_COLUMN
  use constants_mod,     only: r_solver, i_def, l_def
  use fs_continuity_mod, only: W3
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  type, public, extends(kernel_type) :: apply_helmholtz_operator_kernel_type
    private
    type(arg_type) :: meta_args(4) = (/                                    &
         arg_type(GH_FIELD,   GH_REAL,    GH_WRITE, W3),                   &
         arg_type(GH_FIELD,   GH_REAL,    GH_READ,  W3, STENCIL(CROSS2D)), &
         arg_type(GH_FIELD*9, GH_REAL,    GH_READ,  W3),                   &
         arg_type(GH_SCALAR,  GH_LOGICAL, GH_READ)                         &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: apply_helmholtz_operator_code
  end type apply_helmholtz_operator_kernel_type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: apply_helmholtz_operator_code

contains

!> @brief Apply the Helmholtz operator when stored as a stencil of coefficients
!>        for the pressure field: y = H*x.
!> @param[in]  nlayers      Number of vertical levels to solve over
!> @param[in,out] y         Application of the operator to the pressure field
!> @param[in]  x            Pressure field to apply operator to
!> @param[in]  smap_sizes   Stencil sizes
!> @param[in]  max_length   Maximum stencil branch length
!> @param[in]  smap         Stencil dofmap
!> @param[in]  Helm_C       Diagonal entry to Helmholtz matrix
!> @param[in]  Helm_N       North (j+1) entry to Helmholtz matrix
!> @param[in]  Helm_E       East (i+1) entry to Helmholtz matrix
!> @param[in]  Helm_S       South (j-1) entry to Helmholtz matrix
!> @param[in]  Helm_W       West (j-1) entry to Helmholtz matrix
!> @param[in]  Helm_U       Upper (k+1) entry to Helmholtz matrix
!> @param[in]  Helm_UU      2nd Upper (k+2) entry to Helmholtz matrix
!> @param[in]  Helm_D       Lower (k-1) entry to Helmholtz matrix
!> @param[in]  Helm_DD      2nd Lower (k-2) entry to Helmholtz matrix
!> @param[in]  limited_area Switch to use code that can handle stencils at edges
!> @param[in]  ndf          Number of dofs per cell for all fields, should
!!                          be = 1
!> @param[in]  undf         Size of all field arrays
!> @param[in]  map          Array containing the address of the first dof in the
!!                          column
subroutine apply_helmholtz_operator_code(nlayers, &
                                         y, x, &
                                         smap_sizes, max_length, smap,     &
                                         Helm_C,                           &
                                         Helm_N, Helm_E, Helm_S, Helm_W,   &
                                         Helm_U, Helm_UU, Helm_D, Helm_DD, &
                                         limited_area,                     &
                                         ndf, undf, map)

  implicit none

  integer(kind=i_def), intent(in) :: ndf, undf
  integer(kind=i_def), intent(in) :: nlayers, max_length
  logical(kind=l_def), intent(in) :: limited_area
  integer(kind=i_def), dimension(4), intent(in) :: smap_sizes

  integer(kind=i_def), dimension(ndf),                intent(in) :: map
  integer(kind=i_def), dimension(ndf, max_length, 4), intent(in) :: smap

  real(kind=r_solver), dimension(undf), intent(inout) :: y
  real(kind=r_solver), dimension(undf), intent(in)    :: x
  real(kind=r_solver), dimension(undf), intent(in)    :: Helm_C,                           &
                                                         Helm_N, Helm_E, Helm_S, Helm_W,   &
                                                         Helm_U, Helm_UU, Helm_D, Helm_DD

  integer(kind=i_def) :: k, branch, cell
  real(kind=r_solver), dimension(max_length,4) :: coeff

  ! Coefficients on this layer

  if (limited_area) then
    ! Use a method that accounts for stencils at the edges of the mesh
    coeff(1,:) = 0.0_r_solver
    do k = 0, nlayers-1
      coeff(2,1) = Helm_W(map(1)+k)
      coeff(2,2) = Helm_S(map(1)+k)
      coeff(2,3) = Helm_E(map(1)+k)
      coeff(2,4) = Helm_N(map(1)+k)

      y(map(1)+k) = Helm_C(map(1)+k) * x(smap(1,1,1)+k)

      ! Loop over the cells in each arm/branch of the cross stencil,
      ! including the center.
      do branch = 1, 4
        do cell = 1, smap_sizes(branch)
          y(map(1)+k) = y(map(1)+k) &
                      + coeff(cell,branch) * x(smap(1,cell,branch)+k)
        end do
      end do
    end do

  else
    ! Use a more efficient method for the global
    do k = 0,nlayers-1
      y(map(1)+k) = Helm_C(map(1)+k)*x(smap(1,1,1)+k) &
                  + Helm_W(map(1)+k)*x(smap(1,2,1)+k) &
                  + Helm_S(map(1)+k)*x(smap(1,2,2)+k) &
                  + Helm_E(map(1)+k)*x(smap(1,2,3)+k) &
                  + Helm_N(map(1)+k)*x(smap(1,2,4)+k)
    end do

  end if

  ! Coefficients on layers above
  do k = 0,nlayers-3
    y(map(1)+k) = y(map(1)+k) + Helm_U(map(1)+k) *x(map(1)+k+1) &
                              + Helm_UU(map(1)+k)*x(map(1)+k+2)
  end do
  k = nlayers - 2
  y(map(1)+k) = y(map(1)+k) + Helm_U(map(1)+k)*x(map(1)+k+1)

  ! Coefficients on layers below
  k = 1
  y(map(1)+k) = y(map(1)+k) + Helm_D(map(1)+k)*x(map(1)+k-1)
  do k = 2,nlayers-1
    y(map(1)+k) = y(map(1)+k) + Helm_D(map(1)+k) *x(map(1)+k-1) &
                              + Helm_DD(map(1)+k)*x(map(1)+k-2)
  end do

end subroutine apply_helmholtz_operator_code

end module apply_helmholtz_operator_kernel_mod
