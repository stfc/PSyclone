!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-------------------------------------------------------------------------------

module apply_variable_hx_kernel_mod
use argument_mod,            only : arg_type,                               &
                                    GH_FIELD, GH_OPERATOR, GH_REAL,         &
                                    GH_READ, GH_WRITE,                      &
                                    ANY_SPACE_1, W3, W2,                    &
                                    CELLS 
use constants_mod,           only : r_def
use kernel_mod,              only : kernel_type
implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: apply_variable_hx_kernel_type
  private
  type(arg_type) :: meta_args(10) = (/                                 &
       arg_type(GH_FIELD,    GH_WRITE, W3),                            &
       arg_type(GH_FIELD,    GH_READ,  W2),                            &
       arg_type(GH_FIELD,    GH_READ,  ANY_SPACE_1),                   &
       arg_type(GH_FIELD,    GH_READ,  W3),                            &
       arg_type(GH_OPERATOR, GH_READ,  W3, W2),                        &
       arg_type(GH_OPERATOR, GH_READ,  W3, ANY_SPACE_1),               &
       arg_type(GH_OPERATOR, GH_READ,  ANY_SPACE_1, W2),               &
       arg_type(GH_OPERATOR, GH_READ,  W3, W3),                        &
       arg_type(GH_REAL,     GH_READ),                                 &
       arg_type(GH_REAL,     GH_READ)                                  &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::apply_variable_hx_code
end type
type, public, extends(kernel_type) :: opt_apply_variable_hx_kernel_type
  private
  type(arg_type) :: meta_args(10) = (/                                 &
       arg_type(GH_FIELD,    GH_WRITE, W3),                            &
       arg_type(GH_FIELD,    GH_READ,  W2),                            &
       arg_type(GH_FIELD,    GH_READ,  ANY_SPACE_1),                   &
       arg_type(GH_FIELD,    GH_READ,  W3),                            &
       arg_type(GH_OPERATOR, GH_READ,  W3, W2),                        &
       arg_type(GH_OPERATOR, GH_READ,  W3, ANY_SPACE_1),               &
       arg_type(GH_OPERATOR, GH_READ,  ANY_SPACE_1, W2),               &
       arg_type(GH_OPERATOR, GH_READ,  W3, W3),                        &
       arg_type(GH_REAL,     GH_READ),                                 &
       arg_type(GH_REAL,     GH_READ)                                  &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: opt_apply_variable_hx_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface apply_variable_hx_kernel_type
   module procedure apply_variable_hx_kernel_constructor
end interface
interface opt_apply_variable_hx_kernel_type
   module procedure opt_apply_variable_hx_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public apply_variable_hx_code
public opt_apply_variable_hx_code
contains

  type(apply_variable_hx_kernel_type) function apply_variable_hx_kernel_constructor() result(self)
  return
end function apply_variable_hx_kernel_constructor
  type(opt_apply_variable_hx_kernel_type) function opt_apply_variable_hx_kernel_constructor() result(self)
  return
end function opt_apply_variable_hx_kernel_constructor

!> @brief Applies the component of the helmholtz operator that maps from velocity space 
!>        to the pressure space as well as the constant in space part
!> @details The Helmholtz operator can be summarised as:
!>          \f[
!>             H(p) = Mp + \nabla.\left( \nabla p \right) + \bar{ \nabla p }
!>         \f]
!>        For a given p & \f[ \nabla p \f] this kernel applies the 
!>        divergence \f[ \nabla. X \f] and averaging  \f[ \bar{X} \f]
!>        operators as well as the application of the mass matrix M
!> @param[in] cell Horizontal cell index
!> @param[in] nlayers Number of layers
!> @param[inout] lhs pressure field with helmholtz operator applied to it
!> @param[in] x gradient of the pressure field in the velocity space
!> @param[in] mt_inv lumped inverse mass matrix for the temperature space
!> @param[in] pressure field that helmholtz operator is being applied to
!> @param[in] ncell_3d_1 Total number of cells for divergence matrix
!> @param[in] div generalised divergence matrix
!> @param[in] ncell_3d_2 Total number of cells for p3t matrix
!> @param[in] p3t mapping from temperature space to pressure space
!> @param[in] ncell_3d_3 Total number of cells for pt2 matrix
!> @param[in] pt2 mapping from velocity space to temperature space
!> @param[in] ncell_3d_4 Total number of cells for m3 matrix
!> @param[in] m3 mass matrix for the pressure space
!> @param[in] tau relaxtation weight 
!> @param[in] dt weight based upon the timestep
!> @param[in] ndf_w3 Number of degrees of freedom per cell for the pressure space
!> @param[in] undf_w3 Unique number of degrees of freedom  for the pressure space
!> @param[in] map_w3 Dofmap for the cell at the base of the column for the pressure space
!> @param[in] ndf_w2 Number of degrees of freedom per cell for the velocity space
!> @param[in] undf_w2 Unique number of degrees of freedom  for the velocity space
!> @param[in] map_w2 Dofmap for the cell at the base of the column for the velocity space
!> @param[in] ndf_wt Number of degrees of freedom per cell for the temperature space
!> @param[in] undf_wt Unique number of degrees of freedom  for the temperature space
!> @param[in] map_wt Dofmap for the cell at the base of the column for the temperature space
subroutine apply_variable_hx_code(cell,        &
                                  nlayers,     &
                                  lhs, x,      &
                                  mt_inv,      &
                                  pressure,    &
                                  ncell_3d_1,  &
                                  div,         &
                                  ncell_3d_2,  &
                                  p3t,         &
                                  ncell_3d_3,  &
                                  pt2,         &
                                  ncell_3d_4,  &
                                  m3,          &     
                                  tau,         &
                                  dt,          &
                                  ndf_w3, undf_w3, map_w3, &
                                  ndf_w2, undf_w2, map_w2, &
                                  ndf_wt, undf_wt, map_wt)
 
  implicit none
  ! Arguments
  integer,                    intent(in) :: cell, nlayers
  integer,                    intent(in) :: ncell_3d_1, ncell_3d_2, ncell_3d_3, ncell_3d_4
  integer,                    intent(in) :: undf_w2, ndf_w2
  integer,                    intent(in) :: undf_w3, ndf_w3
  integer,                    intent(in) :: undf_wt, ndf_wt
  integer, dimension(ndf_w3), intent(in) :: map_w3
  integer, dimension(ndf_w2), intent(in) :: map_w2
  integer, dimension(ndf_wt), intent(in) :: map_wt

  real(kind=r_def), dimension(undf_w2), intent(in)    :: x
  real(kind=r_def), dimension(undf_wt), intent(in)    :: mt_inv
  real(kind=r_def), dimension(undf_w3), intent(inout) :: lhs
  real(kind=r_def), dimension(undf_w3), intent(in)    :: pressure
  real(kind=r_def),                     intent(in)    :: tau, dt

  real(kind=r_def), dimension(ndf_w3,ndf_w2,ncell_3d_1), intent(in) :: div
  real(kind=r_def), dimension(ndf_wt,ndf_w2,ncell_3d_2), intent(in) :: pt2
  real(kind=r_def), dimension(ndf_w3,ndf_wt,ncell_3d_3), intent(in) :: p3t
  real(kind=r_def), dimension(ndf_w3,ndf_w3,ncell_3d_4), intent(in) :: m3

  ! Internal variables
  integer                             :: df, k, ik, is, ie
  real(kind=r_def), dimension(ndf_w2) :: x_e
  real(kind=r_def), dimension(ndf_w3) :: lhs_e, p_e
  real(kind=r_def), dimension(ndf_wt) :: t_e

  real(kind=r_def), allocatable, dimension(:) :: t

  ! Only need a section of the theta field that contains indices for this
  ! column   
  is = minval(map_wt)
  ie = maxval(map_wt)+nlayers-1
  allocate( t(is:ie) )
  t(is:ie) = 0.0_r_def 

  ! Compute Pt2 * u
  do k = 0, nlayers-1
    do df = 1, ndf_w2  
      x_e(df) = x(map_w2(df)+k)
    end do
    ik = (cell-1)*nlayers + k + 1
      
    t_e = matmul(pt2(:,:,ik),x_e)
    do df = 1,ndf_wt
      t(map_wt(df)+k) = t(map_wt(df)+k) + t_e(df)
    end do
  end do

  ! Compute D * u + tau * P3t * Mt^-1 * ( Pt2 * u )
  do k = 0,nlayers-1
    do df = 1,ndf_wt
      t_e(df) = t(map_wt(df)+k)*mt_inv(map_wt(df)+k)*tau
    end do
    do df = 1, ndf_w2
      x_e(df) = x(map_w2(df)+k)
    end do
    do df = 1, ndf_w3
      p_e(df) = pressure(map_w3(df)+k)
    end do

    ik = (cell-1)*nlayers + k + 1

    lhs_e = matmul(m3(:,:,ik),p_e) + dt*(matmul(div(:,:,ik),x_e) + matmul(p3t(:,:,ik),t_e))
    do df = 1,ndf_w3
       lhs(map_w3(df)+k) = lhs_e(df) 
    end do
  end do
end subroutine apply_variable_hx_code

!=============================================================================!
!> @brief Applies the component of the helmholtz operator that maps from velocity space 
!>        to the pressure space as well as the constant in space part, optimised for lowest
!>        order elements with horizontally discontinuous temperature space
!> @details The Helmholtz operator can be summarised as:
!>          \f[
!>             H(p) = Mp + \nabla.\left( \nabla p \right) + \bar{ \nabla p }
!>         \f]
!>        For a given p & \f[ \nabla p \f] this kernel applies the 
!>        divergence \f[ \nabla. X \f] and averaging  \f[ \bar{X} \f]
!>        operators as well as the application of the mass matrix M
!> @param[in] cell Horizontal cell index
!> @param[in] nlayers Number of layers
!> @param[inout] lhs pressure field with helmholtz operator applied to it
!> @param[in] x gradient of the pressure field in the velocity space
!> @param[in] mt_inv lumped inverse mass matrix for the temperature space
!> @param[in] pressure field that helmholtz operator is being applied to
!> @param[in] ncell_3d_1 Total number of cells for divergence matrix
!> @param[in] div generalised divergence matrix
!> @param[in] ncell_3d_2 Total number of cells for p3t matrix
!> @param[in] p3t mapping from temperature space to pressure space
!> @param[in] ncell_3d_3 Total number of cells for pt2 matrix
!> @param[in] pt2 mapping from velocity space to temperature space
!> @param[in] ncell_3d_4 Total number of cells for m3 matrix
!> @param[in] m3 mass matrix for the pressure space
!> @param[in] tau relaxtation weight 
!> @param[in] dt weight based upon the timestep
!> @param[in] ndf_w3 Number of degrees of freedom per cell for the pressure space
!> @param[in] undf_w3 Unique number of degrees of freedom  for the pressure space
!> @param[in] map_w3 Dofmap for the cell at the base of the column for the pressure space
!> @param[in] ndf_w2 Number of degrees of freedom per cell for the velocity space
!> @param[in] undf_w2 Unique number of degrees of freedom  for the velocity space
!> @param[in] map_w2 Dofmap for the cell at the base of the column for the velocity space
!> @param[in] ndf_wt Number of degrees of freedom per cell for the temperature space
!> @param[in] undf_wt Unique number of degrees of freedom  for the temperature space
!> @param[in] map_wt Dofmap for the cell at the base of the column for the temperature space
subroutine opt_apply_variable_hx_code(cell,        &
                                  nlayers,     &
                                  lhs, x,      &
                                  mt_inv,      &
                                  pressure,    &
                                  ncell_3d_1,  &
                                  div,         &
                                  ncell_3d_2,  &
                                  p3t,         &
                                  ncell_3d_3,  &
                                  pt2,         &
                                  ncell_3d_4,  &
                                  m3,          &     
                                  tau,         &
                                  dt,          &
                                  ndf_w3, undf_w3, map_w3, &
                                  ndf_w2, undf_w2, map_w2, &
                                  ndf_wt, undf_wt, map_wt)
 
  implicit none
  ! Arguments
  integer,                    intent(in) :: cell, nlayers
  integer,                    intent(in) :: ncell_3d_1, ncell_3d_2, ncell_3d_3, ncell_3d_4
  integer,                    intent(in) :: undf_w2, ndf_w2
  integer,                    intent(in) :: undf_w3, ndf_w3
  integer,                    intent(in) :: undf_wt, ndf_wt
  integer, dimension(ndf_w3), intent(in) :: map_w3
  integer, dimension(ndf_w2), intent(in) :: map_w2
  integer, dimension(ndf_wt), intent(in) :: map_wt

  real(kind=r_def), dimension(undf_w2), intent(in)    :: x
  real(kind=r_def), dimension(undf_wt), intent(in)    :: mt_inv
  real(kind=r_def), dimension(undf_w3), intent(inout) :: lhs
  real(kind=r_def), dimension(undf_w3), intent(in)    :: pressure
  real(kind=r_def),                     intent(in)    :: tau, dt

  real(kind=r_def), dimension(1,6,ncell_3d_1), intent(in) :: div
  real(kind=r_def), dimension(2,6,ncell_3d_2), intent(in) :: pt2
  real(kind=r_def), dimension(1,2,ncell_3d_3), intent(in) :: p3t
  real(kind=r_def), dimension(1,1,ncell_3d_4), intent(in) :: m3

  ! Internal variables
  integer                        :: df, k, ik
  real(kind=r_def), dimension(2) :: t_e
  real(kind=r_def)               :: div_u

  ! Compute D * u + tau * P3t * Mt^-1 * ( Pt2 * u )
  ! Hard wired optimisation for desired configuration
  k = 0
  ik = (cell-1)*nlayers + k + 1
  t_e(1:2) = 0.0_r_def
  do df = 1,6
    t_e(1) = t_e(1) + pt2(1,df,ik)*x(map_w2(df)+k)
    t_e(2) = t_e(2) + pt2(2,df,ik)*x(map_w2(df)+k) + pt2(1,df,ik+1)*x(map_w2(df)+k+1)
  end do
  t_e(1) = mt_inv(map_wt(1)+k)*p3t(1,1,ik)*t_e(1)
  t_e(2) = mt_inv(map_wt(2)+k)*p3t(1,2,ik)*t_e(2)

  div_u = div(1,1,ik)*x(map_w2(1)+k) + div(1,2,ik)*x(map_w2(2)+k) + div(1,3,ik)*x(map_w2(3)+k) &
        + div(1,4,ik)*x(map_w2(4)+k) + div(1,5,ik)*x(map_w2(5)+k) + div(1,6,ik)*x(map_w2(6)+k)
  lhs(map_w3(1)+k) = m3(1,1,ik)*pressure(map_w3(1)+k) &
                   + dt*(div_u + tau*(t_e(1) + t_e(2)))

  do k = 1,nlayers-2
    ik = (cell-1)*nlayers + k + 1
    t_e(1:2) = 0.0_r_def
    do df = 1,6
      t_e(1) = t_e(1) + pt2(1,df,ik)*x(map_w2(df)+k) + pt2(2,df,ik-1)*x(map_w2(df)+k-1)
      t_e(2) = t_e(2) + pt2(2,df,ik)*x(map_w2(df)+k) + pt2(1,df,ik+1)*x(map_w2(df)+k+1)
    end do
    t_e(1) = mt_inv(map_wt(1)+k)*p3t(1,1,ik)*t_e(1)
    t_e(2) = mt_inv(map_wt(2)+k)*p3t(1,2,ik)*t_e(2)

    div_u = div(1,1,ik)*x(map_w2(1)+k) + div(1,2,ik)*x(map_w2(2)+k) + div(1,3,ik)*x(map_w2(3)+k) &
          + div(1,4,ik)*x(map_w2(4)+k) + div(1,5,ik)*x(map_w2(5)+k) + div(1,6,ik)*x(map_w2(6)+k)
    lhs(map_w3(1)+k) = m3(1,1,ik)*pressure(map_w3(1)+k) &
                     + dt*(div_u + tau*(t_e(1) + t_e(2)))
  end do

  k = nlayers-1
  ik = (cell-1)*nlayers + k + 1
  t_e(1:2) = 0.0_r_def
  do df = 1,6
    t_e(1) = t_e(1) + pt2(1,df,ik)*x(map_w2(df)+k) + pt2(2,df,ik-1)*x(map_w2(df)+k-1)
    t_e(2) = t_e(2) + pt2(2,df,ik)*x(map_w2(df)+k)
  end do
  t_e(1) = mt_inv(map_wt(1)+k)*p3t(1,1,ik)*t_e(1)
  t_e(2) = mt_inv(map_wt(2)+k)*p3t(1,2,ik)*t_e(2)

  div_u = div(1,1,ik)*x(map_w2(1)+k) + div(1,2,ik)*x(map_w2(2)+k) + div(1,3,ik)*x(map_w2(3)+k) &
        + div(1,4,ik)*x(map_w2(4)+k) + div(1,5,ik)*x(map_w2(5)+k) + div(1,6,ik)*x(map_w2(6)+k)
  lhs(map_w3(1)+k) = m3(1,1,ik)*pressure(map_w3(1)+k) &
                   + dt*(div_u + tau*(t_e(1) + t_e(2)))

end subroutine opt_apply_variable_hx_code

end module apply_variable_hx_kernel_mod
