!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2018, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by I Kavcic, Met Office
!
!> @brief Kernel which computes the advective update u.grad(t) through fitting a
!>        high order upwind 1D reconstruction
!> @details Compute the advective update for a tracer field using a high order
!>          polynomial fit to the tracer values. The stencil used for the
!>          polynomial is centred, with an upwind bias if an even number of 
!>          points are used, therefore for an upwind shceme an odd ordered 
!>          polynomial should be used. In the vertical the order is reduced 
!>          near the boundaries depending on the number of points available.
!>          This method is only valid for lowest order elements 
module sample_poly_adv_kernel_mod

use argument_mod,  only : arg_type, func_type,                  &
                          GH_FIELD, GH_WRITE, GH_READ,          &
                          W2, Wtheta, ANY_SPACE_1,              &
                          GH_BASIS, GH_DIFF_BASIS, CELLS,       &
                          GH_EVALUATOR, STENCIL, CROSS

use constants_mod,         only: r_def, i_def
use kernel_mod,            only: kernel_type
use reference_element_mod, only: W, E, N, S
use transport_config_mod,  only: consistent_metric

use transport_config_mod, only: enforce_monotonicity
use timestepping_config_mod,  only: dt

implicit none

! Precomputed operators, these are the same for all model columns
real(kind=r_def), allocatable,    private :: coeff_matrix(:,:)
integer(kind=i_def), allocatable, private :: dof_stencil(:,:)
integer(kind=i_def), allocatable, private :: np_v(:,:)
integer(kind=i_def),              private :: np
real(kind=r_def),                 private :: x0
real(kind=r_def), allocatable,    private :: dx0(:)
real(kind=r_def), allocatable,    private :: coeff_matrix_v(:,:,:)

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: sample_poly_adv_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                                  &
       arg_type(GH_FIELD,   GH_WRITE, Wtheta),                         &
       arg_type(GH_FIELD,   GH_READ,  Wtheta, STENCIL(CROSS)),         &
       arg_type(GH_FIELD,   GH_READ,  W2),                             &
       arg_type(GH_FIELD,   GH_READ,  ANY_SPACE_1, STENCIL(CROSS)),    &
       arg_type(GH_FIELD,   GH_READ,  ANY_SPACE_1),                    &
       arg_type(GH_FIELD,   GH_READ,  ANY_SPACE_1)                     &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(W2,          GH_BASIS),                               &
       func_type(ANY_SPACE_1, GH_BASIS, GH_DIFF_BASIS)                 &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_EVALUATOR
contains
  procedure, nopass ::sample_poly_adv_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface sample_poly_adv_kernel_type
   module procedure sample_poly_adv_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public sample_poly_adv_code
public sample_poly_adv_init
contains

type(sample_poly_adv_kernel_type) function sample_poly_adv_kernel_constructor() result(self)
  return
end function sample_poly_adv_kernel_constructor

!> @brief Computes the advective update
!! @param[in]  nlayers Number of layers
!! @param[out] advection Advective update to compute 
!! @param[in]  tracer Tracer field
!! @param[in]  stencil_size Size of the stencil (number of cells)
!! @param[in]  stencil_map Dofmaps for the stencil
!! @param[in]  wind Wind field
!! @param[in]  mt_lumped_inv Lumped inverse mass matrix for wtheta
!! @param[in]  chi1 The physical x coordinate in chi
!! @param[in]  stencil_size_wx Size of the stencil (number of cells) for chi
!! @param[in]  stencil_map_wx Dofmaps for the stencil for chi
!! @param[in]  chi2 The physical y coordinate in chi
!! @param[in]  chi3 The physical z coordinate in chi
!! @param[in]  ndf_wt Number of degrees of freedom per cell
!! @param[in]  undf_wt Number of unique degrees of freedom for the tracer field
!! @param[in]  ndf_w2 Number of degrees of freedom per cell
!! @param[in]  undf_w2 Number of unique degrees of freedom for the wind field
!! @param[in]  map_w2 Dofmap for the cell at the base of the column
!! @param[in]  basis_w2 Basis function array evaluated at w2 nodes
!! @param[in]  ndf_wx Number of degrees of freedom per cell
!! @param[in]  undf_wx Number of unique degrees of freedom for chi 
!! @param[in]  map_wx Dofmap for the cell at the base of the column
!! @param[in]  basis_wx Basis function array evaluated at chi nodes
!! @param[in]  diff_basis_wx Differential basis function array evaluated at chi nodes
subroutine sample_poly_adv_code( nlayers,              &
                                 advection,            &
                                 tracer,               &
                                 stencil_size,         &
                                 stencil_map,          &
                                 wind,                 &
                                 mt_lumped_inv,        &
                                 chi1,                 &
                                 stencil_size_wx,      &
                                 stencil_map_wx,       &
                                 chi2, chi3,           &
                                 ndf_wt,               &
                                 undf_wt,              &
                                 map_wt,               &
                                 ndf_w2,               &
                                 undf_w2,              &
                                 map_w2,               &
                                 basis_w2,             &
                                 ndf_wx,               &
                                 undf_wx,              &
                                 map_wx,               &
                                 basis_wx,             &
                                 diff_basis_wx         &
                                 )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                    :: nlayers
  integer(kind=i_def), intent(in)                    :: ndf_wt
  integer(kind=i_def), intent(in)                    :: undf_wt
  integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
  integer(kind=i_def), intent(in)                    :: ndf_w2
  integer(kind=i_def), intent(in)                    :: undf_w2
  integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
  integer(kind=i_def), intent(in)                    :: ndf_wx
  integer(kind=i_def), intent(in)                    :: undf_wx
  integer(kind=i_def), dimension(ndf_wx), intent(in) :: map_wx

  real(kind=r_def), dimension(undf_wt), intent(out)  :: advection
  real(kind=r_def), dimension(undf_w2), intent(in)   :: wind
  real(kind=r_def), dimension(undf_wt), intent(in)   :: tracer
  real(kind=r_def), dimension(undf_wx), intent(in)   :: chi1, chi2, chi3 
  real(kind=r_def), dimension(undf_wt), intent(in)   :: mt_lumped_inv 

  real(kind=r_def), dimension(3,ndf_w2,ndf_wt), intent(in) :: basis_w2
  real(kind=r_def), dimension(1,ndf_wx,ndf_wt), intent(in) :: basis_wx
  real(kind=r_def), dimension(3,ndf_wx,ndf_wt), intent(in) :: diff_basis_wx

  integer(kind=i_def),                                 intent(in) :: stencil_size
  integer(kind=i_def), dimension(ndf_wt,stencil_size), intent(in) :: stencil_map
  integer(kind=i_def),                                 intent(in) :: stencil_size_wx
  integer(kind=i_def), dimension(ndf_wx,stencil_size_wx), intent(in) :: stencil_map_wx

  ! Internal variables
  integer(kind=i_def) :: k, df, dft, p, dir, id, idx, km1, kp1
  real(kind=r_def)    :: u(3,nlayers+1)
  real(kind=r_def)    :: polynomial_tracer, advection_update, z0 
  real(kind=r_def), allocatable :: x(:,:,:)
  real(kind=r_def)    :: etadot, dzdx_l, dzdx_c, dzdy_l, dzdy_c, dx, dy, dz
  real(kind=r_def)    :: tracer_min, tracer_max
  real(kind=r_def)    :: coeff(np), tracer_stencil(np)

  ! Compute wind at tracer points
  u(:,:) = 0.0_r_def
  do k = 0, nlayers - 1
    do df = 1,ndf_w2
      do dft = 1,ndf_wt
        u(:,k + dft) = u(:,k + dft) + wind(map_w2(df) + k)*basis_w2(:,df,dft)
      end do
    end do   
  end do
  ! Average velocities across cell boundaries due to double counting
  ! the winds at the surface and lid are also reduced to mimic
  ! the weighting in the fem mass matrix
  ! which for the surface term is int(1-z,z=0..1) = 1/2
  ! whilst for interior terms it is  int(1-z,z=0..1) + int(z,z=0..1) = 1
  do k = 1,nlayers+1
    u(:,k) = u(:,k)*0.5_r_def   
  end do

  ! Compute coordinates at all tracer points in stencil
  if ( consistent_metric ) then
    allocate( x(3,nlayers+1,stencil_size) )
    x(:,:,:) = 0.0_r_def
    do p = 1,stencil_size
      do df = 1,ndf_wx
        do dft = 1,ndf_wt
          do k = 0, nlayers - 1
            x(1,k + dft,p) = x(1,k + dft,p) + chi1(stencil_map_wx(df,p) + k)*basis_wx(1,df,dft)
            x(2,k + dft,p) = x(2,k + dft,p) + chi2(stencil_map_wx(df,p) + k)*basis_wx(1,df,dft)
            x(3,k + dft,p) = x(3,k + dft,p) + chi3(stencil_map_wx(df,p) + k)*basis_wx(1,df,dft)
          end do
        end do   
      end do
      ! Average coordinates across cell boundaries due to double counting from
      ! the vertical continuity of Wtheta
      do k = 2,nlayers
        x(:,k,p) = 0.5_r_def*x(:,k,p)
      end do
    end do
  end if

  ! Tracer data is stored contiguously and so can just use dft = 1
  ! and increment by 1 to get each subsequent level up to dft + nlayers 
  ! for the model lid
  do k = 0, nlayers
    dft = 1     
    ! Compute x stencil
    ! stencil_dir is East if u < 0 otherwise it is West
    if (  u(1,k+dft) >= 0.0_r_def ) then
      dir = W
    else
      dir = E
    end if
    do p = 1,np
      tracer_stencil(p) = tracer( stencil_map(dft,dof_stencil(p,dir)) + k )
    end do
    coeff(:) = matmul(coeff_matrix,tracer_stencil)
    polynomial_tracer = 0.0_r_def
    do p = 1,np-1
      polynomial_tracer = polynomial_tracer + coeff(p+1)*dx0(p)
    end do
    ! Polynomial tracer contains the directional derivative
    ! ( dP/dx for u > 0, -dP/dx for u < 0) so need to cancel
    ! out sign of wind - hence presense of abs(u)
    advection_update = abs(u(1,k+dft))*polynomial_tracer

    ! Compute metric term using advection scheme
    if ( consistent_metric ) then
      do p = 1,np
        tracer_stencil(p) = x(3,k+dft,dof_stencil(p,dir))
      end do
      coeff(:) = matmul(coeff_matrix,tracer_stencil)
      dzdx_c = 0.0_r_def
      do p = 1,np-1
        dzdx_c = dzdx_c + coeff(p+1)*dx0(p)
      end do    
    end if
    ! Compute y stencil
    ! stencil_dir is north if v < 0 otherwise it is south
    if (  u(2,k+dft) >= 0.0_r_def ) then
      dir = S
    else
      dir = N
    end if
    do p = 1,np
      tracer_stencil(p) = tracer( stencil_map(dft,dof_stencil(p,dir)) + k )
    end do
    coeff(:) = matmul(coeff_matrix,tracer_stencil)
    polynomial_tracer = 0.0_r_def
    do p = 1,np-1
      polynomial_tracer = polynomial_tracer + coeff(p+1)*dx0(p)
    end do
    ! Polynomial tracer contains the directional derivative
    ! ( dP/dy for v > 0, -dP/dy for v < 0) so need to cancel
    ! out sign of wind - hence presense of abs(v)
    advection_update = advection_update + abs(u(2,k+dft))*polynomial_tracer

    ! Compute metric term using advection scheme
    if ( consistent_metric ) then
      do p = 1,np
        tracer_stencil(p) = x(3,k+dft,dof_stencil(p,dir))
      end do
      coeff(:) = matmul(coeff_matrix,tracer_stencil)
      dzdy_c = 0.0_r_def
      do p = 1,np-1
        dzdy_c = dzdy_c + coeff(p+1)*dx0(p)
      end do    
    
      ! Modify vertical velocity to take account of inconsistent metric terms
      ! w => w + u*(dzdx_l - dzdx_c) + v*(dzdy_l - dzdy_c)
      ! with _l indicating centred linear approximation
      ! and _c indicating upwind cubic (or whatever form the horizontal advection
      ! uses)
      dzdx_l = 0.0_r_def
      dzdy_l = 0.0_r_def
      dx = 0.0_r_def
      dy = 0.0_r_def
      dz = 0.0_r_def
      do df = 1,ndf_wx
        dx = dx + chi1(stencil_map_wx(df,1)+k)*diff_basis_wx(1,df,dft)
        dy = dy + chi2(stencil_map_wx(df,1)+k)*diff_basis_wx(2,df,dft)
        dz = dz + chi3(stencil_map_wx(df,1)+k)*diff_basis_wx(3,df,dft)
        dzdx_l = dzdx_l + chi3(stencil_map_wx(df,1)+k)*diff_basis_wx(1,df,dft)
        dzdy_l = dzdy_l + chi3(stencil_map_wx(df,1)+k)*diff_basis_wx(2,df,dft)
      end do
      dzdx_l = dzdx_l / dx
      dzdy_l = dzdy_l / dy 
      dzdx_c = dzdx_c / dx
      dzdy_c = dzdy_c / dy

      etadot = u(3,k+dft) + u(1,k+dft)*dx/dz*(dzdx_l-dzdx_c) + u(2,k+dft)*dy/dz*(dzdy_l-dzdy_c) 
      if ( k == 0 .or. k == nlayers ) etadot = 0.0_r_def
    else
      etadot = u(3,k+dft)
    end if
    ! Compute z stencil, dir < 0 if w > 0
    if (  etadot >= 0.0_r_def ) then
      dir = -1
      idx = 1
    else
      dir = 1
      idx = 2
    end if
    do p = 1,np_v(k,idx) 
      id = stencil_map(dft,1) + k + dir*(np_v(k,idx)/2 - (p-1))
      tracer_stencil(p) = tracer( id )
    end do

    ! Use appropriate inverse matrix for the order of this point
    coeff(1:np_v(k,idx)) = matmul(coeff_matrix_v(1:np_v(k,idx),1:np_v(k,idx),np_v(k,idx)), &
                                  tracer_stencil(1:np_v(k,idx)))
    z0 = real(np_v(k,idx)/2,r_def)
    polynomial_tracer = 0.0_r_def
    do p = 1,np_v(k,idx)-1
      polynomial_tracer = polynomial_tracer + coeff(p+1)*real(p,r_def)*z0**(p-1)
    end do
    ! Polynomial tracer contains the directional derivative
    ! ( dP/dz for w > 0, -dP/dz for w < 0) so need to cancel
    ! out sign of wind - hence presense of abs(w)

    advection_update = advection_update + abs(etadot)*polynomial_tracer
    
    if (enforce_monotonicity)then
      km1=max(0,k-1)
      kp1=min(nlayers, k+1)
      tracer_min = minval(tracer( stencil_map(dft,1:stencil_size)+k))
      tracer_min = min(tracer_min, minval(tracer( stencil_map(dft,1)+km1:stencil_map(dft,1)+kp1)) )
      tracer_max = maxval(tracer( stencil_map(dft,1:stencil_size)+k)) 
      tracer_max = max(tracer_max, maxval(tracer( stencil_map(dft,1)+km1:stencil_map(dft,1)+kp1)) )

      advection_update = min((tracer(stencil_map(dft,1)+k) - tracer_min)/dt &
         /mt_lumped_inv(stencil_map(dft,1)+k), &
         advection_update)
      advection_update = max((tracer(stencil_map(dft,1)+k) - tracer_max)/dt &
         /mt_lumped_inv(stencil_map(dft,1)+k), &
         advection_update)
    end if
    
    advection(stencil_map(dft,1)+k) = advection_update
  end do
end subroutine sample_poly_adv_code

!=============================================================================!
!>@brief Initialise various quantities needed for sample_poly_adv_code
!>@param[in] order Polynomial order for advective computations
!>@param[in] nlayers Number of vertical layers
subroutine sample_poly_adv_init(order, nlayers)

  use matrix_invert_mod, only: matrix_invert

  implicit none
  
  integer(kind=i_def), intent(in) :: order, nlayers
  integer(kind=i_def)             :: p, i, j, k
  integer(kind=i_def)             :: nupwindcells, ndownwindcells
  real(kind=r_def), allocatable   :: inv_coeff_matrix(:,:)

  ! Order p polynomial has p+1 coefficients
  np = order + 1_i_def   

  ! Compute the stencil table
  ! This give the cell id's in the stencilmap in a different order so that
  ! in each direction the first entry is the most upwind cell and the last
  ! the most downwind cell.
  ! e.g for a 1depth stencil map
  !     ---
  !     |5|
  !   -------
  !   |2|1|4|
  !   -------
  !     |3|
  !     ---
  !
  ! The stencil array will be:
  ! stencil = ( 2,1,4
  !             3,1,5
  !             4,1,2
  !             5,1,3)
  ! that is ( i+1, i, i-1
  !           j+1, j, j-1
  !           i-1, i, i+1
  !           j-1, j, j+1) 
  allocate(dof_stencil(np,4))

  ! Index of first upwind cell is  (j + (nupwindcells-1)*4)
  ! where j = [2,3,4,5] for [W,S,E,N] directions
  nupwindcells = int(real(np,r_def)/2.0_r_def)
  i = 1
  do p = 1,nupwindcells
    dof_stencil(i,1) = int((2 + (nupwindcells-1)*4) - (p-1)*4,i_def)
    dof_stencil(i,2) = int((3 + (nupwindcells-1)*4) - (p-1)*4,i_def)
    dof_stencil(i,3) = int((4 + (nupwindcells-1)*4) - (p-1)*4,i_def)
    dof_stencil(i,4) = int((5 + (nupwindcells-1)*4) - (p-1)*4,i_def)
    i = i + 1
  end do
  ! Index of centre cell in stencil is always 1
  dof_stencil(i,:) = 1
  i = i + 1
  ! Index of first downwind cell is j
  ! where j = [5,4,3,2] for [W,S,E,N] directions
  ndownwindcells = int(real(np-1,r_def)/2.0_r_def)
  do p = 1,ndownwindcells
    dof_stencil(i,1) = int(4 + (p-1)*4,i_def)
    dof_stencil(i,2) = int(5 + (p-1)*4,i_def)
    dof_stencil(i,3) = int(2 + (p-1)*4,i_def)
    dof_stencil(i,4) = int(3 + (p-1)*4,i_def)
    i = i + 1
  end do
  
  ! Build Coefficient matrix of arbritray order
  allocate(coeff_matrix(np,np), inv_coeff_matrix(np,np))
  do i = 1,np
    do j = 1,np
      ! (i-1)**(j-1)
      inv_coeff_matrix(i,j) = real(i-1,r_def)**(j-1)
    end do
  end do
  call matrix_invert(inv_coeff_matrix,coeff_matrix,np)

  ! Find sampling point,
  ! first tracer point in stencil is at x = 0, 
  ! x0 then depends upon the number of upwind cells used
  x0 = floor(real(np,r_def)/2.0_r_def)

  ! Compute derivative of x0^p
  allocate( dx0(np-1) )
  do p = 1,np-1
    dx0(p) = real(p,r_def)*x0**(p-1)
  end do
  
  ! For vertical terms we may need to reduce orders near the boundaries
  ! due to lack of points so for each cell compute order and 
  ! number of points in stencil of vertical terms
  ! np_v(:,1) is used if w > 0
  ! np_v(:,2) is used if w < 0
  allocate( np_v(0:nlayers,2) )
  np_v(:,:) = np

  ! Reduce to linear/cubic on levels 1 & n-1
  np_v(1,1) = min(np,2_i_def)
  np_v(1,2) = min(np,4_i_def)
  np_v(nlayers-1,2) = min(np,2_i_def)
  np_v(nlayers-1,1) = min(np,4_i_def)

  ! Reduce to constant on first and last levels
  np_v(0,:) = 1_i_def
  np_v(nlayers,:) = 1_i_def

  do k = 2,nlayers-2
    np_v(k,:) = int(min(np,min(2*k+1,2*(nlayers-k)+1)),i_def)
  end do
  
  ! Compute inverses for vertical
  ! Each lower order matrix is just a subset of the high order matrix
  allocate(coeff_matrix_v(1:np,1:np,np) )
  do p = 1,np
    call matrix_invert(inv_coeff_matrix(1:p,1:p),coeff_matrix_v(1:p,1:p,p),p)
  end do

end subroutine sample_poly_adv_init

end module sample_poly_adv_kernel_mod
