!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes the fluxes for the split transport scheme

module conservative_flux_kernel_mod

use argument_mod,  only : arg_type, func_type,                  &
                          GH_FIELD, GH_WRITE, GH_READ,          &
                          W0, W2, W3, GH_BASIS, CELLS
use constants_mod, only : r_def
use kernel_mod,    only : kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: conservative_flux_kernel_type
  private
  type(arg_type) :: meta_args(7) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,   W2),                             &
       arg_type(GH_FIELD,   GH_READ,  W2),                             &
       arg_type(GH_FIELD,   GH_READ,  W2),                             &
       arg_type(GH_FIELD,   GH_READ,  W3, STENCIL(XORY1D)),            &
       arg_type(GH_FIELD,   GH_READ,  W3, STENCIL(XORY1D)),            &
       arg_type(GH_FIELD,   GH_READ,  W3, STENCIL(XORY1D)),            &
       arg_type(GH_FIELD,   GH_READ,  W3, STENCIL(XORY1D))             &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::conservative_flux_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface conservative_flux_kernel_type
   module procedure conservative_flux_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public conservative_flux_code
contains

type(conservative_flux_kernel_type) function conservative_flux_kernel_constructor() result(self)
  return
end function conservative_flux_kernel_constructor

!> @brief Computes the fluxes for the split advection scheme
!! @param[in] nlayers Integer the number of layers
!! @param[in] undf_w3 Integer the number of unique degrees of freedom
!! @param[in] ndf_w3 Integer the number of degrees of freedom per cell
!! @param[in] map_w3 Integer array holding the dofmap for the cell at the base of the column
!! @param[in] rho Real array, the density values in W3
!! @param[in] a0_coeffs Real array, the coefficients for the subgrid approximation of density
!! @param[in] a1_coeffs Real array, the coefficients for the subgrid approximation of density
!! @param[in] a2_coeffs Real array, the coefficients for the subgrid approximation of density
!! @param[in] undf_w2 Integer, the number of unique degrees of freedom
!! @param[in] ndf_w2 Integer, the number of degrees of freedom per cell
!! @param[in] map_w2 Integer array holding the dofmap for the cell at the base of the column
!! @param[inout] flux Real array, the flux values which are calculated
!! @param[in] dep_pts Real array, the departure points
!! @param[in] u_piola Real array, the Piola winds
!! @param[in] stencil_length Integer The length of the 1D stencil
!! @param[in] stencil_map Integer array holding the dofmaps for the stencil
!! @param[in] direction Integer the direction in which to calculate the fluxes

subroutine conservative_flux_code( nlayers,              &
                                   flux,                 &
                                   dep_pts,              &
                                   u_piola,              &
                                   rho,                  &
                                   rho_stencil_length,   &
                                   rho_direction,        &
                                   rho_stencil_map,      &
                                   a0_coeffs,            &
                                   a0_stencil_length,    &
                                   a0_direction,         &
                                   a0_stencil_map,       &
                                   a1_coeffs,            &
                                   a1_stencil_length,    &
                                   a1_direction,         &
                                   a1_stencil_map,       &
                                   a2_coeffs,            &
                                   a2_stencil_length,    &
                                   a2_direction,         &
                                   a2_stencil_map,       &
                                   ndf_w2,               &
                                   undf_w2,              &
                                   map_w2,               &
                                   ndf_w3,               &
                                   undf_w3,              &
                                   map_w3)

  use cosmic_flux_mod,    only : calc_stencil_ordering,                &
                                 frac_and_int_part,                    &
                                 calc_integration_limits,              &
                                 populate_array,                       &
                                 map_cell_index,                       &
                                 return_part_mass
  use flux_direction_mod, only : x_direction, y_direction

  use timestepping_config_mod,      only: dt

  !Arguments
  integer, intent(in)                                   :: nlayers
  integer, intent(in)                                   :: ndf_w3
  integer, intent(in)                                   :: undf_w3
  integer, dimension(ndf_w3), intent(in)                :: map_w3
  real(kind=r_def), dimension(undf_w3), intent(in)      :: rho
  real(kind=r_def), dimension(undf_w3), intent(in)      :: a0_coeffs
  real(kind=r_def), dimension(undf_w3), intent(in)      :: a1_coeffs
  real(kind=r_def), dimension(undf_w3), intent(in)      :: a2_coeffs
  integer, intent(in)                                   :: ndf_w2
  integer, intent(in)                                   :: undf_w2
  integer, dimension(ndf_w2), intent(in)                :: map_w2
  real(kind=r_def), dimension(undf_w2), intent(inout)   :: flux
  real(kind=r_def), dimension(undf_w2), intent(in)      :: dep_pts
  real(kind=r_def), dimension(undf_w2), intent(in)      :: u_piola
  integer, intent(in)                                   :: rho_stencil_length, a0_stencil_length
  integer, intent(in)                                   :: a1_stencil_length, a2_stencil_length
  integer, intent(in)                                   :: rho_direction, a0_direction
  integer, intent(in)                                   :: a1_direction, a2_direction
  integer, intent(in)                                   :: rho_stencil_map(1:rho_stencil_length)
  integer, intent(in)                                   :: a0_stencil_map(1:a0_stencil_length)
  integer, intent(in)                                   :: a1_stencil_map(1:a1_stencil_length)
  integer, intent(in)                                   :: a2_stencil_map(1:a2_stencil_length)

  !Internal variables
  real(kind=r_def) :: mass_total
  real(kind=r_def) :: departure_dist
  real(kind=r_def) :: rho_local(1:rho_stencil_length)
  real(kind=r_def) :: a0_local(1:a0_stencil_length)
  real(kind=r_def) :: a1_local(1:a1_stencil_length)
  real(kind=r_def) :: a2_local(1:a2_stencil_length)
  real(kind=r_def) :: fractional_distance
  real(kind=r_def) :: mass_frac
  real(kind=r_def) :: mass_from_whole_cells
  real(kind=r_def) :: left_integration_limit
  real(kind=r_def) :: right_integration_limit
  real(kind=r_def) :: subgrid_coeffs(3)

  integer, allocatable :: index_array(:)
  integer, allocatable :: local_density_index(:)

  integer :: stencil_ordering(1:rho_stencil_length)
  integer :: k
  integer :: df1
  integer :: ii
  integer :: edge_option
  integer :: n_cells_to_sum


  call calc_stencil_ordering(rho_stencil_length,stencil_ordering)

  if (direction .EQ. x_direction ) then
    edge_option = 0
    df1=1
  elseif (direction .EQ. y_direction) then
    edge_option = 0
    df1=2
  endif


  do k=0,nlayers-1

    departure_dist = dep_pts( map_w2(df1) + k )

    ! Rearrange data such that it is in the order 1 | 2 | 3 | 4 | 5 | 6 | 7 etc

    do ii=1,rho_stencil_length
      rho_local(ii) = rho( rho_stencil_map(stencil_ordering(ii)) )
      a0_local(ii)  = a0_coeffs( rho_stencil_map(stencil_ordering(ii)) )
      a1_local(ii)  = a1_coeffs( rho_stencil_map(stencil_ordering(ii)) )
      a2_local(ii)  = a2_coeffs( rho_stencil_map(stencil_ordering(ii)) )
    end do

    ! Calculates number of cells of interest and fraction of a cell to add.
    call frac_and_int_part(departure_dist,n_cells_to_sum,fractional_distance)

    ! Calcuates the left and right integration limits for the fractional cell.
    call calc_integration_limits( departure_dist,             &
                                  fractional_distance,        &
                                  left_integration_limit,     &
                                  right_integration_limit )

    allocate(index_array(n_cells_to_sum))
    allocate(local_density_index(n_cells_to_sum))

    call populate_array(n_cells_to_sum,index_array,departure_dist,edge_option)

    do ii=1,n_cells_to_sum
      local_density_index(ii) = map_cell_index(index_array(ii),rho_stencil_length)
    end do

    mass_from_whole_cells = sum(rho_local(local_density_index(1:n_cells_to_sum-1)))

    subgrid_coeffs = (/ a0_local(local_density_index(n_cells_to_sum)), &
                        a1_local(local_density_index(n_cells_to_sum)), &
                        a2_local(local_density_index(n_cells_to_sum)) /)

    mass_frac = return_part_mass(3,subgrid_coeffs,left_integration_limit,right_integration_limit)

    mass_total = mass_from_whole_cells + mass_frac

    flux( map_w2(df1) + k ) = sign(1.0_r_def,u_piola( map_w2(df1) + k ))*mass_total/dt

    if (allocated(index_array)) deallocate(index_array)
    if (allocated(local_density_index)) deallocate(local_density_index)

  end do

end subroutine conservative_flux_code

end module conservative_flux_kernel_mod
