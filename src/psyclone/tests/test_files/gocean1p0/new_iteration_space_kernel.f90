!> \brief Compute the mass flux in the x direction, cu
!! \detail Given the current pressure and velocity fields,
!! computes the mass flux in the x direction.
module new_iteration_space_kernel
  use kind_params_mod
  use kernel_mod
  use argument_mod
  use field_mod
  use grid_mod
  implicit none

  private

  public compute_kern1, compute_kern2, compute_kern3, &
         compute_kern1_code, compute_kern2_code, compute_kern3_code

  type, extends(kernel_type) :: compute_kern1
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CT, GO_POINTWISE)         & ! u
           /)
     integer :: ITERATES_OVER = internal_we_halo

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_kern1_code
  end type compute_kern1


  type, extends(kernel_type) :: compute_kern2
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),        & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),        & ! p
             go_arg(GO_READ,  GO_CT, GO_POINTWISE)         & ! u
           /)
     integer :: ITERATES_OVER = internal_ns_halo

     !> Although the staggering of variables used in an Arakawa
     !! C grid is well defined, the way in which they are indexed is
     !! an implementation choice. This can be thought of as choosing
     !! which grid-point types have the same (i,j) index as a T
     !! point. This kernel assumes that the U,V and F points that
     !! share the same index as a given T point are those immediately
     !! to the South and West of it.
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_kern2_code
  end type compute_kern2

  type, extends(kernel_type) :: compute_kern3
     type(go_arg), dimension(3) :: meta_args =    &
          (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),   & ! cu
             go_arg(GO_READ,  GO_CT, GO_POINTWISE),   & ! p
             go_arg(GO_READ,  GO_CT, GO_POINTWISE)    & ! u
           /)
     integer :: ITERATES_OVER = north_east_corner
     integer :: index_offset = GO_OFFSET_SW

  contains
    procedure, nopass :: code => compute_kern3_code
  end type compute_kern3

contains

  !===================================================

  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_kern1_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p, u

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_kern1_code

  !===================================================
  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_kern2_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p, u

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_kern2_code

  !===================================================
  !> Compute the mass flux in the x direction at point (i,j)
  subroutine compute_kern3_code(i, j, cu, p, u)
    implicit none
    integer,  intent(in) :: I, J
    real(go_wp), intent(out), dimension(:,:) :: cu
    real(go_wp), intent(in),  dimension(:,:) :: p, u

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine compute_kern3_code

end module new_iteration_space_kernel
