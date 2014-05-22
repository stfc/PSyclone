!> \brief Apply boundary conditions for field on CU
!! \detail Applies cyclic boundary conditions for a 
!! field defined on CU.
module apply_bcs_cu
  use kind_params
  use kernel_mod
  use argument_mod
  implicit none

  private

  public manual_invoke_apply_bcs_cu
  public apply_bcs_cu_type, apply_bcs_cu_code

  type, extends(kernel_type) :: apply_bcs_cu_type
     type(arg), dimension(1) :: meta_args =    &
          (/ arg(READWRITE, CU, POINTWISE)     & ! field
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     integer :: ITERATES_OVER = DOFS
  contains
    procedure, nopass :: code => apply_bcs_cu_code
  end type apply_bcs_cu_type

contains

  !===================================================

  !> Manual implementation of the code needed to invoke
  !! apply_bcs_cu_code().
  subroutine manual_invoke_apply_bcs_cu(field)
    implicit none
    real(wp), intent(inout), dimension(:,:) :: field
    ! Locals
    integer :: n, mp1, np1

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating the first column and last row of a quantity on CU.
    ! This looks like (using x to indicate a location that is written first
    ! and y a location that is written second):
    !
    !  i=1   i=M
    ! _ y  y  y  y 
    ! /|x  o  o  o   j=N 
    ! | x  o  o  o
    ! \ x  o  o  o
    !  \x_ o  o  o   j=1
    !   |\______/ 

    !   vi-1j+1--fij+1---vij+1---fi+1j+1-vi+1j+1
    !   |        |       |       |        |    
    !   |        |       |       |        |    
    !   Ti-1j----uij-----Tij-----ui+1j --Ti+1j--
    !   |        |       |       |        |    
    !   |        |       |       |        |    
    !   vi-1j----fij-----vij-----fi+1j --vi+1j--
    !   |        |       |       |        |    
    !   |        |       |       |        |    
    !   Ti-1j-1--uij-1---Tij-1---ui+1j-1-Ti+1j-1
    !

    MP1 = SIZE(field, 1)
    NP1 = SIZE(field, 2)
    N = NP1 - 1

    call apply_bcs_cu_code(n, mp1, np1, field)

  end subroutine manual_invoke_apply_bcs_cu

  !===================================================

  !> Apply cyclic boundary conditions to field on CU
  subroutine apply_bcs_cu_code(n, mp1, np1, field)
    implicit none
    integer,  intent(in) :: n, mp1, np1
    real(wp), intent(inout), dimension(:,:) :: field

    ! First col = last col
    field(1,    1:N) = field(MP1,  1:N)
    ! Last row = first row
    field(1:MP1,NP1) = field(1:MP1,1)

  end subroutine apply_bcs_cu_code

end module apply_bcs_cu
