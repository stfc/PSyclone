!> \brief Apply boundary conditions for field on CV
!! \detail Applies cyclic boundary conditions for a 
!! field defined on CV.
module apply_bcs_cv
  use kind_params
  use kernel_mod
  use argument_mod
  implicit none

  private

  public manual_invoke_apply_bcs_cv
  public apply_bcs_cv_type, apply_bcs_cv_code

  type, extends(kernel_type) :: apply_bcs_cv_type
     type(arg), dimension(1) :: meta_args =    &
          (/ arg(READWRITE, CV, POINTWISE)     & ! field
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     integer :: ITERATES_OVER = DOFS
  contains
    procedure, nopass :: code => apply_bcs_cv_code
  end type apply_bcs_cv_type

contains

  !===================================================

  !> Manual implementation of the code needed to invoke
  !! apply_bcs_cv_code().
  subroutine manual_invoke_apply_bcs_cv(field)
    implicit none
    real(wp), intent(inout), dimension(:,:) :: field
    ! Locals
    integer :: m, mp1, np1

    ! Note that we do not loop over the full extent of the field.
    ! Fields are allocated with extents (M+1,N+1).
    ! Presumably the extra row and column are needed for periodic BCs.
    ! We are updating the first row and last col of a quantity on CV.
    ! This looks like (using x to indicate a location that is written first
    ! and y a location that is written second):
    !
    !  i=1   i=M
    !  -o  o  o  y 
    ! / o  o  o  y   j=N 
    ! | o  o  o  y
    ! \ o  o  o  y
    !  \x  x  x _y   j=1
    !    \______/|

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
    M = MP1 - 1

    call apply_bcs_cv_code(m, mp1, np1, field)

  end subroutine manual_invoke_apply_bcs_cv

  !===================================================

  !> Apply cyclic boundary conditions to field on CV
  subroutine apply_bcs_cv_code(m, mp1, np1, field)
    implicit none
    integer,  intent(in) :: m, mp1, np1
    real(wp), intent(inout), dimension(:,:) :: field

    ! First row = last row
    field(1:M,1    ) = field(1:M,NP1)
    ! Last col = first col
    field(MP1,1:NP1) = field(1,  1:NP1)

  end subroutine apply_bcs_cv_code

end module apply_bcs_cv
