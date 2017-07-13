!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

MODULE time_smooth_mod
  USE kind_params_mod
  USE kernel_mod
  use argument_mod
  IMPLICIT none

  PRIVATE

  PUBLIC time_smooth_init, manual_invoke_time_smooth
  PUBLIC time_smooth_type, time_smooth_code

  !> Parameter for time smoothing
  REAL(wp) :: alpha

  !> The time smoothing operates in time rather than space
  !! and therefore takes three fields defined on any one
  !! of the four grid point types (T, U, V or Q).
  !! Presumably FE should be FD for us and maybe CELLS 
  !! should be COLUMNS?
  TYPE, EXTENDS(kernel_type) :: time_smooth_type
     TYPE(arg), DIMENSION(3) :: meta_args = &
          (/ arg(READ, EVERY, POINTWISE),     &
             arg(READ, EVERY, POINTWISE),     &
             arg(READWRITE , EVERY, POINTWISE)      &
           /)
     !> We only have one value per grid point and that means
     !! we have a single DOF per grid point.
     INTEGER :: ITERATES_OVER = DOFS
  CONTAINS
    procedure, nopass :: code => time_smooth_code
  END type time_smooth_type

CONTAINS

  !===================================================

  !> Initialise the time-smoothing module. Sets parameter
  !! alpha that is used in the time-smooth kernel.
  SUBROUTINE time_smooth_init(alpha_tmp)
    IMPLICIT none
    REAL(wp), INTENT(in) :: alpha_tmp

    alpha = alpha_tmp

  END SUBROUTINE time_smooth_init

  !===================================================

  !> Manual implementation of code to invoke the time-smoothing
  !! kernel
  SUBROUTINE manual_invoke_time_smooth(field, field_new, field_old)
    IMPLICIT none
    REAL(wp), INTENT(in), DIMENSION(:,:) :: field
    REAL(wp), INTENT(in), DIMENSION(:,:) :: field_new
    REAL(wp), INTENT(inout), DIMENSION(:,:) :: field_old
    ! Locals
    INTEGER :: i, j
    INTEGER :: idim1, idim2
    
    ! Here we will query what should be field objects to get at
    ! raw data.
    idim1 = SIZE(field, 1)
    idim2 = SIZE(field, 2)

    ! Loop over 'columns'
    DO J=1,idim2
      DO I=1,idim1
         CALL time_smooth_code(i,j,field,field_new,field_old)
      END DO
    END DO

  END SUBROUTINE manual_invoke_time_smooth

  !===================================================

  !> Kernel to smooth supplied field in time
  SUBROUTINE time_smooth_code(i, j, field, field_new, field_old)
    IMPLICIT none
    INTEGER,  INTENT(in)                    :: i, j
    REAL(wp), INTENT(in),    DIMENSION(:,:) :: field
    REAL(wp), INTENT(in),    DIMENSION(:,:) :: field_new
    REAL(wp), INTENT(inout), DIMENSION(:,:) :: field_old

    field_old(i,j) = field(i,j) + &
         alpha*(field_new(i,j) - 2.*field(i,j) + field_old(i,j))

  END SUBROUTINE time_smooth_code

END MODULE time_smooth_mod
