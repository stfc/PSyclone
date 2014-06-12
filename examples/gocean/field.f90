MODULE field
  USE kind_params
  IMPLICIT none

  ! A field must exist upon a grid.
  ! Does that mean a field extends a grid type?
  ! Or that a field carries with it a description of the grid
  ! on which it lives? If the latter then we have to query
  ! the field to see what type of grid it's on.

  TYPE :: grid_type
     !> Total number of grid points
     INTEGER :: npts

  END type grid_type

  TYPE, EXTENDS(grid_type) :: rquad_type
     !> Extent of grid in x
     INTEGER :: nx
     !> Extent of grid in y
     INTEGER :: ny
     !> Grid spacing in x
     REAL(wp) :: dx
     !> Grid spacing in y
     REAL(wp) :: dy

  END TYPE rquad_type

  TYPE, EXTENDS(rquad_type) :: cgrid_type

     REAL(wp), DIMENSION(:,:), ALLOCATABLE :: data

  END type cgrid_type

  TYPE, EXTENDS(cgrid_type) :: tpt_type

  END TYPE tpt_type

  TYPE :: field_type
     TYPE(grid_type) :: grid
  END type field_type

  TYPE, EXTENDS(field_type) :: scalar_field_type
     REAL(wp) :: data
  END TYPE scalar_field_type

  TYPE, EXTENDS(field_type) :: r2d_field_type
     REAL(wp), DIMENSION(:,:), ALLOCATABLE :: data
  END type r2d_field_type

  INTERFACE set
     MODULE PROCEDURE set_scalar_field
  END INTERFACE set

  INTERFACE copy_field
     MODULE PROCEDURE copy_scalar_field, copy_2dfield
  END INTERFACE copy_field

  INTERFACE increment
     MODULE PROCEDURE increment_scalar_field
  END INTERFACE increment

CONTAINS

  SUBROUTINE copy_scalar_field(field_in, field_out)
    IMPLICIT none
    TYPE(scalar_field_type), INTENT(in) :: field_in
    TYPE(scalar_field_type), INTENT(out) :: field_out

    field_out = field_in

  END SUBROUTINE copy_scalar_field

  !===================================================

  SUBROUTINE copy_2dfield(field_in, field_out)
    IMPLICIT none
    REAL(KIND=8), INTENT(in),  DIMENSION(:,:) :: field_in
    REAL(KIND=8), INTENT(out), DIMENSION(:,:) :: field_out
        
    field_out(:,:) = field_in(:,:)
        
  END SUBROUTINE copy_2dfield

  !===================================================

  SUBROUTINE increment_scalar_field(field, incr)
    IMPLICIT none
    TYPE(scalar_field_type), INTENT(inout) :: field
    TYPE(scalar_field_type), INTENT(in)    :: incr

    field%data = field%data + incr%data

  END SUBROUTINE increment_scalar_field

  !===================================================

  SUBROUTINE set_scalar_field(fld, val)
    IMPLICIT none
    TYPE(scalar_field_type), INTENT(out) :: fld
    REAL(wp), INTENT(in) :: val
    fld%data = val
  END SUBROUTINE set_scalar_field

  !===================================================

!  SUBROUTINE set_2dreal(fld, val)
!    IMPLICIT none
!    TYPE(reg_field_type), INTENT(out) :: fld
!    REAL(wp), INTENT(in) :: val
!
!    fld%data = val
END MODULE field
