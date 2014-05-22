!> Module for physical/mathematical parameters
MODULE physical_params
  USE kind_params
  IMPLICIT none

  PUBLIC

  !> Pi. Is this sufficient or should we keep 
  !! computation using 4.0*ATAN(1.0)?
  REAL(wp), PARAMETER :: pi = 3.1415927410125732

  !> 2 x Pi
  REAL(wp), PARAMETER :: tpi = pi + pi

END MODULE physical_params
