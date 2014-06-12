!> Module to contain all mesh-specific data
MODULE mesh
  IMPLICIT none

  PRIVATE

  !> Extents of the grid
  INTEGER :: nx, ny

  !> Grid spacings in x and y
  REAL(KIND=8) :: dx, dy
  !> 4.0/dx and 4.0/dy
  REAL(KIND=8) :: fsdx, fsdy

  PUBLIC dx, dy, fsdx, fsdy
  PUBLIC set_grid_extents, set_grid_spacings

CONTAINS

  SUBROUTINE set_grid_extents(m, n)
    IMPLICIT none
    INTEGER, INTENT(in) :: m, n

    nx = m
    ny = n

  END SUBROUTINE set_grid_extents

  SUBROUTINE set_grid_spacings(x, y)
    IMPLICIT none
    REAL(KIND=8), INTENT(in) :: x, y

    dx = x
    dy = y

    fsdx = 4./DX
    fsdy = 4./DY

  END SUBROUTINE set_grid_spacings

END MODULE mesh
