module global_parameters_mod
  ! Module containing core parameter values
  use iso_c_binding
  implicit none
  
  private

  ! Kind type for double precision
  integer, parameter, public :: dp = c_double

  ! Default length for names (eg of fields and function spaces)
  integer, parameter, public :: NAME_LEN = 1024

  ! Iteration spaces for kernels.
  public :: CELLS, EDGES, VERTICES
  enum, bind(c)
     ! This is the 2D entity at the base of the columns over which the kernel
     !  is to iterate.
     enumerator :: CELLS=2, EDGES=1, VERTICES=0
  end enum
  
  ! Stencils for kernels
  public :: FE
  enum, bind(c)
     enumerator :: FE
  end enum
  
end module global_parameters_mod
