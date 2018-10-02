module global_parameters_mod
  !> Module containing core parameter values
  use iso_c_binding
  implicit none
  
  private
  
  ! Default length for names (eg of fields and function spaces)
  integer, parameter, public :: NAME_LEN = 1024

  ! What boundary to align arrays on
  ! AVX is 256 bit = 4 d.p. words
  integer, parameter, public :: ALIGNMENT = 4

  ! Iteration spaces for kernels.
  public :: GO_CELLS, GO_EDGES, GO_VERTICES
  enum, bind(c)
     ! This is the 2D entity at the base of the columns over which the kernel
     !  is to iterate.
     enumerator :: GO_CELLS=2, GO_EDGES=1, GO_VERTICES=0
  end enum
  
  ! Stencils for kernels
  public :: GO_FE
  enum, bind(c)
     enumerator :: GO_FE
  end enum
  
end module global_parameters_mod
