module mixed_mod

  use argument_mod,      only : arg_type,                         &
                                GH_FIELD, GH_OPERATOR, GH_SCALAR, &
                                GH_REAL, GH_READ, GH_READWRITE,   &
                                CELL_COLUMN
  use fs_continuity_mod, only : W3, W0
  use constants_mod,     only : r_def, i_def
  use kernel_mod,        only : kernel_type

  implicit none

  type, extends(kernel_type) :: mixed_type
     type(arg_type), dimension(3) :: meta_args =               &
          (/ arg_type(gh_scalar,   gh_real, gh_read),          &
             arg_type(gh_field,    gh_real, gh_readwrite, w3), &
             arg_type(gh_operator, gh_real, gh_read, w0, w0)   &
          /)
     integer :: operates_on = cell_column
  end type mixed_type

  private
  public :: mixed_code

  interface mixed_code
     module procedure mixed_code_32
     module procedure mixed_code_64
  end interface mixed_code

contains

  subroutine mixed_code_32(cell, nlayers, rscalar, field_w3, op_ncell_3d, op,  &
                           ndf_w3, undf_w3, map_w3, ndf_w0)
    USE constants_mod, ONLY: i_def
    IMPLICIT NONE
    INTEGER(KIND=i_def), intent(in) :: nlayers
    INTEGER(KIND=i_def), intent(in) :: ndf_w3
    INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3
    INTEGER(KIND=i_def), intent(in) :: undf_w3, ndf_w0
    REAL*4,              intent(in) :: rscalar
    REAL*4,              intent(inout), dimension(undf_w3) :: field_w3
    INTEGER(KIND=i_def), intent(in) :: cell
    INTEGER(KIND=i_def), intent(in) :: op_ncell_3d
    REAL*4,              intent(in), dimension(ndf_w0,ndf_w0,op_ncell_3d) :: op
    print *, "32-bit example called"
  end subroutine mixed_code_32

  subroutine mixed_code_64(cell, nlayers, rscalar, field_w3, op_ncell_3d, op,  &
                           ndf_w3, undf_w3, map_w3, ndf_w0)
    USE constants_mod, ONLY: i_def
    IMPLICIT NONE
    INTEGER(KIND=i_def), intent(in) :: nlayers
    INTEGER(KIND=i_def), intent(in) :: ndf_w3
    INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3
    INTEGER(KIND=i_def), intent(in) :: undf_w3, ndf_w0
    REAL*8,              intent(in) :: rscalar
    REAL*8,              intent(inout), dimension(undf_w3) :: field_w3
    INTEGER(KIND=i_def), intent(in) :: cell
    INTEGER(KIND=i_def), intent(in) :: op_ncell_3d
    REAL*8,              intent(in), dimension(ndf_w0,ndf_w0,op_ncell_3d) :: op
    print *, "64-bit example called"
  end subroutine mixed_code_64

end module mixed_mod
