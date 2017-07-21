! Modifications copyright (c) 2017, Science and Technology Facilities Council
!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief Kernel which (incrementally) applies a columnwise assembled operator to a field

module columnwise_op_app_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                    &
                                    GH_FIELD, GH_COLUMNWISE_OPERATOR,       &
                                    GH_READ, GH_INC,                        &
                                    ANY_SPACE_1, ANY_SPACE_2,               &
                                    GH_COLUMN_INDIRECTION_DOFMAP,           &
                                    CELLS 

use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: columnwise_op_app_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                      &
       arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                        &  
       arg_type(GH_FIELD,    GH_READ, ANY_SPACE_2),                        &
       arg_type(GH_COLUMNWISE_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_2) &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: columnwise_op_app_kernel_code
end type columnwise_op_app_kernel_type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface columnwise_op_app_kernel_type
   module procedure columnwise_op_app_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public columnwise_op_app_kernel_code
contains
  
  type(columnwise_op_app_kernel_type) function columnwise_op_app_kernel_constructor() result(self)
    implicit none
    return
  end function columnwise_op_app_kernel_constructor

  !> @brief The subroutine which is called directly from the PSY layer and
  !> applies the operator as lhs += A.x
  !>
  !> @param [in] cell the horizontal cell index
  !> @param [in] ncell_2d number of cells in 2d grid
  !> @param [inout] lhs Resulting field lhs += A.x
  !> @param [in] x input field
  !> @param [in] columnwise_matrix banded matrix to assemble into
  !> @param [in] ndf1 number of degrees of freedom per cell for the to-space
  !> @param [in] undf1 unique number of degrees of freedom  for the to-space
  !> @param [in] map1 dofmap for the to-space
  !> @param [in] ndf2 number of degrees of freedom per cell for the from-space
  !> @param [in] undf2 unique number of degrees of freedom for the from-space 
  !> @param [in] map2 dofmap for the from-space
  !> @param [in] nrow number of rows in the banded matrix
  !> @param [in] ncol number of columns in the banded matrix
  !> @param [in] bandwidth bandwidth of the banded matrix
  !> @param [in] alpha banded matrix parameter \f$\alpha\f$
  !> @param [in] beta banded matrix parameter \f$\beta\f$
  !> @param [in] gamma_m banded matrix parameter \f$\gamma_-\f$
  !> @param [in] gamma_p banded matrix parameter \f$\gamma_+\f$
  !> @param [in] indirection_dofmap_to indirection map for to-space
  !> @param [in] indirection_dofmap_from indirection map for from-space
  subroutine columnwise_op_app_kernel_code(cell,              &
                                           ncell_2d,          &
                                           lhs, x,            & 
                                           columnwise_matrix, &
                                           ndf1, undf1, map1, &
                                           ndf2, undf2, map2, &
                                           nrow,              &
                                           ncol,              &
                                           bandwidth,         &
                                           alpha,             &
                                           beta,              &
                                           gamma_m,           &
                                           gamma_p,           &
                                           indirection_dofmap_to, &
                                           indirection_dofmap_from)
    implicit none
    ! Arguments
    integer(kind=i_def), intent(in) :: cell,  ncell_2d
    integer(kind=i_def), intent(in) :: nrow, ncol, bandwidth
    integer(kind=i_def), intent(in) :: undf1, ndf1
    integer(kind=i_def), intent(in) :: undf2, ndf2
    real(kind=r_def), dimension(undf1), intent(inout) :: lhs
    real(kind=r_def), dimension(undf2), intent(in) :: x
    real(kind=r_def), dimension(bandwidth,nrow,ncell_2d), intent(in) :: columnwise_matrix
    integer(kind=i_def), dimension(ndf1), intent(in) :: map1
    integer(kind=i_def), dimension(ndf2), intent(in) :: map2

    integer(kind=i_def), intent(in) :: alpha, beta, gamma_m, gamma_p
    integer(kind=i_def), dimension(nrow), intent(in) :: indirection_dofmap_to
    integer(kind=i_def), dimension(ncol), intent(in) :: indirection_dofmap_from

  end subroutine columnwise_op_app_kernel_code

end module columnwise_op_app_kernel_mod
