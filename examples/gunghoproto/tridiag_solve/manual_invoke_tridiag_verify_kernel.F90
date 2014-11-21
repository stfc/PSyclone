!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author S. Pickles STFC Daresbury Lab

#include "debug.h"
module manual_invoke_tridiag_verify_kernel
  use lfric
  use debug
  implicit none
contains
  subroutine invoke_tridiag_verify_kernel(T, B, X, E)
    ! Giving T intent(in) causes a compile error on 
    ! the pointer assignment to p0dofmap 
    type(field_type), intent(inout) :: T, B, X
    type(field_type), intent(inout) :: E

    type(ColumnTopology), pointer :: topology
    type(FunctionSpace_type), pointer :: T_space
    integer :: column
    integer :: nlayers
    integer, pointer :: p0dofmap(:, :)

    select type(T_space => T%function_space)
    type is (FunctionSpace_type)
       topology => T_space%topology
       p0dofmap => T_space%dof_map(CELLS, FE)
       nlayers = topology%layer_count()
    class default
       abort("Can't happen")
    end select

    do column = 1, topology%entity_counts(CELLS)
       call tridiag_verify_code(nlayers, p0dofmap(:, column), &
                                   T%data, B%data, X%data, E%data)
    end do

! perhaps the automatic generator will re-indent this? 
! I cannot be bothered.
contains
  subroutine tridiag_verify_code(layers, p0dofm, T, B, X, E)

    integer, intent(in)     :: layers
    integer, intent(in)     :: p0dofm(1)
    real(dp), intent(in)    :: T(3,*)        ! The matrix
!
!   T(1,*), T(2,*), T(3,*) are respectively
!     the lower sub-diagonal, diagonal, and upper sub-diagonal
!
    real(dp), intent(in)    :: B(*)          ! The RHS
    real(dp), intent(in)    :: X(*)          ! The candidate solution

!   This follows the integrate_one example, but
!   I am not entirely comfortable with E being array valued.
!   We could do it differently.
    real(dp), intent(inout) :: E(*)          ! cumulative error

    integer :: k
    integer :: offset
    real(dp) :: w, err

!   To make numbering look more natural in fortran
    offset = p0dofm(1)-1

!   E(1) = E(1) + 1.0_dp

!   Calculate the error |TX-B| (i.e. in the 2-norm)
    
    ! (TX)(1) - B(1) 
    w = T(2,1+offset)*X(1+offset) + T(3,1+offset)*X(2+offset)
    err = (w - B(1+offset))**2.0d0
    do k=2,layers-1
      ! (TX)(k) - B(k) 
      w =   T(1,k-1+offset)*X(k-1+offset)     &
          + T(2,k  +offset)*X(k  +offset)     &
          + T(3,k  +offset)*X(k+1+offset)
      err = err + (w - B(k+offset))**2.0d0
    end do
    ! (TX)(layers) - B(layers)
    w =   T(1,layers-1+offset)*X(layers-1+offset) &
        + T(2,layers  +offset)*X(layers  +offset)
    err = err + (w - B(layers+offset))**2.0d0

    err = sqrt(err)

    E(1) = E(1) + err

  end subroutine tridiag_verify_code

  end subroutine invoke_tridiag_verify_kernel
end module manual_invoke_tridiag_verify_kernel
