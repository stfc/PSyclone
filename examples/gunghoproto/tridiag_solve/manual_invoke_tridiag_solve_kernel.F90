!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author S. Pickles STFC Daresbury Lab

#include "debug.h"
module manual_invoke_tridiag_solve_kernel
  use lfric
  use debug
  implicit none
contains
  subroutine invoke_tridiag_solve_kernel(T, B, X)
    ! Giving T intent(in) causes a compile error on 
    ! the pointer assignment to the dofmap.
    ! Giving X intent(out) causes a segfault at run-time.
    ! These comments apply to ifort (13.0.1). 
    type(field_type), intent(inout) :: T
    type(field_type), intent(inout) :: B
    type(field_type), intent(inout) :: X

    type(ColumnTopology), pointer :: topology
    type(FunctionSpace_type), pointer :: T_space
    integer :: column
    integer :: nlayers
    integer, pointer :: p0dofmap(:, :)

    ! The kernel is assuming that T, B, X all have the same
    ! topology, dofmap, and number of layers.
    ! Somehow, the generator must be able to figure this out.
    select type(T_space => T%function_space)
    type is (FunctionSpace_type)
       topology => T_space%topology
       p0dofmap => T_space%dof_map(CELLS, FE)
       nlayers = topology%layer_count()
    class default
       abort("Impossible error in invoke_tridiagonal_solve_kernel")
    end select

    do column = 1, topology%entity_counts(CELLS)
       ewrite(6,*) "calling tridiag_solve_code on column ",column,", nlayers=", nlayers
       call tridiag_solve_code(nlayers, p0dofmap(:, column), &
                                   T%data, B%data, X%data)
    end do

! perhaps the automatic generator will re-indent this? 
! I cannot be bothered.
contains

  subroutine tridiag_solve_code(layers, p0dofm, T, B, X)
    
    integer, intent(in)     :: layers
    integer, intent(in)     :: p0dofm(1)
    real(dp), intent(in)    :: T(3,*)        ! The matrix
!
!   T(1,*), T(2,*), T(3,*) are respectively
!     the lower sub-diagonal, diagonal, and upper sub-diagonal
!
    real(dp), intent(in)    :: B(*)          ! The RHS
    real(dp), intent(out)   :: X(*)          ! The solution
    
    integer :: k
    real(dp) :: m

    real(dp), dimension(:), allocatable :: cp

    allocate( cp(layers) )

!   cp(1) = u(1)/d(1)
    cp(1) = T(3,0+p0dofm(1))/T(2,0+p0dofm(1))

!   x(1) = b(1)/d(1)
    x(0+p0dofm(1)) = B(0+p0dofm(1))/T(2,0+p0dofm(1))

!   do i = 2,n
    do k = 1, layers-1

!     m = 1.0d0/(d(i)-cp(i-1)*l(i-1))
!     cp(i) = u(i)*m
!     x(i) = (b(i)-x(i-1)*l(i-1))*m

      m = 1.0d0/(T(2,k+p0dofm(1))-cp(k)*T(1,k-1+p0dofm(1)))
      cp(k+1) = T(3,k+p0dofm(1))*m
      X(k+p0dofm(1)) = (B(k+p0dofm(1))-X(k-1+p0dofm(1))*T(1,k-1+p0dofm(1)))*m

    end do

!   do i = n-1, 1, -1
!     x(i) = x(i)-cp(i)*x(i+1)
!   end do
    do k = layers-2, 0, -1
      X(k+p0dofm(1)) = X(k+p0dofm(1))-cp(k+1)*X(k+1+p0dofm(1))
    end do

    deallocate(cp)
  end subroutine tridiag_solve_code

  end subroutine invoke_tridiag_solve_kernel
end module manual_invoke_tridiag_solve_kernel
