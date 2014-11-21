!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author D. Ham Imperial College

module manual_invoke_galerkin_rhs_kernel
  use lfric
  implicit none
contains
  subroutine invoke_galerkin_rhs_kernel(x, rhs, f)
    type(field_type), intent(inout) :: x, rhs, f
    type(ColumnTopology), pointer :: topology
    type(FunctionSpace_type), pointer :: x_space
    type(FunctionSpace_type), pointer :: f_space
    integer :: column
    integer :: nlayers
    integer, pointer :: p1dofmap(:, :)
    integer, pointer :: p0dofmap(:, :)

    select type(x_space => x%function_space)
    type is (FunctionSpace_type)
       topology => x_space%topology
       p1dofmap => x_space%dof_map(CELLS, FE)
       nlayers = topology%layer_count()
       !class default
       !abort("Can't happen")
    end select

    select type(f_space => f%function_space)
    type is (FunctionSpace_type)
       p0dofmap => f_space%dof_map(CELLS, FE)
       !class default
       !abort("Can't happen")
    end select

    ! This should be done better
    rhs%data = 0
    do column = 1, topology%entity_counts(CELLS)
       call galerkin_rhs_code(nlayers, p1dofmap(:, column), x%data, rhs%data, &
            p0dofmap(:, column), f%data)
    end do
  contains
    subroutine galerkin_rhs_code(layers, p1dofm, X, rhs, p0dofm, f)
      ! Evaluate rhs = v * f * dx

      integer, intent(in) :: layers
      integer, intent(in) :: p1dofm(6), p0dofm(1)
      real(dp), intent(in) :: X(3,*), f(*)
      real(dp), intent(inout) :: rhs(*)

      real(dp) :: J(3,3), detJ
      integer :: k

      real(dp), parameter :: gradPhi(6,3) = reshape( &
           [-.5, -.5, .5, .5, 0., 0., &
           -.5, -.5, 0., 0., .5, .5, &
           -1./3, 1./3, -1./3, 1./3, -1./3, 1./3], [6,3])


      do k = 0, layers-1
         J=0

         J = matmul(X(:,p1dofm+k),gradPhi)
         detJ = abs((J(2,2)*J(3,3) - J(2,3)*J(3,2))*J(1,1) &
              - (J(1,2)*J(3,3) - J(1,3)*J(3,2))*J(2,1) &
              + (J(1,2)*J(2,3) - J(1,3)*J(2,2))*J(3,1))

         rhs(p1dofm+k) = rhs(p1dofm+k) + f(p0dofm(1)+k)*detJ/12.

      end do

    end subroutine galerkin_rhs_code

  end subroutine invoke_galerkin_rhs_kernel
end module manual_invoke_galerkin_rhs_kernel
