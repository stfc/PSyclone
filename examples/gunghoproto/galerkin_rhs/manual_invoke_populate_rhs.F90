!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author D. Ham Imperial College

#include "debug.h"
module manual_invoke_populate_rhs
  use lfric
  implicit none
contains
  subroutine invoke_populate_rhs(x, f)
    type(field_type), intent(inout) :: x, f
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
       class default
       abort("Can't happen")
    end select

    select type(f_space => f%function_space)
    type is (FunctionSpace_type)
       p0dofmap => f_space%dof_map(CELLS, FE)
       class default
       abort("Can't happen")
    end select

    do column = 1, topology%entity_counts(CELLS)
       call populate_rhs_code(nlayers, p1dofmap(:, column), x%data, &
            p0dofmap(:, column), f%data)
    end do
  contains
    subroutine populate_rhs_code(layers, p1dofm, X, p0dofm, f)
      ! Evaluate f = sin(z)

      integer, intent(in) :: layers
      integer, intent(in) :: p1dofm(6), p0dofm(1)
      real(dp), intent(in) :: X(3,*)
      real(dp), intent(out) :: f(*)

      real(dp) :: J(3,3), detJ
      integer :: k


      do k = 0, layers-1

         f(p0dofm(1)+k) = sin((X(3, p1dofm(2)+k) + X(3, p1dofm(1)+k))/2)

      end do

    end subroutine populate_rhs_code

  end subroutine invoke_populate_rhs
end module manual_invoke_populate_rhs
