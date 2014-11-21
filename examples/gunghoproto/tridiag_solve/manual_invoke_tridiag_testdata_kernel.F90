!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author S. Pickles STFC Daresbury Lab

#include "debug.h"
module manual_invoke_tridiag_testdata_kernel
  use lfric
  use debug
  implicit none
contains
  subroutine invoke_tridiag_testdata_kernel(X, T, B)
    type(field_type), intent(inout) :: X
    type(field_type), intent(inout) :: T
    type(field_type), intent(inout) :: B

    type(ColumnTopology), pointer :: topology, xtopology, ttopology, btopology 
    type(FunctionSpace_type), pointer :: X_space, T_space, B_space
    integer :: column
    integer :: nlayers, xnlayers, tnlayers, bnlayers 
    integer, pointer :: xdofmap(:, :), tdofmap(:,:), bdofmap(:,:)

    ewrite(4,*) "invoke_tridiag_testdata_kernel entered"

    select type(X_space => X%function_space)
    type is (FunctionSpace_type)
       xtopology => X_space%topology
       xdofmap => X_space%dof_map(CELLS, FE)
       xnlayers = xtopology%layer_count()
    class default
       abort("Impossible error in invoke_tridiagonal_testdata_kernel")
    end select

    ewrite(5,*) "X%data has size ", size(X%data)
    ewrite(5,*) "X%data has shape ", shape(X%data)
    ewrite(5,*) "X%data has bounds (", &
             lbound(X%data,dim=1), ":", ubound(X%data,dim=1), &
             ")"
    ewrite(5,*) "xdofmap has shape ", shape(xdofmap)
    ewrite(5,*) "X has ", xnlayers, " layers"

    select type(T_space => T%function_space)
    type is (FunctionSpace_type)
       ttopology => T_space%topology
       tdofmap => T_space%dof_map(CELLS, FE)
       tnlayers = ttopology%layer_count()
    class default
       abort("Impossible error in invoke_tridiagonal_testdata_kernel")
    end select

    ewrite(5,*) "T%data has size ", size(T%data)
    ewrite(5,*) "T%data has shape ", shape(T%data)
    ewrite(5,*) "T%data has bounds (", &
             lbound(T%data,dim=1), ":", ubound(T%data,dim=1), &
             ")"
    ewrite(5,*) "tdofmap has shape ", shape(tdofmap)
    ewrite(5,*) "T has ", tnlayers, " layers"

    select type(B_space => B%function_space)
    type is (FunctionSpace_type)
       btopology => B_space%topology
       bdofmap => B_space%dof_map(CELLS, FE)
       bnlayers = btopology%layer_count()
    class default
       abort("Impossible error in invoke_tridiagonal_testdata_kernel")
    end select

    ewrite(5,*) "B%data has size ", size(B%data)
    ewrite(5,*) "B%data has shape ", shape(B%data)
    ewrite(5,*) "B%data has bounds (", &
             lbound(b%data,dim=1), ":", ubound(b%data,dim=1), &
             ")"
    ewrite(5,*) "bdofmap has shape ", shape(bdofmap)
    ewrite(5,*) "B has ", bnlayers, " layers"

    nlayers  = tnlayers

    do column = 1, ttopology%entity_counts(CELLS)
       ewrite(6,*) "calling tridiag_testdata_code on column ",column, &
               ", nlayers=", nlayers
       ewrite(7,*) "xdofmap(:)=", xdofmap(:,column)
       ewrite(7,*) "tdofmap(:)=", tdofmap(:,column)
       ewrite(7,*) "bdofmap(:)=", bdofmap(:,column)
       call tridiag_testdata_code(nlayers, &
                                  xdofmap(:, column), X%data, &
                                  tdofmap(:, column), T%data, &
                                  bdofmap(:, column), B%data)
    end do

! perhaps the automatic generator will re-indent this? 
! I cannot be bothered.
contains
  subroutine tridiag_testdata_code(layers, &
                                   cdofm, C, &
                                   tdofm, T, &
                                   bdofm, B)

    integer, intent(in)     :: layers
    integer, intent(in)     :: cdofm(*), tdofm(*), bdofm(*)
    real(dp), intent(in)    :: C(3,*)        ! The coordinate field
!
!   The coordinate field is passed in so that we could generate
!   location-dependent test data. We do not currently do this.
!
    real(dp), intent(out)   :: T(3,*)        ! The matrix
!
!   T(1,*), T(2,*), T(3,*) are respectively
!     the lower sub-diagonal, diagonal, and upper sub-diagonal
!
    real(dp), intent(out)   :: B(*)          ! The RHS

    integer :: k

    do k = 0, layers-2
      T(1,k+tdofm(1)) = -1.0_dp
      T(2,k+tdofm(1)) =  4.0_dp
      T(3,k+tdofm(1)) = -1.0_dp
    end do
    T(1,layers-1+tdofm(1)) =  0.0_dp
    T(2,layers-1+tdofm(1)) =  4.0_dp
    T(3,layers-1+tdofm(1)) =  0.0_dp

    do k = 0, layers-1
      B(k+bdofm(1)) = &
        (real(layers,kind=dp)+real(k,kind=dp))/real(layers,kind=dp)
    end do

  end subroutine tridiag_testdata_code
  end subroutine invoke_tridiag_testdata_kernel
end module manual_invoke_tridiag_testdata_kernel
