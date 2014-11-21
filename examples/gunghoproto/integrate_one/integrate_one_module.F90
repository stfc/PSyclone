!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author D. Ham Imperial College

! Example integrating the unit function over the unit cube.

module integrate_one_module
  use kernel_mod
  implicit none

  private
  public integrate_one_kernel
  public integrate_one_code

  type, extends(kernel_type) :: integrate_one_kernel
     type(arg) :: meta_args(2) = (/&
          arg(READ, (CG(1)*CG(1))**3, FE), &
          arg(SUM, R, FE)/)
     integer :: ITERATES_OVER = CELLS
   contains
     procedure, nopass :: code => integrate_one_code
  end type integrate_one_kernel

contains

  subroutine integrate_one_code(layers, p1dofm, X, R)
    
    integer, intent(in) :: layers
    integer, intent(in) :: p1dofm(6)
    real(dp), intent(in) :: X(3,*)
    real(dp), intent(inout) :: R
    
    real(dp) :: dx1(2), dx2(2), area
    integer :: k

    dx1 = X(1:2, p1dofm(3))-X(1:2, p1dofm(1))
    dx2 = X(1:2, p1dofm(5))-X(1:2, p1dofm(1))
    
    area = 0.5*abs(dx1(2)*dx2(1)-dx1(1)*dx2(2))
    
    do k = 0, layers-1
       R = R + area*(X(3, p1dofm(2) + k)-X(3, p1dofm(1) + k))
    end do

  end subroutine integrate_one_code

end module integrate_one_module

