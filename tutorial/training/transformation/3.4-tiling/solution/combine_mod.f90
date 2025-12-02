module combine_mod
  implicit none
  private

  public :: combine

  contains
  subroutine combine(current, die, born)
    use field_mod, only : r2d_field
    real(kind=8), allocatable, dimension(:,:), intent(inout) :: current
    real(kind=8), allocatable, dimension(:,:), intent(in) :: die
    real(kind=8), allocatable, dimension(:,:), intent(in) :: born
    integer :: xstart
    integer :: xstop
    integer :: ystart
    integer :: ystop
    integer :: i
    integer :: j

    xstart = LBOUND(current, dim=1) + 1
    xstop = UBOUND(current, dim=1) - 1
    ystart = LBOUND(current, dim=2) + 1
    ystop = UBOUND(current, dim=2) - 1
    do j = ystart, ystop, 1
      do i = xstart, xstop, 1
        current(i,j) = current(i,j) - die(i,j) + born(i,j)
      enddo
    enddo

  end subroutine combine

end module combine_mod
