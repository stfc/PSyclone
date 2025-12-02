module compute_die_mod
  implicit none
  private

  public :: compute_die

  contains
  subroutine compute_die(die, current, neighbours)
    real(kind=8), allocatable, dimension(:,:), intent(in) :: current
    real(kind=8), allocatable, dimension(:,:), intent(in) :: neighbours
    real(kind=8), allocatable, dimension(:,:), intent(inout) :: die
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
        die(i,j) = 0.0
        if (current(i,j) > 0.0) then
          if (neighbours(i,j) < 2.0 .OR. neighbours(i,j) > 3.0) then
            die(i,j) = 1.0
          end if
        end if
      enddo
    enddo

  end subroutine compute_die

end module compute_die_mod
