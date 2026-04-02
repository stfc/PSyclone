module compute_born_mod
  implicit none
  private

  public :: compute_born

  contains
  subroutine compute_born(born, current, neighbours)
    real(kind=8), allocatable, dimension(:,:), intent(inout) :: born
    real(kind=8), allocatable, dimension(:,:), intent(in) :: current
    real(kind=8), allocatable, dimension(:,:), intent(in) :: neighbours
    integer :: xstart
    integer :: xstop
    integer :: ystart
    integer :: ystop
    integer :: i
    integer :: j

    ! Note that the fields have an outer halo, so we only
    ! work on the inner region (ignoring the first and last
    ! row and column).
    xstart = LBOUND(current, dim=1) + 1
    xstop = UBOUND(current, dim=1) - 1
    ystart = LBOUND(current, dim=2) + 1
    ystop = UBOUND(current, dim=2) - 1

    do j = ystart, ystop, 1
      do i = xstart, xstop, 1
        born(i,j) = 0.0
        if (current(i,j) == 0.0 .AND. neighbours(i,j) == 3.0) then
          born(i,j) = 1.0
        end if
      enddo
    enddo

  end subroutine compute_born

end module compute_born_mod
